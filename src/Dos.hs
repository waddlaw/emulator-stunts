{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dos where

import Control.Applicative
import Control.Concurrent
import Control.Lens as Lens
import Control.Monad.State
import Data.Bits hiding (bit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Int
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Data.String
import qualified Data.Vector.Storable as V
import Data.Word
import Helper
import MachineState
import Sound.ALUT (pitch, play, sourceGain, stop, ($=))
import System.Directory
import System.FilePath (takeFileName)
import System.FilePath.Glob
import Prelude

exePath :: IsString p => p
exePath = "restunts/stunts"

---------------------------------------------- memory allocation

allocateMem :: Word16 -> MemPiece -> (Word16, MemPiece)
allocateMem req' (alloc, end) = (r + 1, (alloc ++ [(r, r + 1 + req')], end))
  where
    r = snd $ last alloc

modifyAllocated :: Word16 -> Word16 -> MemPiece -> Either Word16 MemPiece
modifyAllocated addr req (alloc, endf) = head $ concatMap f $ getOut $ zip alloc $ tail $ map fst alloc ++ [endf]
  where
    getOut xs = zip (inits xs) (tails xs)
    f (ys, ((beg, _), max') : xs)
      | beg == addr - 1 =
        [ if req > max' - beg - 1
            then Left $ max' - beg - 1
            else Right (map fst ys ++ (beg, beg + req + 1) : map fst xs, endf)
        ]
    f _ = []

--------------------------------------

everyNth :: Int -> [a] -> [[a]]
everyNth _ [] = []
everyNth n xs = take n xs : everyNth n (drop n xs)

(@:) :: BS.ByteString -> a -> a
_ @: x = x

infix 5 @:

combine :: Iso' (Word8, Word8) Word16
combine = iso (\(hi, lo) -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo) (\d -> (fromIntegral $ d `shiftR` 8, fromIntegral d))

paragraph :: Word16 -> Int
paragraph = (`shiftL` 4) . fromIntegral

hiret :: Machine ()
hiret = do
  pop >>= (ip ..=)
  pop >>= (cs ..=)
  pop >>= (flags ..=)

iret :: Machine ()
iret = do
  pop >>= (ip ..=)
  pop >>= (cs ..=)
  flags' <- pop
  interruptF ..= testBit flags' 9

haltWith :: [Char] -> a
haltWith = error

halt :: a
halt = error "CleanHalt"

getSender :: IO (Request -> IO ())
getSender = do
  v <- use'' interruptRequest
  return $ \r -> modifyMVar_ v $ return . (++ [r])

setCounter :: IO ()
setCounter = timerOn ...= True

timerThread :: IO b
timerThread = do
  v <- use'' instPerSec
  threadDelay $ round $ 1000000 / v
  o <- use'' timerOn
  when o $ do
    counter ..%= (+ 1)
    c <- use'' counter
    send <- getSender
    send $ AskTimerInterrupt c
  timerThread

--------------------------------------------------------------------------------

input :: Word16 -> Machine Word16
input = \case
  0x21 -> do
    x <- use'' intMask
    trace_ $ "Get interrupt mask " ++ showHex' 2 x
    return $ "???" @: fromIntegral x
  0x60 -> do
    k <- use'' keyDown
    trace_ $ "Keyboard scan code " ++ showHex' 4 k
    return $ "???" @: k
  0x61 -> do
    x <- use'' speaker
    when ((x .&. 0xfc) /= 0x30) $ trace_ $ "speaker -> " ++ showHex' 2 x
    return $ "???" @: fromIntegral x
  0x03da -> do
    -- TODO
    r <- head <$> use'' retrace
    retrace ..%= tail
    trace_ $ "VGA hardware " ++ showHex' 4 r
    return $ "Vretrace | DD" @: r
  v -> haltWith $ "input #" ++ showHex' 4 v

output' :: Word16 -> Word16 -> Machine ()
output' v x = case v of
  0x20 -> do
    case x of
      0x20 -> setCounter
      _ -> undefined
  0x21 -> do
    trace_ $ "Set interrupt mask " ++ showHex' 2 x -- ?
    intMask ...= fromIntegral x
    unless (testBit x 0) setCounter
  0x40 -> do
    trace_ $ "Set timer frequency " ++ showHex' 2 x
  0x41 -> do
    trace_ $ "ch #41 " ++ showHex' 2 x -- ?
  0x42 -> do
    frequency ..%= (.|. (x `shiftL` 8)) . (`shiftR` 8)
    f <- use'' frequency
    source <- use'' soundSource
    when (fromIntegral f >= (256 :: Int)) $ pitch source $= 2711 / fromIntegral f
  0x43 -> do
    -- Set timer control
    case x of
      0x36 -> trace_ "Set timer frequency, square wave"
      0xb6 -> trace_ "Set speaker frequency, square wave"
      _ -> undefined
  0x61 -> do
    x' <- use'' speaker
    speaker ...= fromIntegral x
    when (x .&. 0xfc /= 0x30) $ trace_ $ "speaker <- " ++ showHex' 2 x
    source <- use'' soundSource
    when (testBit x 0 /= testBit x' 0) $ sourceGain source $= if testBit x 0 then 0.1 else 0
    when (testBit x 1 /= testBit x' 1) $ (if testBit x 1 then play else stop) [source]
  _ -> haltWith $ "output #" ++ showHex' 4 v ++ " 0x" ++ showHex' 4 x

--------------------------------------------------------

imMax :: IM.IntMap b -> IM.Key
imMax m
  | IM.null m = 0
  | otherwise = succ . fst . IM.findMax $ m

origInterrupt :: M.Map (Word16, Word16) (Word8, Machine ())
origInterrupt =
  M.fromList
    [ hitem 0x00 (0xf000, 0x1060) $ do
        trace_ "Divison by zero interrupt"
        haltWith "int 00",
      hitem 0x08 (0xf000, 0xfea5) $ do
        output' 0x20 0x20,
      hitem 0x09 (0xf000, 0xe987) $ do
        trace_ "Orig keyboard interrupt"
        haltWith "int 09",
      item 0x10 (0xf000, 0x1320) $ do
        -- Video Services
        v <- use' ah
        case v of
          0x00 -> do
            video_mode_number <- use' al
            trace_ $ "Set Video Mode #" ++ showHex' 2 video_mode_number
            case video_mode_number of
              0x00 -> do
                trace_' "text mode"
              0x03 -> do
                trace_' "mode 3"
              0x13 -> do
                bx ..= 0 -- 4  -- ???
              _ -> haltWith $ "#" ++ showHex' 2 video_mode_number
          0x0b -> do
            trace_ "Select Graphics Palette or Text Border Color"
          0x0e -> do
            a <- use' al
            --            putChar $ chr . fromIntegral $ a
            trace_ $ "Write Character as TTY: " ++ show (chr $ fromIntegral a)
          0x0f -> do
            trace_ "Get Current Video Mode"
            al ..= "text mode" @: 3
            ah ..= "width of screen, in character columns" @: 80
            bh ..= "current active video page (0-based)" @: 0x00 --b8
          0x10 -> do
            -- Set/Get Palette Registers (EGA/VGA)
            f <- use' al
            case f of
              0x12 -> do
                trace_ "Set block of DAC color registers"
                first_DAC_register <- use' bx -- (0-00ffH)
                number_of_registers <- use' cx -- (0-00ffH)
                -- Es:DX addr of a table of R,G,B values (it will be CX*3 bytes long)
                addr <- dxAddr'
                colors <- getBytesAt addr $ 3 * fromIntegral number_of_registers
                palette ..%= \cs' ->
                  cs'
                    V.// zip
                      [fromIntegral first_DAC_register .. fromIntegral (first_DAC_register + number_of_registers - 1)]
                      -- shift 2 more positions because there are 64 intesity levels
                      [ fromIntegral r `shiftL` 26 .|. fromIntegral g `shiftL` 18 .|. fromIntegral b `shiftL` 10
                        | [r, g, b] <- everyNth 3 colors
                      ]
              _ -> haltWith $ "interrupt #10,#10,#" ++ showHex' 2 f
          v' -> haltWith $ "interrupt #10,#" ++ showHex' 2 v',
      item 0x15 (0xf000, 0x11e0) $ do
        -- Misc System Services
        use' ah >>= \case
          0xc2 -> do
            -- Pointing device BIOS interface
            w <- use' al
            case w of
              0x01 -> do
                trace_ "Reset Pointing device"
                ah ..= 0 -- ?
                bl ..= 0xaa -- ?
                returnOK
              _ -> undefined
          v' -> haltWith $ "interrupt #15,#" ++ showHex' 2 v',
      item 0x16 (0xf000, 0x1200) $ do
        -- Keyboard Services
        v <- use' ah
        case v of
          0x00 -> do
            trace_ "Read (Wait for) Next Keystroke"
            ah ..= "Esc scan code" @: 0x39
            al ..= "Esc ASCII code" @: 0x1b
          0x01 -> do
            trace_ "Query Keyboard Status / Preview Key"
            zeroF ..= False -- no keys in buffer
          v' -> haltWith $ "interrupt #16,#" ++ showHex' 2 v',
      item 0x20 (0x0000, 0x0000) $ do
        trace_ "interrupt halt"
        halt,
      item 0x21 (0xf000, 0x14c0) $ do
        -- DOS rutine
        v <- use' ah
        case v of
          0x00 -> do
            trace_ "dos Program terminate"
            halt
          0x1a -> do
            trace_ "Set DTA" -- Disk Transfer Address
            addr <- dxAddr
            dta ...= addr
          0x25 -> do
            v' <- fromIntegral <$> use' al -- interrupt vector number
            trace_ $ "Set Interrupt Vector " ++ showHex' 2 v'
            use' dx >>= setWordAt System (4 * v') -- DS:DX = pointer to interrupt handler
            use' ds >>= setWordAt System (4 * v' + 2)
          0x30 -> do
            trace_ "Get DOS version"
            al ..= "major version number" @: 0x05 --  (2-5)
            ah ..= "minor version number" @: 0x00 --  (in hundredths decimal)
            bh ..= "MS-DOS" @: 0xff
            do
              -- 24 bit OEM serial number
              bl ..= "OEM serial number (high bits)" @: 0
              cx ..= "OEM serial number (low bits)" @: 0
          0x35 -> do
            v' <- fromIntegral <$> use' al
            trace_ $ "Get Interrupt Vector " ++ showHex' 2 v'
            getWordAt System (4 * v') >>= (bx ..=)
            getWordAt System (4 * v' + 2) >>= (es ..=) -- Es:BX = pointer to interrupt handler
          0x3c -> do
            trace_ "Create"
            (_, fn) <- getFileName
            _ <- use' cx
            doesFileExist fn >>= \case
              True -> dosFail 0x05 -- access denied
              False -> do
                writeFile fn ""
                newHandle fn
          0x3d -> do
            trace_ "Open"
            use' al >>= \case
              0 -> do
                -- read mode
                (_, fn) <- getFileName
                checkExists fn $ newHandle fn
              _ -> undefined
          0x3e -> do
            handle <- fromIntegral <$> use' bx
            trace_ $ "Close " ++ showHandle handle
            x <- IM.lookup handle <$> use'' files
            case x of
              Just (_, _) -> do
                files ..%= IM.delete handle
                returnOK
              _ -> undefined
          0x3f -> do
            handle <- fromIntegral <$> use' bx
            (fn, seek) <- (IM.! handle) <$> use'' files
            num <- fromIntegral <$> use' cx
            loc <- dxAddr
            s <- BS.take num . BS.drop seek <$> BS.readFile fn
            let len = BS.length s
            trace_ $ "Read " ++ showHandle handle ++ " " ++ showBytes len
            files ..%= IM.adjust (\(fn', p) -> (fn', p + len)) handle
            setBytesAt loc $ BS.unpack s
            ax ..= "length" @: fromIntegral len
            returnOK
          0x40 -> do
            handle <- fromIntegral <$> use' bx
            num <- fromIntegral <$> use' cx
            trace_ $ "Write " ++ showHandle (handle :: Int) ++ " " ++ showBytes num
            loc <- dxAddr
            this <- getBytesAt loc num
            case handle of
              1 -> trace_' . ("STDOUT: " ++) . map (chr . fromIntegral) $ this
              2 -> trace_' . ("STDERR: " ++) . map (chr . fromIntegral) $ this
              _ -> return ()
            returnOK
          0x41 -> do
            trace_ "Delete"
            (_, fn) <- getFileName
            checkExists fn $ do
              removeFile fn
              returnOK
          0x42 -> do
            handle <- fromIntegral <$> use' bx
            fn <- (^. _1) . (IM.! handle) <$> use'' files
            mode <- use' al
            pos <- fromIntegral . (fromIntegral :: Word32 -> Int32) <$> use' cxdx
            let showMode = \case
                  0 -> ""
                  1 -> "+"
                  2 -> "-"
                  _ -> undefined
            trace_ $ "Seek " ++ showHandle handle ++ " " ++ showMode mode ++ show pos
            s <- BS.readFile fn
            files
              ..%= IM.adjust
                ( \(fn', p) -> case mode of
                    0 -> (fn', pos)
                    1 -> (fn', p + pos)
                    2 -> (fn', BS.length s + pos)
                    _ -> undefined
                )
                handle
            pos' <- (^. _2) . (IM.! handle) <$> use'' files
            dxax ..= fromIntegral pos'
            returnOK
          0x44 -> do
            -- I/O Control for Devices (IOCTL)
            0x44 <- use' ah
            function_value <- use' al
            case function_value of
              0x00 -> do
                handle <- use' bx
                trace_ $ "Get Device Information of " ++ showHandle handle
                let v' = case handle of
                      4 -> 0x80A0 --  0010 1000 00 000100   no D: drive
                      3 -> 0x80D3 --  0010 1000 00 000011   no C: drive
                      2 -> 0x80D3 --  0010 1000 00 000011    B: drive
                      1 -> 0x80D3 --  0010 1000 00 000011    A: drive
                      0 -> 0x80D3 --  0010 1000 00 000011    default drive
                      _ -> undefined
                dx ..= v'
                ax ..= v'
              _ -> undefined
            returnOK
          0x48 -> do
            memory_paragraphs_requested <- use' bx
            trace_ $ "Allocate Memory " ++ showBlocks memory_paragraphs_requested
            h <- use'' heap
            let (x, h') = allocateMem memory_paragraphs_requested h
            heap ...= h'
            ax ..= "segment address of allocated memory block" @: x -- (MCB + 1para)
            returnOK
          0x4a -> do
            new_requested_block_size_in_paragraphs <- use' bx
            trace_ $ "Modify allocated memory to " ++ showBlocks new_requested_block_size_in_paragraphs
            segment_of_the_block <- use' es -- (MCB + 1para)
            h <- use'' heap
            case modifyAllocated segment_of_the_block new_requested_block_size_in_paragraphs h of
              Left x -> do
                bx ..= "maximum block size possible" @: x
                trace_' $ "max possible: " ++ showBlocks x
                dosFail 0x08 -- insufficient memory
              Right h' -> do
                ds' <- use' ds
                ax ..= ds' -- why???
                heap ...= h'
                returnOK
          0x4c -> do
            code <- use' al
            trace_ $ "Terminate Process With Return Code #" ++ showHex' 2 code
            halt
          0x4e -> do
            trace_ "Find file"
            (f_, _) <- getFileName
            attribute_used_during_search <- use' cx
            ad <- use'' dta
            s <- do
              b <- globDir1 (compile $ map toUpper f_) exePath
              case b of
                (f : _) -> Just . (,) f <$> BS.readFile f
                _ -> return Nothing
            case s of
              Just (f, s') -> do
                let f' = strip $ takeFileName f
                trace_' $ "found " ++ f'
                setByteAt System (ad + 0x00) 1
                setBytesAt (ad + 0x02) $ map (fromIntegral . ord) (take 12 $ strip f_) ++ [0]
                setByteAt System (ad + 0x15) $ "attribute of matching file" @: fromIntegral attribute_used_during_search
                setWordAt System (ad + 0x16) $ "file time" @: 0 -- TODO
                setWordAt System (ad + 0x18) $ "file date" @: 0 -- TODO
                snd (dwordAt__ System $ ad + 0x1a) $ fromIntegral (BS.length s')
                setBytesAt (ad + 0x1e) $ map (fromIntegral . ord) (take 12 f') ++ [0]
                ax ..= 0 -- ?
                returnOK
              Nothing -> dosFail 0x02 -- File not found
          0x4f -> do
            ad <- use'' dta
            fname <- getBytesAt (ad + 0x02) 13
            let f_ = map (chr . fromIntegral) $ takeWhile (/= 0) fname
            trace_ $ "Find next matching file " ++ show f_
            n <- getByteAt System $ ad + 0x00
            s <- do
              b <- globDir1 (compile $ map toUpper f_) exePath
              case drop (fromIntegral n) b of
                (f : _) -> Just . (,) f <$> BS.readFile f
                _ -> return Nothing
            case s of
              Just (f, s') -> do
                trace_' $ "found: " ++ show f
                setWordAt System (ad + 0x16) $ "file time" @: 0 -- TODO
                setWordAt System (ad + 0x18) $ "file date" @: 0 -- TODO
                snd (dwordAt__ System $ ad + 0x1a) $ fromIntegral (BS.length s')
                setBytesAt (ad + 0x1e) $ map (fromIntegral . ord) (take 12 $ strip $ takeFileName f) ++ [0]
                setByteAt System (ad + 0x00) $ n + 1
                ax ..= 0 -- ?
                returnOK
              Nothing -> dosFail 0x02
          0x62 -> do
            trace_ "Get PSP address"
            bx ..= "segment address of current process" @: 0x1fe -- hack!!!  !!!
            returnOK
          _ -> haltWith $ "dos function #" ++ showHex' 2 v,
      hitem 0x24 (0x0118, 0x0110) $ do
        trace_ "critical error handler interrupt"
        haltWith "int 24",
      item 0x33 (0xc7ff, 0x0010) $ do
        -- Mouse Services
        v <- use' ax
        case v of
          0x00 -> do
            trace_ "Mouse Reset/Get Mouse Installed Flag"
            ax ..= {- "mouse?" @: 0xffff -} "mouse driver not installed" @: 0x0000
            bx ..= "number of buttons" @: 0 -- 3
          0x03 -> do
            --            trace_ "Get Mouse position and button status"
            cx ..= "mouse X" @: 0
            dx ..= "mouse Y" @: 0
            bx ..= "button status" @: 0
          0x07 -> do
            trace_ "Set Mouse Horizontal Min/Max Position"
            _ <- use' cx
            _ <- use' dx
            return ()
          0x08 -> do
            trace_ "Set Mouse Vertical Min/Max Position"
            _ <- use' cx
            _ <- use' dx
            return ()
          0x0f -> do
            trace_ "Set Mouse Mickey Pixel Ratio"
          _ -> haltWith $ "Int 33h, #" ++ showHex' 2 v
    ]
  where
    --    item :: Word8 -> (Word16, Word16) -> Machine () -> ((Word16, Word16), (Word8, Machine ()))
    item a k m = (k, (a, m >> iret))
    hitem a k m = (k, (a, m >> hiret))
    newHandle fn = do
      handle <- max 5 . imMax <$> use'' files
      files ..%= IM.insert handle (fn, 0)
      trace_' $ showHandle handle
      ax ..= "file handle" @: fromIntegral handle
      returnOK
    getFileName = do
      addr <- dxAddr
      fname <- getBytesAt addr 40
      let f = map (toUpper . chr . fromIntegral) $ takeWhile (/= 0) fname
      trace_' f
      return (f, exePath ++ "/" ++ f)
    showHandle h = "#" ++ show h
    showBytes i = show i ++ " bytes"
    showBlocks i = "0x" ++ showHex' 4 i ++ " blocks"
    dxAddr = liftM2 segAddr (use' ds) (use' dx)
    dxAddr' = liftM2 segAddr (use' es) (use' dx)
    checkExists fn cont = do
      b <- doesFileExist fn
      if b then cont else dosFail 0x02
    returnOK = carryF ..= False
    dosFail errcode = do
      trace_' $ "! " ++ showerr errcode
      ax ..= errcode
      carryF ..= True
      where
        showerr = \case
          0x01 -> "Invalid function number"
          0x02 -> "File not found"
          0x03 -> "Path not found"
          0x04 -> "Too many open files (no handles left)"
          0x05 -> "Access denied"
          0x06 -> "Invalid handle"
          0x07 -> "Memory control blocks destroyed"
          0x08 -> "Insufficient memory"
          0x09 -> "Invalid memory block address"
          0x0A -> "Invalid environment"
          0x0B -> "Invalid format"
          0x0C -> "Invalid access mode (open mode is invalid)"
          _ -> undefined

strip :: [Char] -> [Char]
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

----------------------------------------------

programSegmentPrefix' :: Word16 -> Word16 -> Word16 -> BS.ByteString -> Machine ()
programSegmentPrefix' baseseg envseg endseg args = do
  wordAt_ 0x00 $ "CP/M exit, always contain code 'int 20h'" @: 0x20CD
  wordAt_ 0x02 $ "Segment of the first byte beyond the memory allocated to the program" @: endseg
  --    bytesAt 0x05 5 .= [0xea, 0xff, 0xff, 0xad, 0xde]   -- FAR call to MSDOS function dispatcher (int 21h)?
  --    dwordAt 0x0a .= 0xf00020c8    -- Terminate address of previous program (old INT 22h)
  --    dwordAt 0x0e .= 0x01180000    -- Break address of previous program (old INT 23h)
  --    dwordAt 0x12 .= 0x01180110    -- Critical error address of previous program (old INT 24h)
  --    wordAt 0x16 .= 0x0118    -- Caller's PSP segment (usually COMMAND.COM - internal)

  -- Job File Table (JFT) (internal)
  --    bytesAt 0x18 20 .= [0x01, 0x01, 0x01, 0x00, 0x02, 0x03] ++ repeat 0xff

  wordAt_ 0x2c $ "Environment segment" @: envseg
  --    dwordAt 0x2e .= 0x0192ffe6 -- SS:SP on entry to last INT 21h call (internal)

  --    wordAt 0x32 .= 0x0014 -- JFT size (internal)
  --    dwordAt 0x34 .= 0x01920018-- Pointer to JFT (internal)
  --    dwordAt 0x38 .= 0xffffffff -- Pointer to previous PSP (only used by SHARE in DOS 3.3 and later)
  -- 3Ch-3Fh     4 bytes     Reserved
  --    wordAt 0x40 .= 0x0005 -- DOS version to return (DOS 4 and later, alterable via SETVER in DOS 5 and later)
  -- 42h-4Fh     14 bytes     Reserved
  bytesAt_ 0x50 [0xcd, 0x21, 0xcb] -- (code) Far call to DOS (always contain INT 21h + RETF)
  -- 53h-54h     2 bytes     Reserved
  -- 55h-5Bh     7 bytes     Reserved (can be used to make first FCB into an extended FCB)

  -- 5Ch-6Bh     16 bytes     Unopened Standard FCB 1
  -- 6Ch-7Fh     20 bytes     Unopened Standard FCB 2 (overwritten if FCB 1 is opened)
  --    bytesAt 0x5c (16 + 20) .= repeat 0

  byteAt_ 0x80 $ "args length" @: fromIntegral (min maxlength $ BS.length args)
  bytesAt_ 0x81 $ take maxlength (BS.unpack args) ++ [0x0D] -- Command line string
  --    byteAt 0xff .= 0x36   -- dosbox specific?
  where
    base = baseseg & paragraph
    wordAt_ i = setWordAt System (i + base)
    byteAt_ i = setByteAt System (i + base)
    bytesAt_ i = setBytesAt (i + base)
    maxlength = 125

getBytesAt :: Int -> Int -> IO [Word8]
getBytesAt i j = mapM (getByteAt System) $ take j [i ..]

setBytesAt :: Int -> [Word8] -> IO ()
setBytesAt i = zipWithM_ (setByteAt System) [i ..]

pspSegSize :: Num p => p
pspSegSize = 16

envvarsSegment :: Num p => p
envvarsSegment = 0x1f4

envvars :: [Word8]
envvars =
  map (fromIntegral . ord) "PATH=Z:\\\NULCOMSPEC=Z:\\COMMAND.COM\NULBLASTER=A220 I7 D1 H5 T6\0\0\1\0C:\\GAME.EXE"
    ++ replicate 20 0

loadExe :: Word16 -> BS.ByteString -> Machine (Int -> BSC.ByteString)
loadExe loadSegment gameExe = do
  flags ..= wordToFlags 0xf202

  heap ...= ([(pspSegment - 1, endseg)], 0xa000 - 1)
  setBytesAt (envvarsSegment & paragraph) envvars
  setBytesAt exeStart $ BS.unpack relocatedExe
  ss ..= (ssInit + loadSegment)
  sp ..= spInit
  cs ..= (csInit + loadSegment)
  ip ..= ipInit
  ds ..= pspSegment
  es ..= pspSegment
  cx ..= 0x00ff -- why?
  dx ..= pspSegment -- why?
  bp ..= 0x091c -- why?
  si ..= 0x0012 -- why?
  di ..= envvarsSegment `shiftL` 4
  labels ...= mempty

  setWordAt System 0x410 $ "equipment word" @: 0xd426 --- 0x4463   --- ???
  setByteAt System 0x417 $ "keyboard shift flag 1" @: 0x20

  forM_ [(fromIntegral a, b, m) | (b, (a, m)) <- M.toList origInterrupt] $ \(i, (hi, lo), m) -> do
    setWordAt System (4 * i) $ "interrupt lo" @: lo
    setWordAt System (4 * i + 2) $ "interrupt hi" @: hi
    cache .%= IM.insert (segAddr hi lo) (BuiltIn m)

  programSegmentPrefix' pspSegment envvarsSegment endseg ""

  gameexe ...= (exeStart, relocatedExe)

  -- hack for stunts: skip the first few instructions to ensure constant ss value during run
  ip ..= 0x004f
  si ..= 0x0d20
  di ..= 0x2d85
  sp ..= 0xcc5e
  ss ..= 0x2d85

  return getInst
  where
    getInst i
      | j >= 0 && j < BS.length relocatedExe = BS.drop j relocatedExe
      | otherwise = undefined
      where
        j = i - exeStart
    exeStart = loadSegment & paragraph
    relocatedExe = relocate relocationTable loadSegment $ BS.drop headerSize gameExe
    pspSegment = loadSegment - pspSegSize
    endseg = loadSegment + executableSegSize + additionalMemoryAllocated
    additionalMemoryAllocated = additionalMemoryNeeded
    -- could be anything between additionalMemoryNeeded and maxAdditionalMemoryNeeded

    ( 0x5a4d : bytesInLastPage : pagesInExecutable : relocationEntries
        : paragraphsInHeader
        : additionalMemoryNeeded
        : _maxAdditionalMemoryNeeded
        : ssInit
        : spInit
        : _checksum
        : ipInit
        : csInit
        : firstRelocationItemOffset
        : _overlayNumber
        : headerLeft
      ) =
        map (\[low, high] -> (high, low) ^. combine) $ everyNth 2 $ BS.unpack gameExe
    headerSize = paragraphsInHeader & paragraph
    executableSegSize =
      pagesInExecutable `shiftL` 5
        + (if bytesInLastPage > 0 then (bytesInLastPage + 0xf) `shiftL` 4 - 0x20 else 0)
        - 0x22f -- ???
    relocationTable =
      sort $
        take (fromIntegral relocationEntries) $
          map (\[a, b] -> segAddr b a) $
            everyNth 2 $
              drop (fromIntegral firstRelocationItemOffset `div` 2 - 14) headerLeft

unique :: Eq a => [a] -> Bool
unique xs = length xs == length (nub xs)

relocate :: [Int] -> Word16 -> BS.ByteString -> BS.ByteString
relocate table loc exe = BS.concat $ fst' : map add (bss ++ [last'])
  where
    (last', fst' : bss) = mapAccumL (flip go) exe $ zipWith (-) table $ 0 : table
    go r (BS.splitAt r -> (xs, ys)) = (ys, xs)
    add (BS.uncons -> Just (x, BS.uncons -> Just (y, xs))) = BS.cons x' $ BS.cons y' xs
      where
        (y', x') = combine %~ (+ loc) $ (y, x)
    add _ = undefined
