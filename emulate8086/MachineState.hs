{-# LANGUAGE TemplateHaskell #-}

module MachineState where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Lens as Lens
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as U
import Data.Word
import Edsl
import Helper
import Sound.ALUT
import System.IO.Unsafe
import Prelude

----------------------------------------------

data Request
  = AskKeyInterrupt Word16
  | AskTimerInterrupt Int

type Flags = Word16

type Region = (Int, Int)

type Regions = [Region]

type MemPiece = ([(Word16, Word16)], Word16)

type Cache = IM.IntMap CacheEntry

type Cache2 = IM.IntMap IS.IntSet

data CacheEntry
  = Compiled Bool !Word16 {-cs-} !Word16 {-ss-} !(Maybe Word16 {-es-}) !(Maybe Word16 {-ds-}) !Int {-num of instr-} !Regions !(Machine ())
  | BuiltIn !(Machine ())
  | DontCache !Int

heap'' :: U.IOVector Word8
{-# NOINLINE heap'' #-}
heap'' = unsafePerformIO $ U.new 0xb0000

regs :: U.IOVector Word16
{-# NOINLINE regs #-}
regs = U.unsafeCast regs'

regs' :: U.IOVector Word8
{-# NOINLINE regs' #-}
regs' = unsafePerformIO $ U.new $ 2 * (13 + 1)

type MachinePart'' a = (IO a, a -> IO ())

showReads :: (IO Bool, Bool -> IO ())
showReads = refPart _showReads

showReads' :: (IO Bool, Bool -> IO ())
showReads' = refPart _showReads'

showOffset :: (IO Int, Int -> IO ())
showOffset = refPart _showOffset

stepsCounter :: (IO Int, Int -> IO ())
stepsCounter = refPart _stepsCounter

_showReads, _showReads' :: IORef Bool

_showOffset, _stepsCounter :: IORef Int

{-# NOINLINE _showReads #-}
_showReads = unsafePerformIO $ newIORef False

{-# NOINLINE _showReads' #-}
_showReads' = unsafePerformIO $ newIORef False

{-# NOINLINE _showOffset #-}
_showOffset = unsafePerformIO $ newIORef 0xa0000

{-# NOINLINE _stepsCounter #-}
_stepsCounter = unsafePerformIO $ newIORef 0

{-# INLINE refPart #-}
refPart :: IORef a -> (IO a, a -> IO ())
refPart x = (readIORef x, writeIORef x)

showBuffer :: U.IOVector Word32
{-# NOINLINE showBuffer #-}
showBuffer = unsafePerformIO $ U.new (320 * 200)

cache :: (IO Cache, Cache -> IO ())
cache = refPart _cache

_cache :: IORef Cache
{-# NOINLINE _cache #-}
_cache = unsafePerformIO $ newIORef IM.empty

cache2 :: (IO Cache2, Cache2 -> IO ())
cache2 = refPart _cache2

_cache2 :: IORef Cache2
{-# NOINLINE _cache2 #-}
_cache2 = unsafePerformIO $ newIORef IM.empty

{-# NOINLINE changeState #-}
changeState :: MVar (IO ())
changeState = unsafePerformIO $ newMVar $ return ()

data MachineState = MachineState
  { _verboseLevel :: !Int,
    _showCache :: !Bool,
    _instPerSec :: !Float, -- Hz
    _speed :: !Int, -- 0: stop
    _counter :: !Int, -- timer interrupt counter
    _timerOn :: !Bool,
    _palette :: !(V.Vector Word32),
    _gameexe :: (Int, BS.ByteString),
    _soundSource :: Source,
    _frequency :: !Word16, -- speaker frequency
    _interruptRequest :: !(MVar [Request]),
    _keyDown :: !Word16,
    _speaker :: !Word8, -- 0x61 port
    _heap :: !MemPiece, -- heap layout
    _stack :: [(Word16, Int, Machine Jump')],
    _retrace :: ![Word16],
    _intMask :: !Word8,
    _labels :: !(IM.IntMap BS.ByteString),
    _files :: !(IM.IntMap (FilePath, Int)), -- filepath, position
    _dta :: !Int
  }

type Machine = IO

--type MachinePart a = Lens' MachineState a
type MachinePart' a = (Machine a, a -> Machine ())

$(makeLenses ''MachineState)

wordToFlags :: Word16 -> Flags
wordToFlags w = fromIntegral $ (w .&. 0x0ed3) .|. 0x2

{-# NOINLINE emptyState #-}
emptyState :: IORef MachineState
emptyState = unsafePerformIO $ do
  ivar <- newMVar []
  newIORef $
    MachineState
      { _heap = ([], 0xa000),
        _stack = [],
        _labels = IM.empty,
        _files = IM.empty,
        _dta = 0,
        _retrace = cycle [1, 9, 0, 8], --     [1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0]
        _intMask = 0xf8,
        _verboseLevel = 2,
        _showCache = True,
        _instPerSec = 100,
        _speed = 3000,
        _counter = 0,
        _timerOn = False,
        _speaker = 0x30, -- ??
        _palette = defaultPalette,
        _keyDown = 0x00,
        _interruptRequest = ivar,
        _soundSource = undefined,
        _frequency = 0x0000,
        _gameexe = undefined
      }

defaultPalette :: V.Vector Word32
defaultPalette =
  V.fromList $
    Prelude.map
      (`shiftL` 8)
      [ 0x000000,
        0x0000a8,
        0x00a800,
        0x00a8a8,
        0xa80000,
        0xa800a8,
        0xa85400,
        0xa8a8a8,
        0x545454,
        0x5454fc,
        0x54fc54,
        0x54fcfc,
        0xfc5454,
        0xfc54fc,
        0xfcfc54,
        0xfcfcfc,
        0x000000,
        0x141414,
        0x202020,
        0x2c2c2c,
        0x383838,
        0x444444,
        0x505050,
        0x606060,
        0x707070,
        0x808080,
        0x909090,
        0xa0a0a0,
        0xb4b4b4,
        0xc8c8c8,
        0xe0e0e0,
        0xfcfcfc,
        0x0000fc,
        0x4000fc,
        0x7c00fc,
        0xbc00fc,
        0xfc00fc,
        0xfc00bc,
        0xfc007c,
        0xfc0040,
        0xfc0000,
        0xfc4000,
        0xfc7c00,
        0xfcbc00,
        0xfcfc00,
        0xbcfc00,
        0x7cfc00,
        0x40fc00,
        0x00fc00,
        0x00fc40,
        0x00fc7c,
        0x00fcbc,
        0x00fcfc,
        0x00bcfc,
        0x007cfc,
        0x0040fc,
        0x7c7cfc,
        0x9c7cfc,
        0xbc7cfc,
        0xdc7cfc,
        0xfc7cfc,
        0xfc7cdc,
        0xfc7cbc,
        0xfc7c9c,
        0xfc7c7c,
        0xfc9c7c,
        0xfcbc7c,
        0xfcdc7c,
        0xfcfc7c,
        0xdcfc7c,
        0xbcfc7c,
        0x9cfc7c,
        0x7cfc7c,
        0x7cfc9c,
        0x7cfcbc,
        0x7cfcdc,
        0x7cfcfc,
        0x7cdcfc,
        0x7cbcfc,
        0x7c9cfc,
        0xb4b4fc,
        0xc4b4fc,
        0xd8b4fc,
        0xe8b4fc,
        0xfcb4fc,
        0xfcb4e8,
        0xfcb4d8,
        0xfcb4c4,
        0xfcb4b4,
        0xfcc4b4,
        0xfcd8b4,
        0xfce8b4,
        0xfcfcb4,
        0xe8fcb4,
        0xd8fcb4,
        0xc4fcb4,
        0xb4fcb4,
        0xb4fcc4,
        0xb4fcd8,
        0xb4fce8,
        0xb4fcfc,
        0xb4e8fc,
        0xb4d8fc,
        0xb4c4fc,
        0x000070,
        0x1c0070,
        0x380070,
        0x540070,
        0x700070,
        0x700054,
        0x700038,
        0x70001c,
        0x700000,
        0x701c00,
        0x703800,
        0x705400,
        0x707000,
        0x547000,
        0x387000,
        0x1c7000,
        0x007000,
        0x00701c,
        0x007038,
        0x007054,
        0x007070,
        0x005470,
        0x003870,
        0x001c70,
        0x383870,
        0x443870,
        0x543870,
        0x603870,
        0x703870,
        0x703860,
        0x703854,
        0x703844,
        0x703838,
        0x704438,
        0x705438,
        0x706038,
        0x707038,
        0x607038,
        0x547038,
        0x447038,
        0x387038,
        0x387044,
        0x387054,
        0x387060,
        0x387070,
        0x386070,
        0x385470,
        0x384470,
        0x505070,
        0x585070,
        0x605070,
        0x685070,
        0x705070,
        0x705068,
        0x705060,
        0x705058,
        0x705050,
        0x705850,
        0x706050,
        0x706850,
        0x707050,
        0x687050,
        0x607050,
        0x587050,
        0x507050,
        0x507058,
        0x507060,
        0x507068,
        0x507070,
        0x506870,
        0x506070,
        0x505870,
        0x000040,
        0x100040,
        0x200040,
        0x300040,
        0x400040,
        0x400030,
        0x400020,
        0x400010,
        0x400000,
        0x401000,
        0x402000,
        0x403000,
        0x404000,
        0x304000,
        0x204000,
        0x104000,
        0x004000,
        0x004010,
        0x004020,
        0x004030,
        0x004040,
        0x003040,
        0x002040,
        0x001040,
        0x202040,
        0x282040,
        0x302040,
        0x382040,
        0x402040,
        0x402038,
        0x402030,
        0x402028,
        0x402020,
        0x402820,
        0x403020,
        0x403820,
        0x404020,
        0x384020,
        0x304020,
        0x284020,
        0x204020,
        0x204028,
        0x204030,
        0x204038,
        0x204040,
        0x203840,
        0x203040,
        0x202840,
        0x2c2c40,
        0x302c40,
        0x342c40,
        0x3c2c40,
        0x402c40,
        0x402c3c,
        0x402c34,
        0x402c30,
        0x402c2c,
        0x40302c,
        0x40342c,
        0x403c2c,
        0x40402c,
        0x3c402c,
        0x34402c,
        0x30402c,
        0x2c402c,
        0x2c4030,
        0x2c4034,
        0x2c403c,
        0x2c4040,
        0x2c3c40,
        0x2c3440,
        0x2c3040,
        0x000000,
        0x000000,
        0x000000,
        0x000000,
        0x000000,
        0x000000,
        0x000000,
        0x000000
      ]

infix 4 ..=, .%=, ...=, ..%=

{-# INLINE (..=) #-}
(..=) :: (a, t1 -> t2) -> t1 -> t2
w ..= x = snd w x

(.%=) :: Monad m => (m a, b1 -> m b2) -> (a -> b1) -> m b2
(r, w) .%= f = r >>= w . f

{-# INLINE use' #-}
use' :: (a, b) -> a
use' = fst

use'' :: Getting b MachineState b -> IO b
use'' k = (^. k) <$> readIORef emptyState

(...=) :: ASetter MachineState MachineState a b -> b -> IO ()
k ...= v = modifyIORef' emptyState $ k .~ v

(..%=) ::
  ASetter MachineState MachineState a b ->
  (a -> b) ->
  IO ()
k ..%= v = modifyIORef' emptyState $ k %~ v

{-# INLINE ax #-}
ax :: MachinePart'' Word16
ax = ff 0

{-# INLINE bx #-}
bx :: MachinePart'' Word16
bx = ff 1

{-# INLINE cx #-}
cx :: MachinePart'' Word16
cx = ff 2

{-# INLINE dx #-}
dx :: MachinePart'' Word16
dx = ff 3

{-# INLINE si #-}
si :: MachinePart'' Word16
si = ff 4

{-# INLINE di #-}
di :: MachinePart'' Word16
di = ff 5

{-# INLINE cs #-}
cs :: MachinePart'' Word16
cs = ff 6

{-# INLINE ss #-}
ss :: MachinePart'' Word16
ss = ff 7

{-# INLINE ds #-}
ds :: MachinePart'' Word16
ds = ff 8

{-# INLINE es #-}
es :: MachinePart'' Word16
es = ff 9

{-# INLINE sp #-}
sp :: MachinePart'' Word16
sp = ff 10

{-# INLINE bp #-}
bp :: MachinePart'' Word16
bp = ff 11

{-# INLINE ip #-}
ip :: MachinePart'' Word16
ip = ff 12

{-# INLINE flags_ #-}
flags_ :: MachinePart'' Word16
flags_ = ff 13

{-# INLINE ff #-}
ff :: Int -> MachinePart'' Word16
ff i = (U.unsafeRead regs i, U.unsafeWrite regs i) :: MachinePart'' Word16

flags :: MachinePart'' Word16
flags = second (. wordToFlags) flags_

{-# INLINE al #-}
al :: MachinePart'' Word8
al = rLow 0

{-# INLINE bl #-}
bl :: MachinePart'' Word8
bl = rLow 2

{-# INLINE cl #-}
cl :: MachinePart'' Word8
cl = rLow 4

{-# INLINE dl #-}
dl :: MachinePart'' Word8
dl = rLow 6

{-# INLINE rLow #-}
rLow :: Int -> MachinePart'' Word8
rLow i = (U.unsafeRead regs' i, U.unsafeWrite regs' i) :: MachinePart'' Word8

{-# INLINE ah #-}
ah :: MachinePart'' Word8
ah = rHigh 1

{-# INLINE bh #-}
bh :: MachinePart'' Word8
bh = rHigh 3

{-# INLINE ch #-}
ch :: MachinePart'' Word8
ch = rHigh 5

{-# INLINE dh #-}
dh :: MachinePart'' Word8
dh = rHigh 7

{-# INLINE rHigh #-}
rHigh :: Int -> MachinePart'' Word8
rHigh i = (U.unsafeRead regs' i, U.unsafeWrite regs' i) :: MachinePart'' Word8

dxax, cxdx :: MachinePart'' Word32
dxax = comb dx ax
cxdx = comb cx dx

comb ::
  ( Monad m1,
    Monad m2,
    Integral a1,
    Integral a2,
    Integral a3,
    Bits r,
    Bits a3,
    Num r,
    Num t1,
    Num t2
  ) =>
  (m1 a1, t1 -> m2 a4) ->
  (m1 a2, t2 -> m2 b) ->
  (m1 r, a3 -> m2 b)
comb (rh, wh) (rl, wl) =
  ( liftM2 (\h l -> fromIntegral h `shiftL` 16 .|. fromIntegral l) rh rl,
    \x -> wh (fromIntegral $ x `shiftR` 16) >> wl (fromIntegral x)
  )

{-# INLINE overflowF #-}
overflowF :: MachinePart'' Bool
overflowF = flag 11

directionF :: MachinePart'' Bool
directionF = flag 10

interruptF :: MachinePart'' Bool
interruptF = flag 9

{-# INLINE signF #-}
signF :: MachinePart'' Bool
signF = flag 7

{-# INLINE zeroF #-}
zeroF :: MachinePart'' Bool
zeroF = flag 6

{-# INLINE parityF #-}
parityF :: MachinePart'' Bool
parityF = flag 2

{-# INLINE carryF #-}
carryF :: MachinePart'' Bool
carryF = flag 0

{-# INLINE flag #-}
flag :: Int -> MachinePart'' Bool
flag i = ((`testBit` i) <$> r, \b -> r >>= w . if b then (`setBit` i) else (`clearBit` i)) :: MachinePart'' Bool
  where
    (r, w) = flags_

data Info
  = System
  | forall e. Program (EExp e Int)

info :: Info -> a -> a -> a -> a
info System s1 _ _ = s1
info (Program (SegAddr (C _) (C _))) _ s2 _ = s2
info _ _ _ s3 = s3

getByteAt :: Info -> Int -> IO Word8
getByteAt inf i = do
  when debug $ do
    b <- use' showReads'
    when b $ do
      off <- use' showOffset
      let j = i - off
      when (0 <= j && j < 320 * 200) $ do
        x <- U.unsafeRead showBuffer j
        U.unsafeWrite showBuffer j $ x .|. info inf 0xff00ff00 0x00008000 0x0000ff00
  U.unsafeRead heap'' i

setByteAt :: Info -> Int -> Word8 -> IO ()
setByteAt inf i v = do
  U.unsafeWrite heap'' i v
  when debug $ do
    b <- use' showReads
    when b $ do
      off <- use' showOffset
      let j = i - off
      when (0 <= j && j < 320 * 200) $ do
        x <- U.unsafeRead showBuffer j
        U.unsafeWrite showBuffer j $ x .|. info inf 0xffff0000 0x00800000 0x00ff0000

getWordAt :: Info -> Int -> Machine Word16
getWordAt inf i | even i = do
  when debug $ do
    b <- use' showReads'
    when b $ do
      off <- use' showOffset
      let j = i - off
      when (0 <= j && j < 320 * 200) $ do
        -- TODO
        x <- U.unsafeRead showBuffer j
        U.unsafeWrite showBuffer j $ x .|. info inf 0xff00ff00 0x00008000 0x0000ff00
  U.unsafeRead heap''' (i `shiftR` 1)
getWordAt inf i = liftM2 (\hi lo -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo) (getByteAt inf (i + 1)) (getByteAt inf i)

heap''' :: U.IOVector Word16
heap''' = U.unsafeCast heap''

setWordAt :: Info -> Int -> Word16 -> Machine ()
setWordAt inf i v | even i = do
  when debug $ do
    b <- use' showReads
    when b $ do
      -- TODO
      off <- use' showOffset
      let j = i - off
      when (0 <= j && j < 320 * 200) $ do
        x <- U.unsafeRead showBuffer j
        U.unsafeWrite showBuffer j $ x .|. info inf 0xffff0000 0x00800000 0x00ff0000
  U.unsafeWrite heap''' (i `shiftR` 1) v
setWordAt inf i v = setByteAt inf i (fromIntegral v) >> setByteAt inf (i + 1) (fromIntegral $ v `shiftR` 8)

dwordAt__ :: Info -> Int -> MachinePart' Word32
dwordAt__ inf i = comb (getWordAt inf (i + 2), setWordAt inf (i + 2)) (getWordAt inf i, setWordAt inf i)

trace_, trace_' :: String -> Machine ()
trace_ s = putStr $ " | " ++ s
trace_' s = putStr $ " " ++ s

push :: Word16 -> Machine ()
push x = do
  sp .%= (`subtract` 2)
  ad <- liftM2 segAddr (use' ss) (use' sp)
  setWordAt System ad x

pop :: Machine Word16
pop = do
  ad <- liftM2 segAddr (use' ss) (use' sp)
  x <- getWordAt System ad
  sp .%= (+ 2)
  return x
