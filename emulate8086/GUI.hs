{-# LANGUAGE TypeApplications #-}

module GUI
  ( drawWithFrameBuffer,
  )
where

import Control.Concurrent
import Control.Lens
import Control.Monad as Prelude
import Data.Bits
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as U
import Data.Word
import Emulate
import Foreign
import Graphics.GL.Core32
import Graphics.UI.GLFW as GLFW
import Helper
import MachineState

------------------------------------------------

videoMem :: Num p => p
videoMem = 0xa0000

dof :: Num a => a
dof = 320 * 200

drawWithFrameBuffer :: (Machine () -> IO ()) -> (Request -> IO ()) -> IO () -> IO ()
drawWithFrameBuffer changeSt interrupt' draw = do
  _ <- GLFW.init
  esds <- newMVar False
  _ <- U.new (320 * 200) :: IO (U.IOVector Word32)
  let winW = 960
      winH = 600
      sett r = changeSt $ instPerSec ..%= r
      setOVar f = showOffset .%= max 0 . min videoMem . f
  pos <- newMVar Nothing
  Just window <- GLFW.createWindow winW winH "Haskell Stunts" Nothing Nothing
  GLFW.makeContextCurrent $ Just window
  let posToAddr _ x y = do
        offs <- use' showOffset
        let fb = heap''
            addr = offs + 320 * y + x
        val <- U.read fb addr
        return (addr, val)
  setCursorEnterCallback window $ Just $ \_ -> \case
    CursorState'NotInWindow -> modifyMVar_ pos $ const $ return Nothing
    _ -> return ()
  setCursorPosCallback window $ Just $ \_ x_ y_ -> do
    _ <- use'' id
    let x = round x_ `div` 3
        y = round y_ `div` 3
    when (0 <= x && x < 320 && 0 <= y && y < 200) $ do
      modifyMVar_ pos $ const $ return $ Just ((x, y), Nothing)
  GLFW.setKeyCallback window $ Just $ \_ key _ action _ -> do
    let send (press, release) = case action of
          GLFW.KeyState'Pressed -> interrupt' $ AskKeyInterrupt press
          GLFW.KeyState'Released -> interrupt' $ AskKeyInterrupt release
          _ -> return ()

    when (action /= GLFW.KeyState'Repeating) $ case key of
      Key'Escape -> send (0x01, 0x81)
      Key'Space -> send (0x39, 0xb9)
      Key'Enter -> send (0xe01c, 0x9c)
      Key'C -> send (0x2e, 0xae)
      Key'Left -> send (0xe04b, 0xcb)
      Key'Right -> send (0xe04d, 0xcd)
      Key'Up -> send (0xe048, 0xc8)
      Key'Down -> send (0xe050, 0xd0)
      _ -> return ()
    when (action /= GLFW.KeyState'Released) $ case key of
      Key'R -> setOVar $ const videoMem
      Key'A -> setOVar (+ dof)
      Key'S -> setOVar (+ (- dof))
      Key'X -> setOVar (+ 2 * 320)
      Key'Y -> setOVar (+ (-2 * 320))
      Key'B -> setOVar (+ 4)
      Key'V -> setOVar (+ (-4))
      Key'0 -> sett $ const 0.5
      Key'1 -> sett $ const 1
      Key'2 -> sett $ const 5
      Key'3 -> sett $ const 10
      Key'4 -> sett $ const 50
      Key'5 -> sett $ const 100
      Key'6 -> sett $ const 500
      Key'7 -> sett $ const 1000
      Key'8 -> sett $ const 5000
      Key'9 -> sett $ const 10000
      Key'N -> sett (* (1 / 1.1))
      Key'M -> sett (* 1.1)
      Key'Comma -> modifyMVar_ esds $ return . not
      Key'T -> showReads .%= not
      Key'I -> showReads' .%= not
      Key'U -> changeSt $ showCache ..%= not
      Key'P -> changeSt $ speed ..%= (3000 -)
      Key'W -> changeSt adjustCache
      Key'Q -> GLFW.setWindowShouldClose window True
      _ -> return ()

  -- create back buffer
  (tex, fbo) <- mkBackBuffer

  tv <- newEmptyMVar
  _ <- forkIO $ forever $ do
    threadDelay $ 1000000 `div` 20
    putMVar tv ()
  let mainLoop = do
        b <- GLFW.windowShouldClose window
        unless b $ do
          draw
          _ <- takeMVar tv
          st <- use'' id
          esds' <- readMVar esds
          offs <- use' showOffset
          let fb = heap''
          b' <- use' showReads
          (vec, post) <-
            if b'
              then do
                return $ (,) showBuffer $ do
                  U.set showBuffer 0
                  when (st ^. showCache) $ do
                    ca <- use' cache
                    forM_ (IM.toList $ fst $ IM.split (offs + 320 * 200) $ snd $ IM.split (offs -1) ca) $ \case
                      (_, Compiled _ _ _ es' ds' _ r _) -> forM_ r $ \(beg, end) ->
                        forM_ [max 0 $ beg - offs .. min (320 * 200 - 1) $ end - 1 - offs] $ \i -> do
                          U.unsafeWrite showBuffer i $ maybe 0xffff0000 ((.|. 0x0000ff00) . (`shiftL` 16) . fromIntegral) (if esds' then es' else ds')
                      (k, DontCache _) -> do
                        U.unsafeWrite showBuffer (k - offs) 0xffff0000
                      _ -> return ()
              else do
                let p = st ^. palette
                v <- S.unsafeFreeze fb
                vec2' <- S.unsafeThaw $ S.unsafeBackpermute p (S.map fromIntegral $ S.slice offs (320 * 200) v)
                return (vec2', return ())
          readMVar pos >>= \case
            Nothing -> return ()
            Just ((x, y), v) -> do
              v'@(addr, val) <- posToAddr st x y
              modifyMVar_ pos $ const $ return $ Just ((x, y), Just v')
              when (v /= Just v') $ onScreen $ "[" ++ showHex' 5 addr ++ "] = " ++ showHex' 2 val
              let drawPix x' y' = when (0 <= i && i < 320 * 200) $ U.write vec i 0xffffff00 where i = 320 * y' + x'
              forM_ [5 .. 8] $ \j -> do
                drawPix (x + j) y
                drawPix (x - j) y
                drawPix x (y + j)
                drawPix x (y - j)
          U.unsafeWith vec $ glTexSubImage2D GL_TEXTURE_2D 0 0 0 320 200 GL_RGBA GL_UNSIGNED_INT_8_8_8_8
          glBlitFramebuffer 0 200 320 0 0 0 (fromIntegral @Int winW) (fromIntegral @Int winH) GL_COLOR_BUFFER_BIT GL_NEAREST
          GLFW.swapBuffers window
          post
          GLFW.pollEvents
          mainLoop
  mainLoop

  -- free back buffer
  Foreign.with fbo $ glDeleteFramebuffers 1
  Foreign.with tex $ glDeleteTextures 1

  GLFW.destroyWindow window
  GLFW.terminate

-- TODO
onScreen :: String -> IO ()
onScreen = putStrLn

mkBackBuffer :: IO (GLuint, GLuint)
mkBackBuffer = do
  fbo <- alloca (\pbo -> glGenFramebuffers 1 pbo >> peek pbo)
  glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo
  tex <- alloca (\pto -> glGenTextures 1 pto >> peek pto)
  glBindTexture GL_TEXTURE_2D tex
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral GL_NEAREST
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) 320 200 0 (fromIntegral GL_RGBA) GL_UNSIGNED_BYTE nullPtr
  glFramebufferTexture2D GL_DRAW_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  if status /= GL_FRAMEBUFFER_COMPLETE
    then do
      putStrLn $ "incomplete framebuffer: " ++ show status
    else do
      glBindFramebuffer GL_READ_FRAMEBUFFER fbo
      glBindFramebuffer GL_DRAW_FRAMEBUFFER 0
  return (tex, fbo)
