import Control.Concurrent
import qualified Data.ByteString as BS
import Dos
import Emulate
import GUI
import MachineState
import Sound.ALUT
import System.IO

--------------------------------------------------------------------------------

gameFile :: [Char]
gameFile = "restunts/stunts/game.exe"

loadSegment :: Num p => p
loadSegment = 0x20e

main :: IO ()
main = withProgNameAndArgs runALUT $ \_ args -> do
  let cycles = case args of
        [n] -> read n
        _ -> 1000000000 -- hack
  bData <- createBufferData (Square 440 0 0.1)
  buff <- createBuff bData 1
  [source] <- genObjectNames 1
  loopingMode source $= Looping
  stop [source]
  buffer source $= Just buff

  hSetBuffering stdout NoBuffering
  let addChange m = modifyMVar_ changeState $ \n -> return $ n >> m
  --    l <- getLabels
  tid <- myThreadId
  game <- BS.readFile gameFile
  ir <- use'' interruptRequest
  _ <- forkIO $ do
    verboseLevel ...= 1
    soundSource ...= source
    getInst <- loadExe loadSegment game
    loadCache getInst
    _ <- forkIO timerThread
    let cyc = mkStep >>= checkInt cycles cyc
    cyc
    killThread tid
  drawWithFrameBuffer addChange (\r -> modifyMVar_ ir $ return . (++ [r])) $ return ()

createBuff :: BufferData a -> Frequency -> IO Buffer
createBuff (BufferData m fmt f) x = do
  [b] <- genObjectNames 1
  bufferData b $= BufferData m fmt (x * f)
  return b
