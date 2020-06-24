{-# LANGUAGE BangPatterns #-}

module Emulate where

import CPU
import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad.Reader
import Data.Bits hiding (bit)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Storable as US
import qualified Data.Vector.Storable.Mutable as U
import Data.Word
import DeBruijn
import Dos (input, output')
import Edsl
import Helper
import MachineState
import System.Directory
import Prelude

--------------------------------------

interrupt :: Word8 -> Machine ()
interrupt v = do
  use' flags >>= push
  use' cs >>= push
  use' ip >>= push
  interruptF ..= False
  getWordAt System (ad + 2) >>= (cs ..=)
  getWordAt System ad >>= (ip ..=)
  where
    ad = 4 * fromIntegral v

getFetcher :: Machine Fetcher
getFetcher = do
  v <- US.unsafeFreeze heap''
  (start, bs) <- use'' gameexe
  _ <- use' ip
  _ <- use' cs
  let f ips
        | 0 <= i && i < BS.length bs {-if x /= x' then error $ "getFetcher: " ++ show ((cs_,ip_), ips) else-} = x'
        | otherwise = x
        where
          x = disassemble . BS.pack . map fromIntegral . US.toList $ US.slice ips 7 v
          x' = disassemble . BS.drop i $ bs
          i = ips - start
  return f

fetchBlock_' ::
  p ->
  Fetcher ->
  Word16 ->
  Word16 ->
  Maybe Word16 ->
  Maybe Word16 ->
  Word16 ->
  IO CacheEntry
fetchBlock_' _ f cs' ss' es' ds' ip' = do
  jumps <- use' cache2
  let (n, r, e) = fetchBlock_ (\i -> fromMaybe mempty $ IM.lookup i jumps) f cs' ss' es' ds' ip'
  _ <- evaluate n
  let !cc = spTrans $ convExpM $ snd $ head $ IM.toList e
      !dd = evalExpM' cc
  return $ Compiled (not $ highAddr $ segAddr cs' ip') cs' ss' es' ds' n r $ do
    _ <- dd
    b <- use' showReads
    when b $ do
      off <- use' showOffset
      forM_ r $ \(beg, end) -> forM_ [max 0 $ beg - off .. min (320 * 200 - 1) $ end - 1 - off] $ \i -> do
        x <- U.unsafeRead showBuffer i
        U.unsafeWrite showBuffer i $ x .|. 0xff000000

fetchBlock :: Cache -> Machine CacheEntry
fetchBlock ca = do
  es_ <- use' es
  ds_ <- use' ds
  fetchBlock'' (Just es_) (Just ds_) ca

fetchBlock'' :: Maybe Word16 -> Maybe Word16 -> p -> IO CacheEntry
fetchBlock'' es' ds' ca = do
  cs_ <- use' cs
  ss_ <- use' ss
  ip_ <- use' ip
  f <- getFetcher
  fetchBlock_' ca f cs_ ss_ es' ds' ip_

mkStep :: Machine Int
mkStep = do
  ips <- liftM2 segAddr (use' cs) (use' ip)
  cv <- IM.lookup ips <$> use' cache
  case cv of
    Just v -> case v of
      Compiled _ cs' ss' es' ds' n _ m -> do
        when debug $ do
          cs'' <- use' cs
          when (cs' /= cs'') $ error "cs differs"
          ss'' <- use' ss
          when (ss' /= ss'') $ error "ss differs"
        es'' <- use' es
        ds'' <- use' ds
        if maybe False (/= es'') es' || maybe False (/= ds'') ds'
          then do
            trace_ "recompile"
            let f a b = if a == Just b then a else Nothing
            compile $ fetchBlock'' (f es' es'') (f ds' ds'')
          else do
            m
            return n
      BuiltIn m -> do
        m
        return 1
      DontCache _ -> do
        Compiled _ _ _ _ _ n _ ch' <- fetchBlock mempty
        ch'
        return n
    Nothing -> do
      compile fetchBlock

compile :: (Cache -> IO CacheEntry) -> Machine Int
compile fe = do
  ip_ <- use' ip
  cs_ <- use' cs
  let ips = segAddr cs_ ip_
  entry@(Compiled b _ _ _ _ n _ ch') <- do
    let ca = mempty
    fe ca
  when (cacheOK ips || not b) $ cache .%= IM.insert ips entry
  ch'
  return n

-- ad-hoc hacking for stunts!
cacheOK :: (Ord a, Num a) => a -> Bool
cacheOK ips = ips < 0x39000 || ips >= 0x3a700

highAddr :: (Ord a, Num a) => a -> Bool
highAddr ips = ips >= 0x3a700

type MachinePart' a = (Machine a, a -> Machine ())

evalPart_, evalPart__ :: Part_ e a -> MachinePart'' a
evalPart_ = evalPart__
evalPart__ = \case
  AX -> ax
  BX -> bx
  CX -> cx
  DX -> dx
  SI -> si
  DI -> di
  BP -> bp
  SP -> sp
  Es -> es
  Ds -> ds
  Ss -> ss
  Cs -> cs
  AL -> al
  BL -> bl
  CL -> cl
  DL -> dl
  AH -> ah
  BH -> bh
  CH -> ch
  DH -> dh
  CF -> carryF
  PF -> parityF
  ZF -> zeroF
  SF -> signF
  IF -> interruptF
  DF -> directionF
  OF -> overflowF
  Edsl.Flags -> flags
  DXAX -> dxax
  _ -> undefined

-------------

data Env :: List * -> * where
  Empty :: Env 'Nil
  Push :: {getPushEnv :: Env env, getPushVal :: t} -> Env ('Con t env)

prj :: Var env t -> Env env -> t
prj VarZ = getPushVal
prj (VarS ix) = prj ix . getPushEnv

type Machine' e = ReaderT (Env e) IO

iterateM :: (Eq t, Num t, Monad m) => t -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n -1) f

iff :: p -> p -> Bool -> p
iff x _ True = x
iff _ y _ = y

pushVal :: Machine' ('Con b e) a -> b -> Machine' e a
pushVal (ReaderT m) v = ReaderT $ \x -> m (x `Push` v)

{-# NOINLINE evalExp #-}
evalExp :: EExp e a -> Machine' e a
evalExp (Var ix) = reader (prj ix)
evalExp (Let e (DB f)) = evalExp e >>= pushVal (evalExp f)
evalExp (Iterate n (DB f) a) = do
  i <- evalExp n
  evalExp a >>= iterateM i (pushVal (evalExp f))
evalExp (C a) = return a
evalExp (Get (Heap16 e)) = evalExp e >>= lift . getWordAt (Program e)
evalExp (Get (Heap8 e)) = evalExp e >>= lift . getByteAt (Program e)
evalExp (Get p) = let !x = fst $ evalPart_ p in lift x
evalExp (If b (C x) (C y)) = iff x y <$> evalExp b
evalExp (If b x y) = evalExp b >>= iff (evalExp x) (evalExp y)
evalExp (Eq (C x) y) = (== x) <$> evalExp y
evalExp (Eq y (C x)) = (== x) <$> evalExp y
evalExp (Eq x y) = liftM2 (==) (evalExp x) (evalExp y)
evalExp (Not a) = complement <$> evalExp a
evalExp (ShiftL a) = (`shiftL` 1) <$> evalExp a
evalExp (ShiftR a) = (`shiftR` 1) <$> evalExp a
evalExp (RotateL a) = (`rotateL` 1) <$> evalExp a
evalExp (RotateR a) = (`rotateR` 1) <$> evalExp a
evalExp (Sub (C a) b) = (a -) <$> evalExp b
evalExp (Sub b (C a)) = (+ (- a)) <$> evalExp b
evalExp (Sub a b) = liftM2 (-) (evalExp a) (evalExp b)
evalExp (Add (C a) b) = (+ a) <$> evalExp b
evalExp (Add b (C a)) = (+ a) <$> evalExp b
evalExp (Add a b) = liftM2 (+) (evalExp a) (evalExp b)
evalExp (Mul a b) = liftM2 (*) (evalExp a) (evalExp b)
evalExp (QuotRem a b) = liftM2 quotRem (evalExp a) (evalExp b)
evalExp (And (C a) b) = (.&. a) <$> evalExp b
evalExp (And b (C a)) = (.&. a) <$> evalExp b
evalExp (And a b) = liftM2 (.&.) (evalExp a) (evalExp b)
evalExp (Or (C a) b) = (.|. a) <$> evalExp b
evalExp (Or b (C a)) = (.|. a) <$> evalExp b
evalExp (Or a b) = liftM2 (.|.) (evalExp a) (evalExp b)
evalExp (Xor (C a) b) = xor a <$> evalExp b
evalExp (Xor b (C a)) = xor a <$> evalExp b
evalExp (Xor a b) = liftM2 xor (evalExp a) (evalExp b)
evalExp (EvenParity e) = even . popCount <$> evalExp e
evalExp (SegAddr (C i) f) = (fromIntegral i `shiftL` 4 +) . fromIntegral <$> evalExp f
evalExp (SegAddr e f) = liftM2 segAddr (evalExp e) (evalExp f)
evalExp (Convert e) = fromIntegral <$> evalExp e
evalExp (Tuple a b) = liftM2 (,) (evalExp a) (evalExp b)
evalExp (Fst p) = fst <$> evalExp p
evalExp (Snd p) = snd <$> evalExp p

evalExpM' :: EExpM 'Nil Jump' -> IO ()
evalExpM' e = let !m = evalExpM mempty e in runReaderT m Empty >>= \(JumpAddr c i) -> cs ..= c >> ip ..= i

liftMa :: Machine' e a -> Machine' ('Con x e) a
liftMa (ReaderT f) = ReaderT $ f . getPushEnv

{-# NOINLINE evalExpM #-}
evalExpM :: forall e a. IM.IntMap (Machine' e Jump') -> EExpM e a -> Machine' e a
evalExpM ca (LetM e (DBM f)) = evalExp e >>= pushVal (evalExpM (IM.map liftMa ca) f)
evalExpM ca (Set (Heap16 (C e_)) (C e') c) = lift (setWordAt (Program $ C e_) e_ e') >> evalExpM ca c
evalExpM ca (Set (Heap16 e) (C e') c) = do
  e_ <- evalExp e
  lift (setWordAt (Program e) e_ e')
  evalExpM ca c
evalExpM ca (Set (Heap8 e) (C e') c) = do
  e_ <- evalExp e
  lift (setByteAt (Program e) e_ e')
  evalExpM ca c
evalExpM ca (Set p (C e') c) = let x = snd $ evalPart_ p in lift (x e') >> evalExpM ca c
evalExpM ca (Set (Heap16 (C e_)) e' c) = do
  e_' <- evalExp e'
  lift (setWordAt (Program $ C e_) e_ e_')
  evalExpM ca c
evalExpM ca (Set (Heap16 e) e' c) = do
  e_ <- evalExp e
  e_' <- evalExp e'
  lift (setWordAt (Program e) e_ e_')
  evalExpM ca c
evalExpM ca (Set (Heap8 e) e' c) = do
  e_ <- evalExp e
  e_' <- evalExp e'
  lift (setByteAt (Program e) e_ e_')
  evalExpM ca c
evalExpM ca (Set p e' c) = let x = snd $ evalPart_ p in evalExp e' >>= lift . x >> evalExpM ca c
evalExpM _ (Ret (C a)) = return a
evalExpM _ (Ret a) = evalExp a
evalExpM ca (IfM (C b) x y) = if b then evalExpM ca x else evalExpM ca y
evalExpM ca (IfM b x y) = evalExp b >>= iff (evalExpM ca x) (evalExpM ca y)
evalExpM ca (IfM' b x y (DBM f)) = evalExp b >>= iff (evalExpM ca x) (evalExpM ca y) >>= pushVal (evalExpM (IM.map liftMa ca) f)
evalExpM ca (Replicate n (C True) e (DBM f)) = do
  n' <- evalExp n
  replicateM_ (fromIntegral n') (evalExpM mempty e)
  pushVal (evalExpM (IM.map liftMa ca) f) (0 `asTypeOf` n')
evalExpM ca (Replicate n b e (DBM f)) = evalExp n >>= replicateM' (evalExp b) (evalExpM mempty e) >>= pushVal (evalExpM (IM.map liftMa ca) f)
evalExpM ca (Input a (DBM f)) = evalExp a >>= lift . input >>= pushVal (evalExpM (IM.map liftMa ca) f)
evalExpM ca (Output a b c) = (lift =<< liftM2 output' (evalExp a) (evalExp b)) >> evalExpM ca c
evalExpM ca (Loc ip' c) =
  let m = evalExpM ca' c
      ca' = IM.insert (fromIntegral ip') m ca
   in m
evalExpM ca (Trace s c) = lift (trace_ s) >> evalExpM ca c
evalExpM ca (Call c i ips cont) = do
  st <- ask
  lift $ do
    sp' <- use' sp
    stack ..%= ((sp', ips, flip runReaderT st $ evalExpM ca cont) :)
  liftM2 JumpAddr (evalExp c) (evalExp i)
evalExpM ca (SelfJump prep fallback ip') =
  let m1 = evalExpM mempty prep
      m2 = ca IM.! fromIntegral ip'
      fa = evalExpM ca fallback
   in do
        checkInt' 1 fa (m1 >> m2)
evalExpM _ (Jump' (Left True) c i) = do
  c' <- evalExp c
  i' <- evalExp i
  s <- lift $ use'' stack
  sp' <- lift $ use' sp
  let find' [] = ([], Nothing)
      find' xs'@((sp'', ips, m) : xs)
        | sp' < sp'' = (xs', Nothing)
        | sp' > sp'' = find' xs
        | ips == segAddr c' i' = (xs, Just m)
        | otherwise = (xs', Nothing)
      (s', r) = find' s
  lift $ stack ...= s'
  case r of
    Just m -> lift m
    Nothing -> return $ JumpAddr c' i'
evalExpM _ (Jump' (Left False) (C c) (C i)) = return $ JumpAddr c i
evalExpM _ (Jump' (Left False) c i) = liftM2 JumpAddr (evalExp c) (evalExp i)
evalExpM ca (Jump' (Right ((cs0, ip0), table, fallback)) cs' ip') =
  let table' = IM.map (evalExpM ca) table
      end = evalExpM ca fallback
   in do
        cs'' <- evalExp cs'
        ip'' <- evalExp ip'
        let ip''' = fromIntegral ip''
        if cs0 /= cs''
          then end
          else case IM.lookup ip''' table' of
            Just m -> m
            Nothing -> do
              lift $ cache2 .%= alter' (segAddr cs0 ip0) (IS.insert ip''')
              end
evalExpM _ _ = undefined

alter' :: Monoid t => IS.Key -> (t -> t) -> IM.IntMap t -> IM.IntMap t
alter' i f = IM.alter (Just . maybe (f mempty) f) i

replicateM' :: (Eq a1, Num a1, Monad m) => m Bool -> m a2 -> a1 -> m a1
replicateM' _ _ n@0 = return n
replicateM' b m n = do
  _ <- m
  y <- b
  let !n' = n - 1
  if y then replicateM' b m n' else return n'

checkInt' :: Int -> Machine' e a -> Machine' e a -> Machine' e a
checkInt' n fail' cont = do
  ns <- lift $ use' stepsCounter
  let !ns' = ns + n
  lift $ stepsCounter ..= ns'
  let ma = complement 0xff
  if ns' .&. ma == ns .&. ma
    then cont
    else do
      i <- lift $ use' interruptF
      if not i
        then cont
        else do
          mask' <- lift $ use'' intMask
          ivar <- lift $ use'' interruptRequest
          ints <- lift $ takeMVar ivar
          cc <- lift $ use'' counter
          let ibit = \case
                AskTimerInterrupt {} -> 0
                AskKeyInterrupt {} -> 1
              getFirst [] = (cont, [])
              getFirst (x : xs) = case x of
                AskTimerInterrupt c | c /= cc -> getFirst xs
                _ | testBit mask' $ ibit x -> (m, x : xs') where (m, xs') = getFirst xs
                AskTimerInterrupt _ -> (fail', x : xs)
                AskKeyInterrupt _ -> (fail', x : xs)
              (now, later) = getFirst ints
          lift $ putMVar ivar later
          now

checkInt :: Int -> IO () -> Int -> IO ()
checkInt cycles cont n = do
  ns <- use' stepsCounter
  let !ns' = ns + n
  stepsCounter ..= ns'
  let ma = complement 0xff
  if ns' .&. ma == ns .&. ma
    then cont
    else do
      join $ modifyMVar changeState $ \m -> return (return (), m)
      i <- use' interruptF
      when i $ do
        mask' <- use'' intMask
        ivar <- use'' interruptRequest
        ints <- takeMVar ivar
        cc <- use'' counter
        let ibit = \case
              AskTimerInterrupt {} -> 0
              AskKeyInterrupt {} -> 1
            getFirst [] = (return (), [])
            getFirst (x : xs) = case x of
              AskTimerInterrupt c | c /= cc -> getFirst xs
              _ | testBit mask' $ ibit x -> (m, x : xs') where (m, xs') = getFirst xs
              AskTimerInterrupt _ -> (timerOn ...= False >> interrupt 0x08, xs)
              AskKeyInterrupt scancode -> (keyDown ...= scancode >> interrupt 0x09, xs)
            (now, later) = getFirst ints
        putMVar ivar later
        now
      when (ns' < cycles) cont

--------------------------------------------------------------------------------

cacheFile :: [Char]
cacheFile = "cache.txt"

adjustCache :: IO ()
adjustCache = do
  trace_ "adjust cache"
  ch' <- use' cache
  jumps <- use' cache2
  let p (Compiled True cs' ss' es' ds' _ _ _) = Just (cs', ss', es', ds')
      p _ = Nothing

  (cf, jumps') <- read <$> readFile cacheFile
  let cf' =
        ( foldr
            (uncurry IM.insert)
            cf
            [(i, (cs', ss', alter'' es', alter'' ds')) | (i, p -> Just (cs', ss', es', ds')) <- IM.toList ch'],
          IM.union jumps jumps'
        )
  cf' `deepseq` writeFile cacheFile (show cf')
  where
    alter'' :: Maybe Word16 -> Int
    alter'' (Just i) = fromIntegral i
    alter'' Nothing = -1

loadCache :: (Int -> BS.ByteString) -> IO ()
loadCache getInst = do
  trace_ "Loading cache..."
  (cf, jumps) <- readCache
  let fromIntegral' :: Int -> Maybe Word16
      fromIntegral' x | x == -1 = Nothing
      fromIntegral' x = Just $ fromIntegral x
      fromIntegral_ :: Int -> Word16
      fromIntegral_ = fromIntegral
  cf' <- cf `deepseq` do
    let ca = mempty :: Cache
    forM (IM.toList cf) $
      \( ip',
         ( fromIntegral_ -> cs',
           fromIntegral_ -> ss',
           fromIntegral' -> es',
           fromIntegral' -> ds'
           )
         ) -> (,) ip' <$> fetchBlock_' ca (disassemble . getInst) cs' ss' es' ds' (fromIntegral $ ip' - segAddr cs' 0)
  cache .%= IM.union (IM.fromList cf')
  cache2 .%= IM.unionWith IS.union jumps
  trace_' "ok"

type CacheFile = (IM.IntMap (Int, Int, Int, Int), Cache2)

readCache :: IO CacheFile
readCache = do
  let newCache = do
        writeFile cacheFile $ show (mempty :: CacheFile)
        return mempty
  b <- doesFileExist cacheFile
  if not b
    then newCache
    else do
      x <- readFile cacheFile
      case x `deepseq` reads x of
        [(v, "")] -> return v
        _ -> do
          putStrLn "outdated cache file deleted!"
          newCache
