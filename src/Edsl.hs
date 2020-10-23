{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Edsl where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.Word
import Helper
import Prelude

----------------------------------------

data Part_ e a where
  Heap8 :: e Int -> Part_ e Word8
  Heap16 :: e Int -> Part_ e Word16
  AL, AH, BL, BH, CL, CH, DL, DH :: Part_ e Word8
  AX, BX, CX, DX, SI, DI, BP, SP :: Part_ e Word16
  Es, Ds, Ss, Cs :: Part_ e Word16
  CF, PF, ZF, SF, IF, DF, OF :: Part_ e Bool
  Flags :: Part_ e Word16
  DXAX :: Part_ e Word32

instance Eq (Part_ e a) where
  AL == AL = True
  BL == BL = True
  CL == CL = True
  DL == DL = True
  AH == AH = True
  BH == BH = True
  CH == CH = True
  DH == DH = True
  AX == AX = True
  BX == BX = True
  CX == CX = True
  DX == DX = True
  SI == SI = True
  DI == DI = True
  BP == BP = True
  SP == SP = True
  Es == Es = True
  Ds == Ds = True
  Ss == Ss = True
  Cs == Cs = True
  CF == CF = True
  PF == PF = True
  ZF == ZF = True
  SF == SF = True
  IF == IF = True
  DF == DF = True
  OF == OF = True
  Flags == Flags = True
  DXAX == DXAX = True
  _ == _ = False -- TODO

mapPart :: (forall a. e a -> f a) -> Part_ e b -> Part_ f b
mapPart f = \case
  Heap8 a -> Heap8 (f a)
  Heap16 a -> Heap16 (f a)
  AL -> AL
  BL -> BL
  CL -> CL
  DL -> DL
  AH -> AH
  BH -> BH
  CH -> CH
  DH -> DH
  AX -> AX
  BX -> BX
  CX -> CX
  DX -> DX
  SI -> SI
  DI -> DI
  BP -> BP
  SP -> SP
  Es -> Es
  Ds -> Ds
  Ss -> Ss
  Cs -> Cs
  CF -> CF
  PF -> PF
  ZF -> ZF
  SF -> SF
  IF -> IF
  DF -> DF
  OF -> OF
  Flags -> Flags
  DXAX -> DXAX

--------------------------------------------------------------------------------

data Exp_ (v :: * -> *) (c :: * -> * -> *) a where
  Var :: v a -> Exp_ v c a
  C :: a -> Exp_ v c a
  Let :: Exp_ v c a -> c a b -> Exp_ v c b
  Tuple :: Exp_ v c a -> Exp_ v c b -> Exp_ v c (a, b)
  Fst :: Exp_ v c (a, b) -> Exp_ v c a
  Snd :: Exp_ v c (a, b) -> Exp_ v c b
  Iterate :: Exp_ v c Int -> c a a -> Exp_ v c a -> Exp_ v c a
  If :: Exp_ v c Bool -> Exp_ v c a -> Exp_ v c a -> Exp_ v c a
  Get :: Part_ (Exp_ v c) a -> Exp_ v c a
  Eq :: Eq a => Exp_ v c a -> Exp_ v c a -> Exp_ v c Bool
  Add, Mul :: (Num a, Eq a) => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
  QuotRem :: Integral a => Exp_ v c a -> Exp_ v c a -> Exp_ v c (a, a)
  And, Or, Xor :: Bits a => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
  Not, ShiftL, ShiftR, RotateL, RotateR :: Bits a => Exp_ v c a -> Exp_ v c a
  EvenParity :: Exp_ v c Word8 -> Exp_ v c Bool
  Convert :: (Integral a, Num b) => Exp_ v c a -> Exp_ v c b
  SegAddr :: Exp_ v c Word16 -> Exp_ v c Word16 -> Exp_ v c Int

unTup :: Exp_ v c (a, b) -> (Exp_ v c a, Exp_ v c b)
unTup x = (fst' x, snd' x)

pattern Neg ::
  forall (v :: * -> *) (c :: * -> * -> *) a.
  () =>
  (Num a, Eq a) =>
  Exp_ v c a ->
  Exp_ v c a
pattern Neg a = Mul (C (-1)) a

pattern Sub ::
  forall (v :: * -> *) (c :: * -> * -> *) a.
  () =>
  (Num a, Eq a) =>
  Exp_ v c a ->
  Exp_ v c a ->
  Exp_ v c a
pattern Sub a b = Add a (Neg b)

add :: (Num a, Eq a) => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
add (C c) (C c') = C $ c + c'
add (C c) (Add (C c') v) = add (C $ c + c') v
add (C 0) x = x
add a (C c) = add (C c) a
add (Neg a) (Neg b) = Neg (add a b)
add a b = Add a b

sub :: (Num a, Eq a) => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
sub a b = add a (mul (C $ -1) b)

mul :: (Num a, Eq a) => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
mul (C c) (C c') = C $ c * c'
mul (C c) (Mul (C c') x) = mul (C $ c * c') x -- signed modulo multiplication is also associative
mul (C 0) _ = C 0
mul (C 1) x = x
mul x (C c) = mul (C c) x
mul a b = Mul a b

and' :: (Num a, Bits a) => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
and' (C 0) _ = C 0
and' (C c) (C c') = C $ c .&. c'
and' a b = And a b

or' :: Bits a => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
or' (C c) (C c') = C $ c .|. c'
or' a b = Or a b

xor' :: Bits a => Exp_ v c a -> Exp_ v c a -> Exp_ v c a
xor' (C c) (C c') = C $ xor c c'
xor' a b = Xor a b

and'' :: Exp_ v c Bool -> Exp_ v c Bool -> Exp_ v c Bool
and'' (C False) _ = C False
and'' (C True) b = b
and'' a b = And a b

not' :: Bits a => Exp_ v c a -> Exp_ v c a
not' (C c) = C $ complement c
not' b = Not b

eq' :: Eq a => Exp_ v c a -> Exp_ v c a -> Exp_ v c Bool
eq' (C c) (C c') = C $ c == c'
eq' a b = Eq a b

fst' :: Exp_ v c (a, b) -> Exp_ v c a
fst' (Tuple a _) = a
fst' e = Fst e

snd' :: Exp_ v c (a, b) -> Exp_ v c b
snd' (Tuple _ b) = b
snd' e = Snd e

iterate' ::
  Exp_ Co Fun Int ->
  (Exp_ Co Fun a -> Exp_ Co Fun a) ->
  Exp_ Co Fun a ->
  Exp_ Co Fun a
iterate' (C 0) _ e = e
iterate' (C 1) f e = f e
iterate' n f e = Iterate n (Fun f) e

convert :: (Num b, Integral a) => Exp_ v c a -> Exp_ v c b
convert (C a) = C $ fromIntegral a
convert e = Convert e

highBit :: (Integral a, Bits a) => Exp_ v c a -> Exp_ v c Bool
highBit = Convert . RotateL

setHighBit :: (Num a, Bits a) => Exp_ v c Bool -> Exp_ v c a -> Exp_ v c a
setHighBit c = Or (RotateR $ Convert c)

segAddr_ :: Exp_ v c Word16 -> Exp_ v c Word16 -> Exp_ v c Int
segAddr_ (C s) (C o) = C $ segAddr s o
segAddr_ seg off = SegAddr seg off

{-# INLINE foldExp #-}
foldExp ::
  forall v v' c c' a.
  (forall x y. c x y -> c' x y) ->
  (forall x. v x -> Exp_ v' c' x) ->
  (forall x. Part_ (Exp_ v c) x -> Exp_ v' c' x) ->
  Exp_ v c a ->
  Exp_ v' c' a
foldExp tr var get = f
  where
    f :: Exp_ v c x -> Exp_ v' c' x
    f = \case
      Var c -> var c
      Get x -> get x
      C c -> C c
      Fst p -> Fst $ f p
      Snd p -> Snd $ f p
      Tuple a b -> Tuple (f a) (f b)
      If a b c -> If (f a) (f b) (f c)
      Let e x -> Let (f e) (tr x)
      Iterate n g c -> Iterate (f n) (tr g) (f c)
      Eq a b -> Eq (f a) (f b)
      Add a b -> add (f a) (f b)
      Mul a b -> mul (f a) (f b)
      And a b -> And (f a) (f b)
      Or a b -> Or (f a) (f b)
      Xor a b -> Xor (f a) (f b)
      SegAddr a b -> SegAddr (f a) (f b)
      QuotRem a b -> QuotRem (f a) (f b)
      Not a -> Not (f a)
      ShiftL a -> ShiftL (f a)
      ShiftR a -> ShiftR (f a)
      RotateL a -> RotateL (f a)
      RotateR a -> RotateR (f a)
      EvenParity a -> EvenParity (f a)
      Convert a -> Convert (f a)

instance Eq a => Eq (Exp_ v c a) where
  C a == C b = a == b
  Get a == Get b = a == b
  _ == _ = False -- TODO

------------------------------------ HOAS

newtype Co a = Co Int

newtype Fun a b = Fun {getFun :: Exp a -> Exp b}

type Exp = Exp_ Co Fun

type Part = Part_ Exp

---------------------- DeBruijn

data List a = Con a (List a) | Nil

data Var :: List * -> * -> * where
  VarZ :: Var ( 'Con a e) a
  VarS :: Var e a -> Var ( 'Con b e) a

newtype DB e a b = DB {getDB :: EExp ( 'Con a e) b}

type EExp e = Exp_ (Var e) (DB e)

--------------------------------------------------------------------------------

data Jump' = JumpAddr Word16 Word16

type JumpInfo e = Either Bool ((Word16, Word16), IM.IntMap (e Jump'), e Jump')

data ExpM_ (e :: * -> *) (c :: * -> * -> *) a where
  Ret :: e a -> ExpM_ e c a
  Set :: Part_ e b -> e b -> ExpM_ e c a -> ExpM_ e c a
  Jump' :: JumpInfo (ExpM_ e c) -> e Word16 -> e Word16 -> ExpM_ e c Jump'
  SelfJump :: ExpM_ e c () -> ExpM_ e c Jump' -> Word16 -> ExpM_ e c Jump'
  Call :: e Word16 -> e Word16 -> Int -> ExpM_ e c Jump' -> ExpM_ e c Jump'
  -- constrained let type (with more efficient interpretation)
  LetMC :: e b -> c b () -> ExpM_ e c a -> ExpM_ e c a
  LetM :: e b -> c b a -> ExpM_ e c a
  IfM :: e Bool -> ExpM_ e c a -> ExpM_ e c a -> ExpM_ e c a
  IfM' :: e Bool -> ExpM_ e c a -> ExpM_ e c a -> c a b -> ExpM_ e c b
  Replicate :: Integral b => e b -> e Bool -> ExpM_ e c () -> c b a -> ExpM_ e c a
  Input :: e Word16 -> c Word16 a -> ExpM_ e c a
  Output :: e Word16 -> e Word16 -> ExpM_ e c a -> ExpM_ e c a
  Loc :: Word16 -> ExpM_ e c Jump' -> ExpM_ e c Jump' -- location info (ip)
  Trace :: String -> ExpM_ e c a -> ExpM_ e c a

class CC c where
  type Ex c :: * -> *
  cid :: c a a
  (.>=>) :: c a b -> (b -> ExM c x) -> c a x

  ret :: c a (Ex c a)

type ExM c = ExpM_ (Ex c) c

instance (CC c, Ex c ~ Exp_ v c') => Functor (ExpM_ (Exp_ v c') c) where
  fmap = liftM

instance (CC c, Ex c ~ Exp_ v c') => Applicative (ExpM_ (Exp_ v c') c) where
  pure = return
  (<*>) = ap

instance (CC c, Ex c ~ Exp_ v c') => Monad (ExpM_ (Exp_ v c') c) where
  return a = Ret (C a)
  expr >>= f = case expr of
    Ret (C x) -> f x
    Ret x -> LetM x $ cid .>=> f
    Set a b e -> Set a b $ e >>= f
    LetMC e x g -> LetMC e x $ g >>= f
    LetM e g -> LetM e $ g .>=> f
    IfM b x y -> IfM b (x >>= f) (y >>= f)
    IfM' b x y g -> IfM' b x y $ g .>=> f
    Replicate n b m g -> Replicate n b m $ g .>=> f
    Input e g -> Input e $ g .>=> f
    Output a b e -> Output a b $ e >>= f
    Call {} -> error "Call >>= "
    Jump' {} -> error "Jump' >>="
    SelfJump {} -> error "SelfJump >>="
    Loc {} -> error "Loc >>="
    Trace s e -> Trace s $ e >>= f

set :: Monad (ExpM_ e c) => Part_ e b -> e b -> ExpM_ e c ()
set x y = Set x y (return ())

ifM ::
  Exp_ v c1 Bool ->
  ExpM_ (Exp_ v c1) c2 a ->
  ExpM_ (Exp_ v c1) c2 a ->
  ExpM_ (Exp_ v c1) c2 a
ifM (C c) a b = if c then a else b
ifM x a b = IfM x a b

ifM' ::
  CC c1 =>
  Exp_ v c2 Bool ->
  ExpM_ (Exp_ v c2) c1 b ->
  ExpM_ (Exp_ v c2) c1 b ->
  ExpM_ (Exp_ v c2) c1 b
ifM' (C c) a b = if c then a else b
ifM' x a b = IfM' x a b cid

letM ::
  (CC c1, Ex c1 ~ Exp_ v c2) =>
  Exp_ v c2 b ->
  ExpM_ (Exp_ v c2) c1 (Exp_ v c2 b)
letM x@(C _) = return x
letM x = LetM x ret

letMC ::
  (CC c1, Ex c1 ~ Exp_ v c2) =>
  Exp_ v c2 b ->
  (Exp_ v c2 b -> ExpM_ (Exp_ v c2) c1 ()) ->
  ExpM_ (Exp_ v c2) c1 ()
letMC (C c) f = f (C c)
letMC x f = LetMC x (ret .>=> f) (return ())

letMC' ::
  (CC c1, Ex c1 ~ Exp_ v c2) =>
  Exp_ v c2 b1 ->
  (Exp_ v c2 b1 -> ExpM_ (Exp_ v c2) c1 b2) ->
  ExpM_ (Exp_ v c2) c1 b2
letMC' x f = letM x >>= f

output :: Monad (ExpM_ e c) => e Word16 -> e Word16 -> ExpM_ e c ()
output a b = Output a b (return ())

{-# INLINE foldExpM #-}
foldExpM ::
  forall e e' c c' a.
  (forall x. e x -> e' x) ->
  (forall x y. c x y -> c' x y) ->
  (forall x b. Part_ e b -> e b -> ExpM_ e c x -> ExpM_ e' c' x) ->
  (JumpInfo (ExpM_ e c) -> e Word16 -> e Word16 -> ExpM_ e' c' Jump') ->
  (ExpM_ e c () -> ExpM_ e' c' Jump' -> Word16 -> ExpM_ e' c' Jump') ->
  (Word16 -> ExpM_ e c Jump' -> ExpM_ e' c' Jump') ->
  ExpM_ e c a ->
  ExpM_ e' c' a
foldExpM q tr set' jump selfjump loc = k
  where
    k :: ExpM_ e c x -> ExpM_ e' c' x
    k = \case
      LetM e g -> LetM (q e) (tr g)
      Input e g -> Input (q e) (tr g)
      IfM a b c -> IfM (q a) (k b) (k c)
      IfM' a b c d -> IfM' (q a) (k b) (k c) (tr d)
      Replicate n b a g -> Replicate (q n) (q b) (k a) (tr g)
      Jump' i cs ip -> jump i cs ip
      SelfJump m m' ip -> selfjump m (k m') ip
      Call a b i g -> Call (q a) (q b) i (k g)
      Set p a g -> set' p a g
      Output a b c -> Output (q a) (q b) (k c)
      Ret x -> Ret (q x)
      Loc s x -> loc s x
      Trace s x -> Trace s (k x)
      _ -> undefined

---------------------- HOAS

newtype FunM a b = FunM {getFunM :: Exp a -> ExpM b}

instance CC FunM where
  type Ex FunM = Exp
  FunM f .>=> g = FunM $ f >=> g
  ret = FunM return
  cid = FunM Ret

type ExpM = ExM FunM

-------------------------- DeBruijn

newtype DBM e a b = DBM {getDBM :: EExpM ( 'Con a e) b}

type EExpM e = ExpM_ (EExp e) (DBM e)
