{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module DeBruijn
  ( convExpM,
    spTrans,
  )
where

import qualified Data.IntMap.Strict as IM
import Data.Word
import Edsl
import Unsafe.Coerce

--------------------------

data Layout :: List * -> List * -> * where
  EmptyLayout :: Layout env 'Nil
  PushLayout :: Layout env env' -> Var env t -> Layout env ('Con t env')

size :: Layout env env' -> Int
size EmptyLayout = 0
size (PushLayout lyt _) = size lyt + 1

inc :: Layout env env' -> Layout ('Con t env) env'
inc EmptyLayout = EmptyLayout
inc (PushLayout lyt ix) = PushLayout (inc lyt) (VarS ix)

prjIx :: Int -> Layout env env' -> Var env t
prjIx _ EmptyLayout = error "Convert.prjIx: internal error"
prjIx 0 (PushLayout _ ix) = unsafeCoerce ix
prjIx n (PushLayout l _) = prjIx (n - 1) l

convExpM :: ExpM a -> EExpM 'Nil a
convExpM = convM EmptyLayout
  where
    convM :: forall e a. Layout e e -> ExpM a -> EExpM e a
    convM lyt =
      foldExpM
        (conv lyt)
        (\(FunM g) -> DBM $ convM (inc lyt `PushLayout` VarZ) $ g $ Var $ Co $ size lyt)
        (\p a g -> Set (convPart lyt p) (conv lyt a) (convM lyt g))
        (\i cs ip -> Jump' ((\(x, y, z) -> (x, IM.map (convM lyt) y, convM lyt z)) <$> i) (conv lyt cs) (conv lyt ip))
        (SelfJump . convM lyt)
        (\ip m -> Loc ip (convM lyt m))
    conv :: forall a e. Layout e e -> Exp a -> EExp e a
    conv lyt =
      foldExp
        (\(Fun convM') -> DB $ conv (inc lyt `PushLayout` VarZ) $ convM' $ Var $ Co $ size lyt)
        (\(Co sz) -> Var (prjIx (size lyt - sz - 1) lyt))
        (Get . convPart lyt)
    convPart :: Layout e e -> Part_ Exp a -> Part_ (EExp e) a
    convPart lyt = mapPart (conv lyt)

----------------------------

lift' :: EExp e a -> EExp ('Con x e) a
lift' = lift'' VarS

incV :: (Var e a -> Var f a) -> Var ('Con x e) a -> Var ('Con x f) a
incV _ VarZ = VarZ
incV f (VarS x) = VarS $ f x

lift'' :: forall e e'. (forall x. Var e x -> Var e' x) -> forall a. EExp e a -> EExp e' a
lift'' gv = foldExp (\(DB x) -> DB $ lift'' (incV gv) x) (Var . gv) (Get . mapPart (lift'' gv))

------------------------------

spTrans :: EExpM e a -> EExpM e a
spTrans = spTr IM.empty (Get SP)
  where
    spTrE :: forall e a. EExp e Word16 -> EExp e a -> EExp e a
    spTrE sp = foldExp (\(DB c) -> DB $ spTrE (lift' sp) c) Var get
      where
        get :: Part_ (EExp e) x -> EExp e x
        get SP = sp
        get x = Get x
    spTr :: forall e a. IM.IntMap (EExp e Word16) -> EExp e Word16 -> EExpM e a -> EExpM e a
    spTr im sp = foldExpM (spTrE sp) (\(DBM c) -> DBM $ spTr (IM.map lift' im) (lift' sp) c) set' jump selfjump loc
      where
        set' :: Part_ (EExp e) b -> EExp e b -> EExpM e x -> EExpM e x
        set' SP (add_ -> (i, Get SP)) c = spTr im (add (C i) sp) c
        set' SP v c = Set SP (spTrE sp v) (spTr im (Get SP) c)
        set' p v c = Set p (spTrE sp v) (spTr im sp c)
        jump :: JumpInfo (EExpM e) -> EExp e Word16 -> EExp e Word16 -> EExpM e Jump'
        jump (Right (x, y, z)) cs ip = Jump' (Right (x, IM.map (spTr im sp) y, spTr im sp z)) (spTrE sp cs) (spTrE sp ip)
        jump (Left r) cs ip = case sp of
          Get SP -> cont
          sp' -> Set SP sp' cont
          where
            cont = Jump' (Left r) cs ip
        selfjump :: EExpM e () -> EExpM e Jump' -> Word16 -> EExpM e Jump'
        selfjump prep m ip = case compatible (im IM.! fromIntegral ip) sp of
          Nothing -> m
          Just prep' -> SelfJump (prep' prep) m ip
        loc :: Word16 -> EExpM e Jump' -> EExpM e Jump'
        loc ip m = Loc ip $ spTr (IM.insert (fromIntegral ip) sp im) sp m

compatible :: EExp e Word16 -> EExp e Word16 -> Maybe (EExpM e () -> EExpM e ())
compatible (Get SP) (Get SP) = Just id
compatible _ _ = Nothing

add_ :: Num a => Exp_ v c a -> (a, Exp_ v c a)
add_ (Add (C j) x) = (j, x)
add_ v = (0, v)
