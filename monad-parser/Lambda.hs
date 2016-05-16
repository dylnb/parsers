{-# LANGUAGE StandaloneDeriving, DeriveFunctor, DeriveFoldable #-}

module Lambda where

import Data.List (elemIndex)
import Data.Foldable hiding (notElem)
import Data.Maybe (fromJust)
import Data.Traversable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Prelude hiding (foldr,abs)
import Prelude.Extras
import Bound
import Bound.Name
import Bound.Scope (bindings)
import System.Exit

data Entity = Dylan deriving (Eq, Ord, Read)

instance Show Entity where
  show Dylan = "\\texttt{d}"

data L = LI Integer | LB Bool | LE Entity deriving (Eq, Ord, Read)

instance Show L where
  show (LI n) = show n
  show (LB b) = show b
  show (LE e) = show e

infixl 9 :@

data Exp a
  = L L
  | V a
  | Exp a :@ Exp a
  | Lam (Scope (Name String ()) Exp a)
  | Bind (Exp a) (Exp a)
  | Return (Exp a)
  deriving (Eq,Ord,Show,Read)

lambda :: String -> Exp String -> Exp String
lambda u b = Lam (abstract1Name u b)

unit :: Exp a
unit = fromJust . closed $ "x" ! Return (V"x")

bind :: Exp a -> Exp a -> Exp a
bind = Bind

deriving instance Functor Exp
deriving instance Foldable Exp

instance Applicative Exp where
  pure  = V
  (<*>) = ap

instance Traversable Exp where
  traverse f (L l)      = pure (L l)
  traverse f (V a)      = V <$> f a
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam e)    = Lam <$> traverse f e
  traverse f (Return x) = Return <$> traverse f x
  traverse f (Bind m k) = Bind <$> traverse f m <*> traverse f k

instance Monad Exp where
  return = V
  L l      >>= f = L l
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)
  Return x >>= f = Return (x >>= f)
  Bind m k >>= f = Bind (m >>= f) (k >>= f)

-- these 4 classes are needed to help Eq, Ord, Show and Read pass through Scope
instance Eq1 Exp      where (==#)      = (==)
instance Ord1 Exp     where compare1   = compare
instance Show1 Exp    where showsPrec1 = showsPrec
instance Read1 Exp    where readsPrec1 = readsPrec

csubst :: Monad m => (Exp a -> m ()) -> Exp a -> Exp a -> m (Exp a)
csubst k a f@(Lam b) = k (f :@ a) >> return (instantiate1Name a b)

{--}
nf :: Exp a -> Exp a
nf e@L{}   = e
nf e@V{}   = e
nf (Lam b) = Lam $ toScope $ nf $ fromScope b
nf (f :@ a) = case whnf f of
  Lam b -> nf (instantiate1Name a b)
  f' -> nf f' :@ nf a
nf (Return x) = Return (nf x)
nf (Bind (Return x) k) = (nf $ k :@ x)
nf (Bind (Bind m f) g) = nf $ Bind m (kcomp :@ f :@ g)
  where kcomp = fromJust . closed $ "f" ! "g" ! "x" ! Bind (V"f" :@ V"x") (V"g")
nf (Bind m k@(Lam b)) = case nf (fromScope b) of
  Return (V (B{})) -> nf m
  _ -> Bind (nf m) (nf k)
nf (Bind m k) = Bind (nf m) (nf k)

{--}
whnf :: Exp a -> Exp a
whnf e@L{}   = e
whnf e@V{}   = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1Name a b)
  f'    -> f' :@ a
whnf (Return x) = Return (whnf x)
whnf (Bind (Return x) k) = (whnf $ k :@ x)
whnf (Bind (Bind m f) g) = whnf $ Bind m (kcomp :@ f :@ g)
  where kcomp = fromJust . closed $ "f" ! "g" ! "x" ! Bind (V"f" :@ V"x") (V"g")
whnf (Bind m k@(Lam b)) = case fromScope b of
  Return (V (B{})) -> whnf m
  _ -> Bind (nf m) (nf k)
whnf (Bind m k) = Bind (whnf m) (whnf k)

{--}
infixr 0 !
(!) :: String -> Exp String -> Exp String
x ! t = lambda x t

-- fromLex :: [(String, Exp String)] -> Exp String -> Exp a
-- fromLex lexicon = fromJust . closed . let_ lexicon


--}

