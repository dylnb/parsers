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
  show Dylan = "d"

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
  | Let [Scope (Name String Int) Exp a] (Scope (Name String Int) Exp a)
  deriving (Eq,Ord,Show,Read)

lambda :: String -> Exp String -> Exp String
lambda u b = Lam (abstract1Name u b)

unit :: Exp String
unit = V "return"

bind :: Exp String
bind = V "(>>=)"

let_ :: [(String, Exp String)] -> Exp String -> Exp String
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstractName (`elemIndex` map fst bs)

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
  traverse f (Let bs b) = Let <$> traverse (traverse f) bs <*> traverse f b

instance Monad Exp where
  return = V
  L l      >>= f = L l
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)
  Let bs b >>= f = Let (map (>>>= f) bs) (b >>>= f)

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
nf (Let bs b) = nf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)

{--}
nfTrace :: Monad m => (Exp a -> m ()) -> Exp a -> m (Exp a)
nfTrace k e@L{}   = return e
nfTrace k e@V{}   = return e
nfTrace k (Lam b) = (Lam . toScope) <$> nfTrace (k . Lam . toScope) (fromScope b)
nfTrace k (f :@ a) =
  let tracedF = whnfTrace (k . (:@ a)) f
      test = \exp -> case exp of
        Lam b -> csubst k a (Lam b) >>= nfTrace k
        f' -> liftM2 (:@) (nfTrace k f') (nfTrace k a)
  in tracedF >>= test
-- nfTrace (Let bs b) = nf (inst b)
--   where es = map inst bs
--         inst = instantiateName (es !!)

  
{--}
whnf :: Exp a -> Exp a
whnf e@L{}   = e
whnf e@V{}   = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1Name a b)
  f'    -> f' :@ a
whnf (Let bs b) = whnf (inst b)
  where es = map inst bs
        inst = instantiateName (es !!)

whnfTrace :: Monad m => (Exp a -> m ()) -> Exp a -> m (Exp a)
whnfTrace k e@L{}   = return e
whnfTrace k e@V{}   = return e
whnfTrace k e@Lam{} = return e
whnfTrace k (f :@ a) =
  let tracedF = whnfTrace (k . (:@ a)) f
      test = \exp -> case exp of
        Lam b -> csubst k a (Lam b) >>= whnfTrace k 
        f'    -> return (f' :@ a)
  in tracedF >>= test
-- whnfSteps (Let bs b) = whnf (inst b)
--   where es = map inst bs
--         inst = instantiateName (es !!)

{--}
infixr 0 !
(!) :: String -> Exp String -> Exp String
x ! t = lambda x t

fromLex :: [(String, Exp String)] -> Exp String -> Exp a
fromLex lexicon = fromJust . closed . let_ lexicon


--}

