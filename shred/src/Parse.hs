{-# LANGUAGE TupleSections #-}

module Parse where

import Data.Char
import Data.List
import Lambda
import Control.Monad
import Data.Function (on)
import Control.Arrow ((***))

data Type
  = E | T | N
  | M Type
  | Type :/: Type | Type :\: Type
  | Type ://: Type | Type :\\: Type
  | X
  deriving (Show, Eq)

data Tree = Tree {rep :: String, sem :: TypedTerm, sub :: SubTree} deriving (Show)
data SubTree = Empty | Bin Tree Tree | Phase Tree deriving (Show)

type Sentence = [Tree]
type TypedTerm = (Exp String, Type)
type Config = ([Tree], Sentence)

shift :: Config -> Config
shift c@(_, []) = c
shift (ts, b:buff) = (b:ts, buff)

step :: Config -> [Config]
step (t3:t2:t1:ts, buffer)
  | rep t3 == "]" && rep t1 == "[" && evaluated (snd (sem t2))  = [(t2:ts, buffer)]
step c@(t2:t1:ts, buffer) = [(t:ts, buffer) | t <- combine t1 t2 >>= addLower] ++ [shift c]
step c = [shift c]

ctrees :: Config -> [Config]
ctrees = \c -> do
  c' <- step c
  if on (==) (length *** length) c c' then return c' else ctrees c'
  
parse :: (String -> [TypedTerm]) -> String -> [Tree]
parse dict s = map (head . fst) $ filter finished tests
  where tests = ([],) <$> tokenize dict s >>= ctrees
        finished (ts, buff) = length ts == 1 && null buff

tokenize :: (String -> [TypedTerm]) -> String -> [Sentence]
tokenize dict = sequence . map define . words . concatMap spaceSymb
  where define w = [Tree w t Empty | t <- dict w]
        spaceSymb c = if isAlphaNum c then [c] else " " ++ c:" "

returnTypes :: [Type]
returnTypes = iterate M (M T)

addLower :: Tree -> [Tree]
addLower ttr@(Tree rp sm@(dn,ty) (Bin t1 t2))
  | ty == snd (lower sm) = [ttr]
  | otherwise            = [ttr, ttr {rep = "Lower("++rp++")", sem = lower sm}]
addLower ttr             = [ttr]

lower :: TypedTerm -> TypedTerm
lower (m, (a ://: (b :\\: c)))
    | M b == c          = (m :@ unit, a)
    | x == c            = (m :@ ("m" ! t), a)
    | otherwise         = ("k" ! m :@ ("m" ! V "k" :@ t), a ://: (x :\\: c))
  where (t, x) = lower (V "m", b)
lower t                 = t

evaluated :: Type -> Bool
evaluated (_ ://: _) = False
evaluated (a :/: _)  = evaluated a
evaluated (_ :\: b)  = evaluated b
evaluated (_ :\\: b) = evaluated b
evaluated (M a)      = evaluated a
evaluated _          = True

howDeep :: Int
howDeep = 5

combine :: Tree -> Tree -> [Tree]
combine t1@(Tree _ sem1 _) t2@(Tree _ sem2 _) = concat
  [ do (f, a :/: b) <- return sem1
       (x, c) <- return sem2
       guard $ c == b
       return $ Tree "FA" (f :@ x, a) (Bin t1 t2)

  , do (x, c) <- return sem1
       (f, b :\: a) <- return sem2
       guard $ c == b
       return $ Tree "BA" (f :@ x, a) (Bin t1 t2)

  , do (f, a :/: b) <- return sem1
       (x, c) <- return sem2
       guard $ b == M c
       return $ Tree "FA$_\\eta$" (f :@ (unit :@ x), a) (Bin t1 t2)

  , do (x, c) <- return sem1
       (f, b :\: a) <- return sem2
       guard $ b == M c
       return $ Tree "BA$_\\eta$" (f :@ (unit :@ x), a) (Bin t1 t2)

  , do (l, x ://: (y :\\: z)) <- return sem1
       (Tree rp (dn,ty) (Bin _ _)) <- combine (t1 {sem = (V "x", y)}) t2
       let sL = "k" ! l :@ ("x" ! V "k" :@ dn)
       return $ Tree ("SL("++rp++")") (sL, x ://: (ty :\\: z)) (Bin t1 t2)

  , do (r, x ://: (y :\\: z)) <- return sem2
       (Tree rp (dn,ty) (Bin _ _)) <- combine t1 (t2 {sem = (V "y", y)})
       let sR = "k" ! r :@ ("y" ! V "k" :@ dn)
       return $ Tree ("SR("++rp++")") (sR, x ://: (ty :\\: z)) (Bin t1 t2)

  , do (l, M t) <- return sem1
       rt <- take howDeep returnTypes
       let bt1 = t1 {sem = (bind :@ l, rt ://: (t :\\: rt))}
       (Tree rp (dn,ty) (Bin _ _)) <- combine bt1 t2
       return $ Tree ("$\\star$L("++rp++")") (dn,ty) (Bin t1 t2)

  , do (r, M t) <- return sem2
       rt <- take howDeep returnTypes
       let bt2 = t2 {sem = (bind :@ r, rt ://: (t :\\: rt))}
       (Tree rp (dn,ty) (Bin _ _)) <- combine t1 bt2
       return $ Tree ("$\\star$R("++rp++")") (dn,ty) (Bin t1 t2)
  ]
