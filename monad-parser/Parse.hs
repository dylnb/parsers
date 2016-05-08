module Parse where

import Data.Char
import Data.List
import Lambda
import Control.Monad

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

tokenize :: (String -> [TypedTerm]) -> String -> [Sentence]
tokenize dict = sequence . map define . words . concatMap spaceSymb
  where define w = [Tree w t Empty | t <- dict w]
        spaceSymb c = if isAlphaNum c then [c] else " "++[c]++" "
          -- parse symbols separately, regardless of whitespace

-- parsing
--
ttrees :: Sentence -> [Tree]
ttrees [] = []
ttrees [t] = [t]
ttrees ts =
    do (ls, rs) <- splits ts
       l <- ttrees ls
       r <- ttrees rs
       addLowers $ combine l r
  ++
    do (_, X) <- return $ sem (head ts)
       (_, X) <- return $ sem (last ts)
       filter (evaluated . snd . sem) $ ttrees (init . tail $ ts)
  -- ++
  -- [(Phase (Bin w t1 t2), [s]) |
  --   x <- snd (head ts), y <- snd (last ts), x == X, y == X,
  --   (ls,rs) <- splits . init . tail $ ts, -- ditch the punctuation
  --   l <- ttrees ls, r <- ttrees rs,
  --   (Bin w t1 t2, ss) <- addLowers $ combine l r,
  --   s <- ss, evaluated s]

parse :: (String -> [TypedTerm]) -> String -> [Tree]
parse dict = tokenize dict >=> ttrees

-- helper functions for parsing
--
howDeep :: Int
howDeep = 5
-- wow: the way things are set up guarantees you don't climb higher
-- in the type hierarchy than you need to....?

returnTypes :: [Type]
returnTypes = iterate M (M T)

splits :: [a] -> [([a], [a])] -- return all cleavings of a list
splits ts = tail $ init $ zip (inits ts) (tails ts)
-- splits ts = concat [f ts | f <- map modSplitAt [1..length ts - 1]]

{--
modSplitAt :: Int -> [TTree] -> [([TTree], [TTree])]
-- using island boundaries to narrow the search space; not necessary
modSplitAt n [] = []
modSplitAt n xs = [(ys,zs) | (ys,zs) <- [splitAt n xs],
                             count "[" ys == count "]" ys,
                             count "[" zs == count "]" zs]
  where count x = length . filter ((== Atom x) . fst)
--}

addLowers :: [Tree] -> [Tree]
addLowers ttrs = ttrs ++
  do Tree rp (den, ty) (Bin t1 t2) <- ttrs
     tl <- tail $ closeUnderLower [ty]
     return $ Tree ("Lower(" ++ rp ++ ")") (den :@ unit, tl) (Bin t1 t2)

closeUnder :: Eq a => (a -> a) -> [a] -> [a]
closeUnder f ts = let new = union ts (map f ts) in
  if new == ts then ts else closeUnder f new

closeUnderLower :: [Type] -> [Type]
closeUnderLower = closeUnder lowerType

joinType :: Type -> Type
joinType (M (M a)) = M a
joinType t         = t

lowerType :: Type -> Type
lowerType (a ://: (b :\\: c))
  | M b == c          = a
  | lowerType b == c  = a
  | otherwise         = a ://: (lowerType b :\\: c)
lowerType t = t

evaluated :: Type -> Bool
evaluated (_ ://: _) = False
evaluated (a :/: _)  = evaluated a
evaluated (_ :\: b)  = evaluated b
evaluated (_ :\\: b) = evaluated b
evaluated (M a)      = evaluated a
evaluated _          = True

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
