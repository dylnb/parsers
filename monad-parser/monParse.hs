{-- to add: semantics, binding[?] --}
import           Data.Char
import           Data.List

data Type =
 E | T | N | V |                  -- atomic cats
 M Type |                         -- monads
 FS Type Type | BS Type Type |    -- {/ , \}
 FF Type Type | BB Type Type |    -- {//,\\}
 X                                -- islands
  deriving (Show, Eq)

data Tree = Atom String | Bin String TTree TTree | Phase Tree
  deriving (Show, Eq)

type TTree = (Tree, [Type])
type Sentence = [TTree]

sentence :: String -> Sentence
sentence = map lookup . words . concatMap spaceSymb
  where lookup w = (Atom w, lexicon w)
        spaceSymb c = if isAlphaNum c then [c] else " "++[c]++" "
          -- parse symbols separately, regardless of whitespace

-- lexicon
--
lexicon :: String -> [Type]
lexicon w
  | w == "dylan" = [E]
  | w == "boy" = [N]
  | w == "elk" = [N]
  | w == "binoculars" = [N]
  | w == "left" = [BS E T]
  | w == "saw" = [FS (BS E T) E]
  | w == "with" = [FS (BS (BS E T) (BS E T)) E, FS (BS N N) E]
  | w == "thought" = [FS (BS E T) T]
  | w == "the" = [FS E N]
  | w == "if" = [FS (FS (M T) (M T)) $ M T]
  | w == "and" = [FS (BS T T) T]
  | w == "someone" = [M E]
  | w == "everyone" = [FF (M T) (BB E $ M T)]
  | w == "every" = [FF (FF (M T) (BB E $ M T)) (BB (FS T N) $ M T)]
  | w == "some" = [FF (M E) (BB (FS T N) $ M T)]
  | w == "[" = [X]
  | w == "]" = [X]

-- parsing
--
ttrees :: Sentence -> [TTree]
ttrees [] = []
ttrees [t] = [t]
ttrees ts =
  [t | (ls,rs) <- splits ts,
       l <- ttrees ls, r <- ttrees rs,
       t <- addLowers $ combine l r]
                                      -- break in half multiple ways,
                                      -- try to combine the pieces,
                                      -- add the lowers, then the joins
  ++
  [(Phase (Bin w t1 t2), [s]) |
    x <- snd (head ts), y <- snd (last ts), x == X, y == X,
    (ls,rs) <- splits . init . tail $ ts, -- ditch the punctuation
    l <- ttrees ls, r <- ttrees rs,
    (Bin w t1 t2, ss) <- addLowers $ combine l r,
    s <- ss, evaluated s]

combine :: TTree -> TTree -> [TTree]
combine (a,ts) (b,ss) =
  [(Bin -- L R
    "FA" (a, [FS t1 t2]) (b, [s]), [t1]) |
      FS t1 t2 <- ts, s <- ss, t2 == s]
  ++
  [(Bin -- R L
    "BA" (a, [t]) (b, [BS s1 s2]), [s2]) |
      t <- ts, BS s1 s2 <- ss, t == s1]
  ++
  [(Bin -- L $ unit R
    "FA$_\\eta$" (a, [FS t1 t2]) (b, [s]), [t1]) |
      FS t1 t2 <- ts, s <- ss, t2 == M s]
  ++
  [(Bin -- R $ unit L
    "BA$_\\eta$" (a, [t]) (b, [BS s1 s2]), [s2]) |
      t <- ts, BS s1 s2 <- ss, M t == s1]
  ++
  [(Bin -- \k -> L (\x -> k $ combine x R)
    ("SL("++w++")") (a, [FF x (BB y z)]) (b, [u]), [FF x (BB v z)]) |
      FF x (BB y z) <- ts, u <- ss,
      (Bin w t1 t2, rs) <- combine (a, [y]) (b, [u]), v <- rs]
  ++
  [(Bin -- \k -> R (\x -> k $ combine L x)
    ("SR("++w++")") (a, [u]) (b, [FF x (BB y z)]), [FF x (BB v z)]) |
      u <- ts,
      FF x (BB y z) <- ss,
      (Bin w t1 t2, rs) <- combine (a, [u]) (b, [y]), v <- rs]
  ++
  [(Bin -- \k -> L >>= \x -> k $ combine x R
    ("$\\star$L("++w++")") (a, [M t]) (b, [s]), [v]) |
      M t <- ts, s <- ss,
      (Bin w t1 t2, rs) <- combine (a, [FF x (BB t x) | x <- returnTypes])
      (b, [s]), v <- rs]
  ++
  [(Bin -- \k -> R >>= \x -> k $ combine L x
    ("$\\star$R("++w++")") (a, [t]) (b, [M s]), [v]) |
      t <- ts, M s <- ss,
      (Bin w t1 t2, rs) <- combine (a, [t])
      (b, [FF x (BB s x) | x <- returnTypes]), v <- rs]

parse :: String -> [TTree]
parse = ttrees . sentence

-- helper functions for parsing
--
howDeep :: Int
howDeep = 5
-- wow: the way things are set up guarantees you don't climb higher
-- in the type hierarchy than you need to....?

enumTypes :: Int -> [Type]
enumTypes 1 = [M T]
enumTypes n = enumTypes (n-1) ++ [M . last $ enumTypes (n-1)]

returnTypes :: [Type]
returnTypes = enumTypes howDeep

splits :: [TTree] -> [([TTree], [TTree])] -- return all cleavings of a list
splits ts = concat [f ts | f <- map modSplitAt [1..length ts - 1]]

modSplitAt :: Int -> [TTree] -> [([TTree], [TTree])]
-- using island boundaries to narrow the search space; not necessary
modSplitAt n [] = []
modSplitAt n xs = [(ys,zs) | (ys,zs) <- [splitAt n xs],
                             count "[" ys == count "]" ys,
                             count "[" zs == count "]" zs]
  where count x = length . filter ((== Atom x) . fst)

addLowers :: [TTree] -> [TTree]
addLowers ttrees =
  ttrees
  ++
  [(Bin -- incorporating all possible lowerings of the ttrees in question
    ("Lower("++w++")") t1 t2, [tl]) |
      (Bin w t1 t2, ts) <- ttrees, t <- ts, tl <- tail $ closeUnderLower [t]]

closeUnder :: Eq a => (a -> a) -> [a] -> [a]
closeUnder f ts = let new = union ts (map f ts) in
  if new == ts then ts else closeUnder f new

closeUnderLower :: [Type] -> [Type]
closeUnderLower = closeUnder lowerType

joinType :: Type -> Type
joinType t = case t of
  M (M a) -> M a
  _ -> t

lowerType :: Type -> Type
lowerType t = case t of
  FF a (BB b c) ->
    if M b == c || lowerType b == c then a else FF a (BB (lowerType b) c)
  _ -> t

evaluated :: Type -> Bool
evaluated t = case t of
  FF a b -> False
  FS a b -> evaluated a
  BS a b -> evaluated b
  BB a b -> evaluated b
  M a -> evaluated a
  _ -> True

-- parsing concluded
-- here's the pretty printer
-- writes a tex file with trees of the sentential parses in your working dir
--
prettyCat :: Type -> String -- printing cats
prettyCat cat = case cat of
  FS x y -> "(" ++ prettyCat x ++ " / " ++ prettyCat y ++ ")"
  BS x y -> "(" ++ prettyCat x ++ " \\backslash " ++ prettyCat y ++ ")"
  FF x y -> "(" ++ prettyCat x ++ " \\sslash " ++ prettyCat y ++ ")"
  BB x y -> "(" ++ prettyCat x ++ " \\bbslash " ++ prettyCat y ++ ")"
  M a -> "\\textsf{M}" ++ prettyCat a
  _ -> map toLower (show cat)

pprettyCat :: Type -> String -- dropping outer parens
pprettyCat thing = case xs of
  [] -> []
  _ -> if head xs == '(' && last xs == ')'
       then drop 1 $ init xs
       else xs
  where xs = prettyCat thing

pretty :: TTree -> [String]
pretty tree = case tree of
  (Atom x, ts) -> ["[{$"++pprettyCat t++"$} ["++x++"] ]" | t <- ts]
  (Bin w x y, ts) ->
    ["[{$\\textbf{\\textsf{"++w++"}} \\vdash " ++
     pprettyCat t++"$} "++a++" "++b++" ]"
      |
     t <- ts, a <- pretty x, b <- pretty y]
  (Phase t, ts) -> ["[$\\blacksquare$ "++a++" ]" | a <- pretty (t, ts)]

toForest :: String -> String
toForest = unlines . concatMap
  (map
    (\x -> "\\begin{forest}" ++
           "for tree={scale=.8}," ++
           "where n children=0{tier=word}{}\n" ++
           x ++
           "\n\\end{forest}\\\\\n")
  . pretty) . filter (prop . snd) . parse -- LOL

prop :: [Type] -> Bool
prop ts = case ts of
  [FF a b] -> False
  _ -> True

toParse :: String
toParse = "if [someone saw every elk with the binoculars] [dylan left]"

main :: IO ()
main = do
  putStr output
  writeFile "sandbox.tex" $ preamble++output++end
    where output = toForest toParse
          preamble =
            "\\documentclass{article}\n\\synctex=1\n" ++
            "\\usepackage[margin=.8in]{geometry}\n" ++
            "\\usepackage{forest,mathtools,newtxtext,newtxmath}\n" ++
            "\\newcommand\\bs\\backslash{}\n" ++
            "\\newcommand\\sslash{\\mathord{/\\mkern-5mu/}}\n" ++
            "\\newcommand\\bbslash{\\mathord{\\bs\\mkern-5.2mu\\bs}}\n" ++
            "\\begin{document}\n\n"
          end = "\\end{document}"