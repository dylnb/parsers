module Pretty where

import Lambda
import Parse
import Bound
import Bound.Name
import Bound.Scope (bindings)
import Data.Char
import Data.List
import Data.Foldable

prettyCat :: Type -> String -- printing cats
prettyCat cat =
  case cat of
    x :/: y -> "(" ++ prettyCat x ++ " / " ++ prettyCat y ++ ")"
    x :\: y -> "(" ++ prettyCat x ++ " \\backslash " ++ prettyCat y ++ ")"
    x ://: y -> "(" ++ prettyCat x ++ " \\sslash " ++ prettyCat y ++ ")"
    x :\\: y -> "(" ++ prettyCat x ++ " \\bbslash " ++ prettyCat y ++ ")"
    M a -> "\\textsf{M}" ++ prettyCat a
    _ -> map toLower (show cat)

pprettyCat :: Type -> String -- dropping outer parens
pprettyCat thing = case xs of
  [] -> []
  _ -> if head xs == '(' && last xs == ')'
       then tail (init xs)
       else xs
  where xs = prettyCat thing

pretty :: Tree -> [String]
pretty (Tree rp (den, ty) Empty) =
  return $ "[{$" ++ pprettyCat ty ++ "$\\\\ $" ++ prettyExp (nf den) ++ "$} [" ++ rp ++ "] ]"
pretty (Tree rp (den, ty) (Bin t1 t2)) = do
  a <- pretty t1
  b <- pretty t2
  return $ "[{$\\textbf{\\textsf{" ++ rp ++ "}} \\vdash "
           ++ pprettyCat ty ++ "$\\\\ $" ++ prettyExp (nf den) ++ "$} "
           ++ a ++ " " ++ b ++ " ]"
pretty (Tree _ _ (Phase t1)) = do
  a <- pretty t1
  return $ "[$\\blacksquare$ " ++ a ++ " ]"

toForest :: (String -> [TypedTerm]) -> String -> String
toForest dict = unlines . concatMap
  (map
    (\x -> "\\begin{forest}" ++
           "for tree={scale=.8,align=center}," ++
           "where n children=0{tier=word}{}\n" ++
           x ++
           "\n\\end{forest}\\\\\n")
  . pretty) . filter (prop . snd . sem) . parse dict -- LOL


prop :: Type -> Bool
prop (_ ://: _) = False
prop _          = True

prettyPrec :: [String] -> Bool -> Int -> Exp String -> ShowS
prettyPrec _      d n (L l)      = showString (show l)
prettyPrec _      d n (V a)      = showString ("\\texttt{" ++ a ++ "}")
prettyPrec vs     d n (x :@ y)   = showParen d $ 
  case x of
    (Lam b) -> prettyPrec vs True n x . showString "\\, " . prettyPrec vs True n y
    _       -> prettyPrec vs False n x . showString "\\, " . prettyPrec vs True n y
prettyPrec (v:vs) d n (Lam b)    = showParen d $ 
  showString "\\lambda " . showString u . showString ".\\, "
    . prettyPrec vs False n (instantiate1Name (V $ "$" ++ u ++ "$") b)
  where u = case bindings b of
              [] -> v
              l  -> name $ head l
prettyPrec vs     d n (Let bs b) = showParen d $ 
  showString "let" .  foldr (.) id (zipWith showBinding xs bs) .
  showString " in " . indent . prettyPrec ys False n (inst b)
  where (xs,ys) = splitAt (length bs) vs
        inst = instantiateName (\n -> V (xs !! n))
        indent = showString ('\n' : replicate (n + 4) ' ')
        showBinding x b = indent . showString x . showString " = " . prettyPrec ys False (n + 4) (inst b)

prettyWith :: [String] -> Exp String -> String
prettyWith vs t = prettyPrec (filter (`notElem` toList t) vs) False 0 t ""

prettyExp :: Exp String -> String
prettyExp = prettyWith $ [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
