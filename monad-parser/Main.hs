module Main where

import Lambda
import Parse
import Pretty

-- lexicon
--
lexicon :: String -> [TypedTerm]
lexicon w
  | w == "dylan" = [(dylan', E)]
  | w == "boy" = [(boy', N)]
  | w == "elk" = [(elk', N)]
  | w == "binoculars" = [(binos', N)]
  | w == "left" = [(left', E :\: T)]
  | w == "saw" = [(saw', (E :\: T) :/: E)]
  | w == "with" = [(with', ((E :\: T) :\: (E :\: T)) :/: E), (with', (N :\: N) :/: E)]
  | w == "thought" = [(thought', (E :\: T) :/: T)]
  | w == "the" = [(the', E :/: N)]
  | w == "if" = [(if', (M T :/: M T) :/: M T)]
  | w == "and" = [(and', (T :\: T) :/: T)]
  | w == "someone" = [(someone', M E)]
  | w == "everyone" = [(everyone', M T ://: (E :\\: M T))]
  | w == "every" = [(every', (M T ://: (E :\\: M T)) ://: ((T :/: N) :\\: M T))]
  | w == "some" = [(some', M E ://: ((T :/: N) :\\: M T))]
  | w == "[" = [(undefined, X)]
  | w == "]" = [(undefined, X)]
  | otherwise = error "unknown word"

dylan',boy',elk',binos',left',saw' :: Exp String
dylan' = L (LE Dylan)
boy' = V "boy"
elk' = V "elk"
binos' = V "binos"
left' = V "left"
saw' = V "saw"
with' = V "with"
thought' = V "with"
the' = V "the"
if' = V "if"
and' = V "and"
someone' = V "smone"
everyone' = V "evone"
every' = V "every"
some' = V "some"



toParse :: String
toParse = "the boy saw someone"
-- toParse = "if [someone saw every elk with the binoculars] [dylan left]"

main :: IO ()
main = do
  putStr output
  writeFile "sandbox/sandbox.tex" $ preamble++output++end
    where output = toForest lexicon toParse
          preamble =
            "\\documentclass{article}\n\\synctex=1\n" ++
            "\\usepackage[margin=.8in]{geometry}\n" ++
            "\\usepackage{forest,mathtools,newtxtext,newtxmath}\n" ++
            "\\newcommand\\bs\\backslash{}\n" ++
            "\\newcommand\\sslash{\\mathord{/\\mkern-5mu/}}\n" ++
            "\\newcommand\\bbslash{\\mathord{\\bs\\mkern-5.2mu\\bs}}\n" ++
            "\\begin{document}\n\n"
          end = "\\end{document}"
