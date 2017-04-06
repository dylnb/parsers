module Lexicon where

import Lambda
import Parse
import Pretty

-- lexicon
--
lexicon :: String -> [TypedTerm]
lexicon w = maybe (error "unknown word") id (lookup w lxcn)

lxcn :: [(String, [TypedTerm])]
lxcn = 
  [ ( "dylan"      , [(dylan', E)] )
  , ( "boy"        , [(boy', N)] )
  , ( "elk"        , [(elk', N)] )
  , ( "binoculars" , [(binos', N)] )
  , ( "left"       , [(left', E :\: T)] )
  , ( "saw"        , [(saw', (E :\: T) :/: E)] )
  , ( "with"       , [(with', ((E :\: T) :\: (E :\: T)) :/: E), (with', (N :\: N) :/: E)] )
  , ( "thought"    , [(thought', (E :\: T) :/: T)] )
  , ( "the"        , [(the', E :/: N)] )
  , ( "if"         , [(if', (M T :/: M T) :/: M T)] )
  , ( "and"        , [(and', (T :\: T) :/: T)] )
  , ( "someone"    , [(someone', M E)] )
  , ( "everyone"   , [(everyone', M T ://: (E :\\: M T))] )
  , ( "every"      , [(every', (M T ://: (E :\\: M T)) ://: ((T :/: N) :\\: M T))] )
  , ( "some"       , [(some', M E ://: ((T :/: N) :\\: M T))] )
  , ( "["          , [(undefined, X)] )
  , ( "]"          , [(undefined, X)] )
  ]

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

