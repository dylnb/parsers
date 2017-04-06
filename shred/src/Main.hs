{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parse
import Lexicon
import Pretty (prettyExp, pprettyCat)
import Lambda
import JSUtils

import Reflex.Dom
import Reflex.Dom.Class
import Web.KeyCode

import Control.Monad.IO.Class
import Data.List (intercalate, nubBy, sortOn)
import Data.Function (on)

-------------------------------------------------------------------------------
-- Auxuiliary functions
-------------------------------------------------------------------------------

confCut, parsCut :: Int
confCut = 5
parsCut = 5

updConfig :: String -> [Config] -> [Config]
updConfig q cs = [(ts, b ++ s) | (ts, b) <- cs, s <- tokenize lexicon q] >>= ctrees

drawStuff :: Either [Tree] [Config] -> IO ()
drawStuff = either dispTrees dispConfigs

truncDisp :: Either [Tree] [Config] -> Either [Tree] [Config]
truncDisp (Left ts) = Left . take parsCut . nubBy sameDen $ ts
  where sameDen = (==) `on` (nf . fst . sem)
truncDisp (Right cs) = Right . take confCut . nubBy sameDen $ sortOn (length . fst) cs
  where sameDen = (==) `on` (map (nf . fst . sem) . fst)

-------------------------------------------------------------------------------
-- Main layout
-------------------------------------------------------------------------------

main :: IO ()
main = mainWidget $ do

  -- page header
  el "h1" (text "parse demo")

  -- query input box
  (inc, parses, confs) <- divClass "page-wrap" $ do
    divClass "column-main" $ do
      rec dynTB <- mapDyn termEntry dynInc
          termBox <- dyn dynTB
          evQuery <- switchPromptly never termBox
          dynInc <- elAttr "label" ("id" =: "inc-box") $ do
            checkbox False def >>= \cb -> text "Incremental" >> return (value cb)
      dynConfig <- foldDyn updConfig [([],[])] $ gate (current dynInc) evQuery
      dynParse <- holdDyn [] $ parse lexicon <$> gate (not <$> current dynInc) evQuery
      return (dynInc, dynParse, dynConfig)

  -- parse display area
  elAttr "div" ("id" =: "cy") $ do 
    let evDisps = leftmost [Left <$> updated parses, Right <$> updated confs]
        truncDisps = truncDisp <$> evDisps 
    dynDisps <- widgetHold (return $ Left []) $ parseDivs <$> truncDisps
    performEvent_ $ (liftIO . drawStuff) <$> updated dynDisps

  -- information display area
  elAttr "div" ("id" =: "infocol") $ do
    el "h2" (text "Lexicon")
    lexDiv
    el "h2" (text "Reductions")
    reduxDiv
    
-------------------------------------------------------------------------------
-- Query widget
-------------------------------------------------------------------------------

termEntry :: MonadWidget t m => Bool -> m (Event t String)
termEntry inc = divClass "sh" $ do
  let termBoxAttrs =
        [ "placeholder" =: "Enter a term"
        -- , "name" =: "newTerm"
        , "id" =: "search-wrapper"
        ]
      magicKey x = keyCodeLookup x == (if inc then Space else Enter)
  rec let newValueEntered = ffilter magicKey (_textInput_keypress termBox)
      termBox <- divClass "query" $
        textInput $ def & setValue .~ ("" <$ newValueEntered)
                        & attributes .~ constDyn (mconcat termBoxAttrs)
  return $ tag (current $ _textInput_value termBox) newValueEntered

-------------------------------------------------------------------------------
-- Parse widgets
-------------------------------------------------------------------------------

parseDivs :: MonadWidget t m => Either [a] [b] -> m (Either [a] [b])
parseDivs = 
  either (\ts -> createDivs parsCut ts >> return (Left ts))
         (\cs -> createDivs confCut cs >> return (Right cs))
  where createDivs cut is = snd (foldr (accum cut) (0 :: Int, blank) is)
        accum cut _ (n, divs)
          | n < cut   = let div' = elAttr "div" ("id" =: ("parse" ++ show n)) blank
                            n' = n + 1
                         in (n', divs >> div' >> elClass "hr" "treebreak" blank)
          | otherwise = (n, divs)

-------------------------------------------------------------------------------
-- Lexicon widget
-------------------------------------------------------------------------------

lexDiv :: MonadWidget t m => m ()
lexDiv = elAttr "div" ("id" =: "lex") $ foldr (\e es -> e >> es) blank els
  where printLex (wd, tts) = wd ++ ":: " ++ intercalate "," [pprettyCat (snd tt) | tt <- tts]
        els =  map (elAttr "span" ("class" =: "lex-item") . text . printLex)  lxcn

-------------------------------------------------------------------------------
-- Reductions widget
-------------------------------------------------------------------------------

reduxDiv :: MonadWidget t m => m ()
reduxDiv = elAttr "div" ("id" =: "reductions") $ text ("(try clicking on a node)")
