{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Lambda
import Pretty
import Lexicon
import Parse

import Data.Foldable (for_)
import Data.List (nubBy, sortOn)
import Data.Function (on)
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
  { quctx :: String
  , tmctx :: [Config]
  }

initState :: IState
initState = IState "" [([],[])]

type Repl a = HaskelineT (StateT IState IO) a
hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Lookup ( returns [Sentence] )
  let mod = tokenize lexicon (L.unpack source)

  -- Parse
  let tmctx' = [(ts, b <> s) | (ts, b) <- tmctx st, s <- mod] >>= ctrees

  -- Create the new environment
  let st' = st { tmctx = tmctx'
               , quctx = quctx st <> " " <> (L.unpack source)
               }

  -- Update the interpreter state
  when update (put st')

  liftIO (putStrLn ("\nQuery: " <> quctx st'))

  let sortedTms = sortOn (length . fst) (tmctx st')
      sameDen = (==) `on` (map (nf . fst . sem) . fst)
      quickTms  = take 3 . nubBy sameDen $ sortedTms

  -- Print new parse state
  for_ (reverse quickTms) $ liftIO . putStrLn . prettyConfig

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

-- :w command
word :: [String] -> Repl ()
word args =
  exec True (L.pack $ unwords args)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess


-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = map fst lxcn ++ [":load", ":quit", ":w"]
  return $ filter (isPrefixOf n) cmds

options :: [(String, [String] -> Repl ())]
options = [
    ("w"   , word)
  , ("load"   , load)
  -- , ("browse" , browse)
  , ("quit"   , quit)
  -- , ("type"   , Main.typeof)
  -- , ("tree"   , tree)
  -- , ("terms"  , terms)
  ]

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
     $ evalRepl "Shred> " cmd options completer pre

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

toParse :: String
-- toParse = "the boy saw everyone"
-- toParse = "someone saw everyone"
-- toParse = "[someone saw every elk with the binoculars]"
toParse = "if [someone saw everyone] [dylan left]"

start :: IO ()
start = do
  args <- getArgs
  case args of
    []      -> shell (return ())
    [fname] -> shell (load [fname])
    -- ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
    _ -> putStrLn "invalid arguments"
