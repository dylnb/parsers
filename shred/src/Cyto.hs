-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
-- {-# LANGUAGE DefaultSignatures #-}

module Cyto where

import Parse
import Lexicon

import Data.JSString (JSString, pack)
import qualified JavaScript.JSON.Types.Internal as JS
import JavaScript.JSON.Types.Class

foreign import javascript unsafe
  "var cy = cytoscape({\
    \container: document.getElementById('cy'),\
    \boxSelectionEnabled: false,\
    \autounselectify: true,\
    \layout: {\
    	\name: 'dagre'\
    \},\
    \style: [\
    	\{\
    		\selector: 'node',\
    		\style: {\
    			\'content': 'data(id)',\
    			\'text-opacity': 0.5,\
    			\'text-valign': 'center',\
    			\'text-halign': 'center',\
    			\'background-color': '#fff'\
    		\}\
    	\},\
    	\{\
    		\selector: 'edge',\
    		\style: {\
    			\'width': 2,\
    			\'target-arrow-shape': 'none',\
    			\'line-color': '#9dbaea',\
    			\'target-arrow-color': '#9dbaea',\
    			\'curve-style': 'haystack',\
    			\'haystack-radius': '1'\
    		\}\
    	\}\
    \],\
    \elements: {\
      \nodes: $1,\
      \edges: $2\
    \}\
   \});"
  cyto :: JS.Value -> JS.Value -> IO ()

ov = JS.objectValue . JS.object
sv = JS.stringValue . pack
av = JS.arrayValue . JS.arrayValueList
dv = JS.doubleValue

data TState = TState { nid :: Int, eid :: Int }

t2c :: Tree -> Int -> TState -> ([JS.Value], [JS.Value], TState)

t2c (Tree rp sm Empty) parent st =
  let node =
        ov [ ("data", ov [ ("id", sv $ "n" ++ show (nid st + 1)) ]) ]
      edge =
        ov [ ("data", ov [ ("id", sv $ "e" ++ show (eid st + 1))
                         , ("source", sv $ "n" ++ show parent)
                         , ("target", sv $ "n" ++ show (nid st + 1))
                         ]
             )
           ]

   in ([node], [edge], TState (nid st + 1) (eid st + 1))

t2c (Tree rp sm (Bin t1 t2)) parent st =
  let node =
        ov [ ("data", ov [ ("id", sv $ "n" ++ show (nid st + 1)) ]) ]
      edge =
        ov [ ("data", ov [ ("id", sv $ "e" ++ show (eid st + 1))
                         , ("source", sv $ "n" ++ show parent)
                         , ("target", sv $ "n" ++ show (nid st + 1))
                         ]
             )
           ]
      (lNodes, lEdges, lSt) = t2c t1 (parent + 1) (TState (nid st + 1) (eid st + 1))
      (rNodes, rEdges, rSt) = t2c t2 (parent + 1) (TState (nid lSt) (eid lSt))
   in (node : lNodes ++ rNodes, edge : lEdges ++ rEdges, TState (nid rSt) (eid rSt))

t2c (Tree rp sm (Phase t)) parent st =
  let node =
        ov [ ("data", ov [ ("id", sv $ "n" ++ show (nid st + 1)) ]) ]
      edge =
        ov [ ("data", ov [ ("id", sv $ "e" ++ show (eid st + 1))
                         , ("source", sv $ "n" ++ show parent)
                         , ("target", sv $ "n" ++ show (nid st + 1))
                         ]
             )
           ]
      (uNodes, uEdges, uSt) = t2c t (parent + 1) (TState (nid st + 1) (eid st + 1))
   in (node : uNodes, edge : uEdges, TState (nid uSt) (eid uSt))


dispTree :: String -> IO ()
dispTree s =
  let (nodes, edges, _) = t2c (head $ parse lexicon s) 0 (TState 0 0)
   in cyto (av nodes) (av edges)
