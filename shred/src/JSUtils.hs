{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE OverloadedStrings #-}

module JSUtils where

import Parse
import Lexicon
import Pretty (prettyExp)
import Lambda

import Data.Maybe (fromJust)
import Control.Monad.Writer

import Data.JSString (JSString, pack)
import qualified JavaScript.JSON.Types.Internal as JS
import JavaScript.JSON.Types.Class
import GHCJS.Foreign.Callback
import GHCJS.Types
import GHCJS.Marshal.Pure

foreign import javascript unsafe
  "cy = cytoscape({\
    \container: document.getElementById('parse' + $1),\
    \boxSelectionEnabled: false,\
    \autounselectify: true,\
    \layout: { name: 'dagre' },\
    \style: [\
      \{\
        \selector: 'node',\
        \style: {\
          \'width': 'label',\
          \'height': 'label',\
          \'padding': '5px',\
          \'shape': 'rectangle',\
          \'content': 'data(txt)',\
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
          \'curve-style': 'haystack'\
        \}\
      \}\
    \],\
    \elements: {\
      \nodes: $2,\
      \edges: $3\
    \}\
  \}).fit().userZoomingEnabled(true).on('tap', 'node', function(e) {\
    \$4(parseInt(e.cyTarget.id().substring(1)));\
  \});"
  cyto :: Int -> JS.Value -> JS.Value -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "console.log($1)"
  consoleLog :: JSVal -> IO ()

foreign import javascript unsafe
  "document.getElementById('reductions').innerHTML = $1;"
  reduct :: JSVal -> IO ()

lkpNM :: NodeMap -> Int -> IO ()
lkpNM nm n = reduct . pToJSVal $ expList >>= \e -> "<p>" ++ prettyExp e ++ "</p>"
  where exp = fromJust $ lookup n nm 
        (e,c) = runWriter $ nfTrace (\x -> tell [x]) exp
        expList = c ++ [e]

nodemapCB :: NodeMap -> IO (Callback (JSVal -> IO ()))
nodemapCB nm = asyncCallback1 $ \n -> lkpNM nm (pFromJSVal n)

ov = JS.objectValue . JS.object
sv = JS.stringValue . pack
av = JS.arrayValue . JS.arrayValueList
dv = JS.doubleValue

data TState = TState { nid :: Int, eid :: Int }
type NodeMap = [(Int, Exp String)]

t2c :: Tree -> Int -> TState -> ([JS.Value], [JS.Value], NodeMap, TState)

t2c (Tree rp (den,ty) Empty) parent st =
  let node =
        ov [ ("data", ov [ ("id", sv $ "n" ++ show (nid st + 1))
                         , ("txt", sv rp)
                         ]
             )
           , ("group", sv "nodes")
           ]
      edge =
        ov [ ("data", ov [ ("id", sv $ "e" ++ show (eid st + 1))
                         , ("source", sv $ "n" ++ show parent)
                         , ("target", sv $ "n" ++ show (nid st + 1))
                         ]
             )
           , ("group", sv "edges")
           ]
   in ( [node]
      , if parent /= 0 then [edge] else []
      , [(nid st + 1, den)]
      , TState (nid st + 1) (eid st + 1)
      )

t2c (Tree rp (den,ty) (Bin t1 t2)) parent st =
  let node =
        ov [ ("data", ov [ ("id", sv $ "n" ++ show (nid st + 1))
                         , ("txt", sv . prettyExp . nf $ den)
                         ]
             )
           , ("group", sv "nodes")
           ]
      edge =
        ov [ ("data", ov [ ("id", sv $ "e" ++ show (eid st + 1))
                         , ("source", sv $ "n" ++ show parent)
                         , ("target", sv $ "n" ++ show (nid st + 1))
                         ]
             )
           , ("group", sv "edges")
           ]
      (lNodes, lEdges, lNM, lSt) = t2c t1 (nid st + 1) (TState (nid st + 1) (eid st + 1))
      (rNodes, rEdges, rNM, rSt) = t2c t2 (nid st + 1) (TState (nid lSt) (eid lSt))
   in ( node : lNodes ++ rNodes
      , (if parent /= 0 then (edge:lEdges) else lEdges) ++ rEdges
      , (nid st + 1, den) : lNM ++ rNM
      , TState (nid rSt) (eid rSt)
      )

t2c (Tree rp (den,ty) (Phase t)) parent st =
  let node =
        ov [ ("data", ov [ ("id", sv $ "n" ++ show (nid st + 1))
                         , ("txt", sv . prettyExp . nf $ den)
                         ]
             )
           , ("group", sv "nodes")
           ]
      edge =
        ov [ ("data", ov [ ("id", sv $ "e" ++ show (eid st + 1))
                         , ("source", sv $ "n" ++ show parent)
                         , ("target", sv $ "n" ++ show (nid st + 1))
                         ]
             )
           , ("group", sv "edges")
           ]
      (uNodes, uEdges, uNM, uSt) = t2c t (nid st + 1) (TState (nid st + 1) (eid st + 1))
   in ( node : uNodes
      , if parent /= 0 then (edge:uEdges) else uEdges
      , (nid st + 1, den) : uNM
      , TState (nid uSt) (eid uSt)
      )


dispTree :: Int -> Tree -> IO ()
dispTree n tree =
  let (nodes, edges, nodemap, _) = t2c tree 0 (TState 0 0)
   in nodemapCB nodemap >>= cyto n (av nodes) (av edges)

dispTrees :: [Tree] -> IO ()
dispTrees = sequence_ . zipWith zpr [0..]
  where zpr n t = let (ns, es, nm, _) = t2c t 0 (TState 0 0)
                   in nodemapCB nm >>= cyto n (av ns) (av es)

dispConfigs :: [Config] -> IO ()
dispConfigs = sequence_ . zipWith zpr [0..]
  where zpr n (ts, _) = gatherTrees n ts
        accum t (ns, es, nm, st) =
          let (ns', es', nm', st') = t2c t 0 st
           in (ns ++ ns', es ++ es', nm ++ nm', st')
        gatherTrees n trees =
          let (nds, eds, ndm, _) = foldr accum ([], [], [], TState 0 0) trees
           in nodemapCB ndm >>= cyto n (av nds) (av eds)

