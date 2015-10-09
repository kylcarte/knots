{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

-- See <http://mazzo.li/posts/graph-drawing.html> for a lengthy
-- explanation about this code.
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.Random
import qualified Data.Foldable as F
import Data.Maybe (isJust)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game

type Vertex     = Int
type Edge       = (Vertex, Vertex)


-- Graph ----------------------------------------------------------------------
-- INVARIANT Every `Vertex` present in a set of neighbours is present as
-- a key in the `Map`.
newtype Graph 
        = Graph {grNeighs :: Map Vertex (Set Vertex)}

-- | An empty graph, with no edges or vertexes.
emptyGraph :: Graph
emptyGraph = Graph M.empty


-- | Add a new vertex to the graph.
addVertex :: Vertex -> Graph -> Graph
addVertex v (Graph neighs) 
 = Graph 
 $ case M.lookup v neighs of
        Nothing -> M.insert v S.empty neighs
        Just _  -> neighs

-- | Add a new edge to the graph.
addEdge :: Edge -> Graph -> Graph
addEdge (v1, v2) gr 
 = Graph neighs
 where  gr'     = addVertex v1 (addVertex v2 gr)
        neighs  = M.insert v1 (S.insert v2 (vertexNeighs v1 gr')) 
                $ M.insert v2 (S.insert v1 (vertexNeighs v2 gr')) 
                $ grNeighs gr'


-- | Yield the neighbours of a vertex.
vertexNeighs :: Vertex -> Graph -> Set Vertex
vertexNeighs v (Graph neighs) = neighs M.! v


-- | Get the set of edges in a graoh.
graphEdges :: Graph -> Set Edge
graphEdges 
 = M.foldrWithKey' foldNeighs S.empty . grNeighs
 where
        -- For each vertex `v1`, insert an edge for each neighbour `v2`.
        foldNeighs v1 ns es 
         = foldr (\v2 -> S.insert (order (v1, v2))) es ns

        order (v1, v2) 
         = if v1 > v2 then (v1, v2) else (v2, v1)


-- Scene ----------------------------------------------------------------------
-- INVARIANT The keys in `scGraph` are the same as the keys in `scPoints`.
data Scene = Scene 
  { scGraph     :: Graph
  , scPoints    :: Map Vertex Point
  , scSelected  :: Maybe Vertex
  , scViewState :: ViewState
  }

-- | An empty scene.
emptyScene :: Scene
emptyScene = Scene 
  { scGraph     = emptyGraph
  , scPoints    = M.empty
  , scSelected  = Nothing
  , scViewState = viewStateInit
  }


-- | Add a vertex to a scene.
scAddVertex :: Vertex -> Point -> Scene -> Scene
scAddVertex v pt sc = sc
  { scGraph  = scGraph  sc & addVertex v
  , scPoints = scPoints sc & M.insert v pt
  }


-- | Add an edge to a scene.
scAddEdge :: Edge -> Scene -> Scene
scAddEdge e@(v1, v2) sc = if M.member v1 pts && M.member v2 pts
  then sc
    { scGraph = scGraph sc & addEdge e
    }
  else error "scAddEdge: non existant point!"
  where
  pts = scPoints sc


-- | Randomize the endpoints of some edges, and pack them into a Scene.
fromEdges :: StdGen -> [Edge] -> Scene
fromEdges gen es = foldr scAddEdge
  ( fst (foldr addv (emptyScene, gen) vs)
  ) es
 where
 vs         = S.fromList (concat [[v1, v2] | (v1, v2) <- es])
 halfWidth  = fromIntegral (fst windowSize) / 2
 halfHeight = fromIntegral (snd windowSize) / 2
 addv v (sc, gen1) 
  = let (x, gen2) = randomR (-halfWidth,  halfWidth ) gen1
        (y, gen3) = randomR (-halfHeight, halfHeight) gen2
    in  (scAddVertex v (x, y) sc, gen3)


-- Drawing --------------------------------------------------------------------
vertexPos :: Vertex -> Scene -> Point
vertexPos v sc = scPoints sc M.! v

vertexRadius :: Float
vertexRadius    = 6


vertexColor :: Color
vertexColor     = makeColor 1 0 0 1 -- Red


edgeColor :: Color
edgeColor       = makeColor 1 1 1 0.8 -- Whiteish


drawVertex :: Vertex -> Scene -> Picture
drawVertex v sc = uncurry
    Translate (vertexPos v sc)
  $ ThickCircle (vertexRadius / 2) vertexRadius


drawEdge :: Edge -> Scene -> Picture
drawEdge (v1, v2) sc = Line
  [ vertexPos v1 sc
  , vertexPos v2 sc
  ]


drawScene :: Scene -> Picture
drawScene sc = applyViewPortToPicture port
 $ Pictures
 [ Color edgeColor $ Pictures
   [ drawEdge e sc
   | e <- F.toList $ graphEdges gr
   ]
 , Color vertexColor $ Pictures
   [ drawVertex n sc
   | n <- M.keys $ grNeighs gr
   ]
 ]
 where
 gr   = scGraph sc
 port = viewStateViewPort $ scViewState sc


-- Graph Layout ---------------------------------------------------------------
charge :: Float
charge = 100000

pushForce ::
     Point  -- ^ Vertex we're calculating the force for
  -> Point  -- ^ Vertex pushing the other away
  -> Vector
pushForce v1 v2 -- If we are analysing the same vertex, l = 0
  | l > 0     = (charge / l) `mulSV` normalizeV d 
  | otherwise = 0
  where
  d = v1 - v2
  l = magV d ** 2


stiffness :: Float
stiffness = 1 / 2

pullForce :: Point -> Point -> Vector
pullForce v1 v2 = stiffness `mulSV` (v2 - v1)


-- | Apply forces to update the position of a single point.
updatePosition ::
     Float       -- ^ Time since the last update
  -> Vertex      -- ^ Vertex we are analysing
  -> Scene
  -> Point       -- ^ New position
updatePosition dt v1 sc = v1pos + pull + push
  where
  v1pos  = vertexPos v1 sc
  -- Gets a velocity by multiplying the time by the force (we assume
  -- a mass of 1).
  getVel f = mulSV dt . f v1pos
  -- Sum all the pushing and pulling.  All the other vertices push,
  -- the connected vertices pull.
  push = sum $ M.map (getVel pushForce)
    $ scPoints sc
  pull = sum $ S.map (getVel pullForce . (`vertexPos` sc))
    $ vertexNeighs v1
    $ scGraph sc

-- | Apply forces to update the position of all the points.
updatePositions :: Float -> Scene -> Scene
updatePositions dt sc = foldr f sc $ M.keys $ grNeighs $ scGraph sc
 where
 sel = scSelected sc
 f n sc' = scAddVertex n pt sc'
    where
    pt = if Just n == sel
      then vertexPos n sc 
      else updatePosition dt n sc'


-- | Check if a point is in the given circle.
inCircle ::
     Point  -- ^ Where the user has clicked
  -> Float  -- ^ The scaling factor in the ViewPort
  -> Point  -- ^ The position of the vertex
  -> Bool
inCircle p sca v = magV (v - p) <= vertexRadius * sca

findVertex :: Point -> Float -> Scene -> Maybe Vertex
findVertex p1 sca = M.foldrWithKey' f Nothing . scPoints
  where
  f v p2 = \case
    Just v                 -> Just v
    _ | inCircle p1 sca p2 -> Just v
      | otherwise          -> Nothing

-- Events ---------------------------------------------------------------------
handleEvent :: Event -> Scene -> Scene
handleEvent ev sc = case ev of
  EventKey (MouseButton LeftButton) Down Modifiers{shift = Down} pos
    | Just v <- findVertex (inv pos) sca sc
                    -> sc { scSelected = Just v }
    | otherwise     -> sc
  EventKey (MouseButton LeftButton) Up _ _
    | Just _ <- sel -> sc { scSelected = Nothing }
  EventMotion pos
    | Just v <- sel -> sc { scPoints = scPoints sc & M.insert v (inv pos) }
  _                 -> sc { scViewState = scViewState sc & updateViewStateWithEvent ev }
  where
  sel       = scSelected  sc
  viewState = scViewState sc
  port      = viewStateViewPort viewState
  sca       = viewPortScale  port
  inv       = invertViewPort port

-- Sample Graph ---------------------------------------------------------------
-- Taken from <http://www.graphviz.org/Gallery/undirected/transparency.gv.txt>.
sampleGraph :: [Edge]
sampleGraph =
  [ ( 1, 30), ( 1, 40), ( 8, 46), ( 8, 16), (10, 25), (10, 19), (10, 33)
  , (12,  8), (12, 36), (12, 17), (13, 38), (13, 24), (24, 49), (24, 13)
  , (24, 47), (24, 12), (25, 27), (25, 12), (27, 12), (27, 14), (29, 10)
  , (29,  8), (30, 24), (30, 44), (38, 29), (38, 35), ( 2, 42), ( 2, 35)
  , ( 2, 11), (14, 18), (14, 24), (14, 38), (18, 49), (18, 47), (26, 41)
  , (26, 42), (31, 39), (31, 47), (31, 25), (37, 26), (37, 16), (39, 50)
  , (39, 14), (39, 18), (39, 47), (41, 31), (41,  8), (42, 44), (42, 29)
  , (44, 37), (44, 32), ( 3, 20), ( 3, 28), ( 6, 45), ( 6, 28), ( 9,  6)
  , ( 9, 16), (15, 16), (15, 48), (16, 50), (16, 32), (16, 39), (20, 33)
  , (33,  9), (33, 46), (33, 48), (45, 15), ( 4, 17), ( 4, 15), ( 4, 12)
  , (17, 21), (19, 35), (19, 15), (19, 43), (21, 19), (21, 50), (23, 36)
  , (34, 23), (34, 24), (35, 34), (35, 16), (35, 18), (36, 46), ( 5,  7)
  , ( 5, 36), ( 7, 32), ( 7, 11), ( 7, 14), (11, 40), (11, 50), (22, 46)
  , (28, 43), (28,  8), (32, 28), (32, 39), (32, 42), (40, 22), (40, 47)
  , (43, 11), (43, 17)
  ]


-- Main -----------------------------------------------------------------------
windowSize :: (Int, Int)
windowSize = (800, 600)


sceneWindow :: Scene -> IO ()
sceneWindow sc = play
  ( InWindow "Graph Drawing - shift + left mouse button to drag"
    windowSize (10, 10)
  ) black 30 sc drawScene handleEvent updatePositions


main :: IO ()
main = do
  gen <- getStdGen
  sceneWindow $ fromEdges gen sampleGraph

(&) :: a -> (a -> b) -> b
a & f = f a
infixl 0 &

