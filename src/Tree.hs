{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Tree (run) where

import           Graphics.Gloss (white, blank, display, Display(InWindow), Point
                               , Picture, line, pictures, translate)
import           Debug.Trace
import           Graphics.Gloss.Interface.Pure.Game

leaf = Tree []

-- | The 'run' function is a end point for testing first designs
-- | It will probably be removed when functionalities are split up in their respective modules and there is an actual api
run :: IO ()
run = do
  let pre = genTrees 16
      t = filter (\ts -> length ts == maximum (length <$> pre)) pre
  putStrLn "Exit: No Error"
  print $ "#Trees: " ++ show (length pre)
  print $ "Max Tree: " ++ show (maximum $ length <$> t)
  print $ "Max Depth: " ++ show (1 + maximum (maximum <$> t))

  --interactiveTree
  --display (InWindow "Trees" (round cDWidth, round cDHeight) (0, 0)) white t
  --interactiveRepresent t
maxOccurrence :: [Depth] -> Int
maxOccurrence ts = maximum $ mo ts (maximum ts) 0
  where
    mo ts' mx cur = if cur <= mx
                    then length (filter (== cur) ts'):mo ts' mx (cur + 1)
                    else []

cDWidth :: Float
cDWidth = 75 * 16

cDHeight :: Float
cDHeight = 75 * 9

horOffset :: Float
horOffset = 0

vertMargin :: Float
vertMargin = -25

horMargin :: Float
horMargin = 10

rootCoord :: Point
rootCoord = (0, 0)

newtype Tree = Tree [Tree]

type Index = Int

type Depth = Int

type Id = Int

type Count = Int

data T a = T a [T a]
  deriving Show

type DepthTree = T Int

-- Tree [Tree [], Tree [Tree [], Tree []]]
treeDepth :: Tree -> DepthTree
treeDepth (Tree ts) = T 0 $ treeDepth' 1 <$> ts

treeDepth' :: Depth -> Tree -> DepthTree
treeDepth' d (Tree ts) = T d $ treeDepth' (d + 1) <$> ts

dTreeFold :: DepthTree -> [Depth]
dTreeFold (T d ts) = d:concatMap dTreeFold ts

revix :: Int -> [a] -> Int
revix ix ls = length ls - 1 - ix

ixTreeFold :: [Depth] -> [Index]
ixTreeFold ds = ixTreeFold' ds ds 0

ixTreeFold' :: [Depth] -> [Depth] -> Index -> [Index]
ixTreeFold' ref (d:ds) ix = findIndex ref d ix:ixTreeFold' ref ds (ix + 1)
ixTreeFold' _ [] _ = []

findIndex :: [Depth] -> Depth -> Index -> Index
findIndex ref d ix = if d - 1 == ref !! ix || d == 0
                     then ix
                     else findIndex ref d (ix - 1)

generateIds :: [Depth] -> [Id]
generateIds (d:ds) = length (filter (== d) ds):generateIds ds
generateIds [] = []

prepareTreeToPic :: [Depth] -> [(Depth, Index, Id)]
prepareTreeToPic ds =
  zip3 ds (ixTreeFold ds) (reverse . generateIds . reverse $ ds)

reifyPoints :: [(Depth, Index, Id)] -> [(Point, Index)]
reifyPoints ((0, ix, id):ts) = ((0, 0), ix):reifyPoints ts
reifyPoints ((d, ix, id):ts) =
  ((fromIntegral id * horMargin, fromIntegral d * vertMargin), ix)
  :reifyPoints ts
reifyPoints [] = []

doLines :: [(Point, Index)] -> [(Point, Index)] -> [Picture]
doLines ref (((0, 0), _):ts) = doLines ref ts
doLines ref ((p, 0):ts) = line ((0, 0):[p]):doLines ref ts
doLines ref ((p, ix):ts) = line (fst (ref !! ix):[p]):doLines ref ts
doLines _ [] = []

depthTreeToPic :: [Depth] -> Picture
depthTreeToPic ds = let branches = reifyPoints . prepareTreeToPic $ ds
                    in pictures $ doLines branches branches

root :: [Depth]
root = [0, 1]

deeper :: [Depth] -> [Depth]
deeper ts = reverse $ last ts + 1:reverse ts

wider :: [Depth] -> [Depth]
wider ts = reverse $ last ts:reverse ts

unsafeUp :: [Depth] -> [Depth]
unsafeUp ts = reverse $ last ts - 1:reverse ts

up :: [Depth] -> Maybe [Depth]
up ts = if last ts > 1
        then Just $ unsafeUp ts
        else Nothing

nextTree :: [Depth] -> ([Depth], [Depth], Maybe [Depth])
nextTree t = (deeper t, wider t, up t)

genTrees :: Count -> [[Depth]]
genTrees c = root:genTrees' root c

genTrees' :: [Depth] -> Count -> [[Depth]]
genTrees' _ 0 = []
genTrees' ts c = let (d, w, mu) = nextTree ts
                     assured = d:genTrees' d (c - 1) ++ w:genTrees' w (c - 1)
                 in case mu of
                      Just u  -> assured ++ u:genTrees' u (c - 1)
                      Nothing -> assured

treeGallery :: Count -> Picture
treeGallery c = pictures $ treeGallery' (genTrees c) 0

treeGallery' :: [[Depth]] -> Float -> [Picture]
treeGallery' (t:ts) margin = translate (-margin) 0 (depthTreeToPic t)
  :treeGallery' ts (margin + fromIntegral (maxOccurrence t) * vertMargin)
treeGallery' [] _ = []

interactiveTree :: IO ()
interactiveTree = play
  (InWindow "Trees" (round cDWidth, round cDHeight) (0, 0))
  white
  5
  [0, 1]
  depthTreeToPic
  handleInputs
  (const id)

handleInputs :: Event -> [Depth] -> [Depth]
handleInputs (EventKey (SpecialKey k) Down _ _) t = case k of
  KeyUp    -> if last t > 1
              then unsafeUp t
              else t
  KeyDown  -> deeper t
  KeyLeft  -> root
  KeyRight -> wider t
handleInputs _ t = t

class Representable a where
  represent :: a -> Picture

instance Representable [Depth] where
  represent = depthTreeToPic

interactiveRepresent :: Representable a => [a] -> IO ()
interactiveRepresent rs = play
  (InWindow "Trees" (round cDWidth, round cDHeight) (0, 0))
  white
  60
  (rs, 0)
  (\(rs, ix) -> represent (rs !! (ix `mod` length rs)))
  handleIRep
  (const id)

handleIRep :: Event -> ([a], Index) -> ([a], Index)
handleIRep (EventKey (MouseButton k) _ _ _) (rs, ix) = case k of
  WheelUp   -> if ix - 10 < 0
               then (rs, length rs - 10)
               else (rs, ix - 10)
  WheelDown -> (rs, ix + 10)
  _         -> (rs, ix)
handleIRep _ t = t
