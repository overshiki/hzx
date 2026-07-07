-- | Small dense tensors for evaluating ZX diagrams.
--
--   This is intentionally minimal: all axes have dimension 2, and the
--   implementation is list-based so it needs no extra dependencies.
module HZX.Verification.Tensor
  ( Tensor(..)
  , LabeledTensor(..)
  , EdgeLabel
  , contract
  , traceAxes
  , traceAxesH
  , applyH
  , kronecker
  , permuteLabels
  , reshapeToMatrix
  , zSpider
  , hBox
  ) where

import Data.Complex (Complex(..), cis)
import Data.List (delete, elemIndex)

type EdgeLabel = Int

-- | A dense tensor with shape and a flat row-major vector of complex values.
data Tensor = Tensor
  { tShape :: [Int]
  , tData  :: [Complex Double]
  } deriving (Show, Eq)

-- | A tensor whose axes are labeled by edge occurrences.
data LabeledTensor = LabeledTensor
  { ltShape  :: [Int]
  , ltLabels :: [EdgeLabel]
  , ltData   :: [Complex Double]
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Flat-index helpers (row-major)
-- ---------------------------------------------------------------------------

flatIndex :: [Int] -> [Int] -> Int
flatIndex shape coords = sum $ zipWith (*) coords strides
  where
    strides = scanr (*) 1 (tail shape)

allCoords :: [Int] -> [[Int]]
allCoords [] = [[]]
allCoords (d:ds) = [ c : cs | c <- [0 .. d - 1], cs <- allCoords ds ]

lookupCoord :: LabeledTensor -> [Int] -> Complex Double
lookupCoord t coords = ltData t !! flatIndex (ltShape t) coords

-- ---------------------------------------------------------------------------
-- Tensor constructors
-- ---------------------------------------------------------------------------

-- | Z spider tensor of rank @n@ and phase @alpha@ (multiple of π).
zSpider :: Int -> Rational -> [EdgeLabel] -> LabeledTensor
zSpider n alpha labels =
  let shape = replicate n 2
      vals  = [ if all (== 0) coords then 1
                else if all (== 1) coords then cis (pi * fromRational alpha)
                else 0
              | coords <- allCoords shape ]
  in LabeledTensor shape labels vals

-- | Hadamard box tensor (rank 2).
hBox :: [EdgeLabel] -> LabeledTensor
hBox labels =
  let h = 1 / sqrt 2
      shape = [2, 2]
      vals  = [ h, h, h, -h ]
  in LabeledTensor shape labels vals

-- ---------------------------------------------------------------------------
-- Tensor operations
-- ---------------------------------------------------------------------------

-- | Contract two labeled tensors on the given axes.
contract :: LabeledTensor -> EdgeLabel -> LabeledTensor -> EdgeLabel -> LabeledTensor
contract t1 lbl1 t2 lbl2 =
  let Just i = elemIndex lbl1 (ltLabels t1)
      Just j = elemIndex lbl2 (ltLabels t2)
      d = ltShape t1 !! i
      s1 = ltShape t1
      s2 = ltShape t2
      s1' = deleteIndex i s1
      s2' = deleteIndex j s2
      shape = s1' ++ s2'
      vals =
        [ sum [ lookupCoord t1 (insertAt i k c1') *
                lookupCoord t2 (insertAt j k c2')
              | k <- [0 .. d - 1] ]
        | c1' <- allCoords s1'
        , c2' <- allCoords s2'
        ]
      labels = deleteIndex i (ltLabels t1) ++ deleteIndex j (ltLabels t2)
  in LabeledTensor shape labels vals

-- | Trace two axes of the same tensor that share the given label.
traceAxes :: LabeledTensor -> EdgeLabel -> LabeledTensor
traceAxes = traceAxesWithH False

-- | Trace two axes of the same tensor that share the given label, inserting
--   a Hadamard matrix between them.
traceAxesH :: LabeledTensor -> EdgeLabel -> LabeledTensor
traceAxesH = traceAxesWithH True

traceAxesWithH :: Bool -> LabeledTensor -> EdgeLabel -> LabeledTensor
traceAxesWithH withH t label =
  let idxs = labelIndices label (ltLabels t)
  in case idxs of
       [i, j] ->
         let (low, high) = if i < j then (i, j) else (j, i)
             d = ltShape t !! low
             s = ltShape t
             s' = deleteIndex low $ deleteIndex high s
             h k1 k2 = if withH
                       then (if k1 == 0 && k2 == 0 then 1 / sqrt 2
                             else if k1 == 0 && k2 == 1 then 1 / sqrt 2
                             else if k1 == 1 && k2 == 0 then 1 / sqrt 2
                             else -1 / sqrt 2)
                       else if k1 == k2 then 1 else 0
             vals =
               [ sum [ h k1 k2 * lookupCoord t (insertAt high k2 (insertAt low k1 c'))
                     | k1 <- [0 .. d - 1], k2 <- [0 .. d - 1] ]
               | c' <- allCoords s'
               ]
             labels = deleteIndex low $ deleteIndex high (ltLabels t)
         in LabeledTensor s' labels vals
       _ -> error "traceAxesWithH: label must appear exactly twice"

-- | Apply a Hadamard matrix to the axis with the given label.
applyH :: LabeledTensor -> EdgeLabel -> LabeledTensor
applyH t label =
  let temp = freshLabel t
      t' = renameLabel label temp t
      h = hBox [temp, label]
  in contract t' temp h temp

-- | Kronecker product of two tensors (append axes).
kronecker :: LabeledTensor -> LabeledTensor -> LabeledTensor
kronecker t1 t2 =
  let shape = ltShape t1 ++ ltShape t2
      vals  = [ a * b | a <- ltData t1, b <- ltData t2 ]
      labels = ltLabels t1 ++ ltLabels t2
  in LabeledTensor shape labels vals

-- | Reorder axes to the given label order.
permuteLabels :: LabeledTensor -> [EdgeLabel] -> LabeledTensor
permuteLabels t target =
  let mapping = [ case elemIndex l (ltLabels t) of
                    Just i  -> i
                    Nothing -> error "permuteLabels: label not found"
                | l <- target ]
      newShape = [ ltShape t !! i | i <- mapping ]
      oldStrides = scanr (*) 1 (tail (ltShape t))
      newStrides = scanr (*) 1 (tail newShape)
      vals =
        [ let oldCoords = [ (idx `div` s) `mod` d | (d, s) <- zip (ltShape t) oldStrides ]
              newCoords = [ oldCoords !! i | i <- mapping ]
              newIdx    = sum $ zipWith (*) newCoords newStrides
          in ltData t !! newIdx
        | idx <- [0 .. product newShape - 1]
        ]
  in LabeledTensor newShape target vals

-- | Flatten the tensor into a matrix with input labels as rows and output
--   labels as columns.
reshapeToMatrix :: [EdgeLabel] -> [EdgeLabel] -> LabeledTensor -> [[Complex Double]]
reshapeToMatrix inputs outputs t =
  let t' = permuteLabels t (inputs ++ outputs)
      nOut = length outputs
      cols = 2 ^ nOut
      vals = ltData t'
  in [ [ vals !! (r * cols + c) | c <- [0 .. cols - 1] ] | r <- [0 .. 2 ^ length inputs - 1] ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

deleteIndex :: Int -> [a] -> [a]
deleteIndex i xs = take i xs ++ drop (i + 1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

labelIndices :: Eq a => a -> [a] -> [Int]
labelIndices x = go 0
  where
    go _ [] = []
    go i (y:ys) | x == y    = i : go (i + 1) ys
                | otherwise = go (i + 1) ys

freshLabel :: LabeledTensor -> EdgeLabel
freshLabel t = if null (ltLabels t) then 0 else maximum (ltLabels t) + 1

renameLabel :: EdgeLabel -> EdgeLabel -> LabeledTensor -> LabeledTensor
renameLabel old new t =
  t { ltLabels = [ if l == old then new else l | l <- ltLabels t ] }
