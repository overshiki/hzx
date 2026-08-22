{-# LANGUAGE BangPatterns #-}

module HZX.LinAlg.Z2
  ( Matrix
  , fromLists
  , toLists
  , nRows
  , nCols
  , (!)
  , row
  , col
  , transpose
  , identity
  , gaussianEliminate
  , rank
  , nullspaceBasis
  , solveLinearSystem
  , matrixMultZ2
  , vectorAddZ2
  ) where

import Data.Bits ((.&.), xor, testBit, setBit, clearBit, shiftR)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- | A matrix over Z2 (the field with 2 elements).
--   Stored in row-major form with bits packed into Integers.
--   Each row is an Integer where bit j represents column j.
newtype Matrix = Matrix
  { rows :: IntMap Integer  -- ^ Map from row index to bit-packed row
  } deriving (Eq, Show)

-- | Create a matrix from a list of lists (row-major).
--   Each inner list should contain 0s and 1s.
fromLists :: [[Int]] -> Matrix
fromLists rs = Matrix $ IM.fromList $ zip [0..] (map packRow rs)
  where
    packRow :: [Int] -> Integer
    packRow cs = foldl' setIfOne 0 (zip [0..] cs)
    
    setIfOne :: Integer -> (Int, Int) -> Integer
    setIfOne acc (j, 1) = setBit acc j
    setIfOne acc (_, 0) = acc
    setIfOne _ (_, _)   = error "fromLists: elements must be 0 or 1"

-- | Convert a matrix to a list of lists.
toLists :: Matrix -> Int -> [[Int]]
toLists (Matrix rs) numCols = map (unpackRow numCols . snd) (IM.toList rs)
  where
    unpackRow :: Int -> Integer -> [Int]
    unpackRow n r = [if testBit r j then 1 else 0 | j <- [0..n-1]]

-- | Get the number of rows.
nRows :: Matrix -> Int
nRows (Matrix rs) = IM.size rs

-- | Get the number of columns (requires knowing the expected width).
nCols :: Matrix -> Int
nCols _ = 0  -- Cannot determine from representation alone; use context

-- | Get element at row i, column j.
(!) :: Matrix -> Int -> Int -> Int
(!) (Matrix rs) i j =
  case IM.lookup i rs of
    Just r -> if testBit r j then 1 else 0
    Nothing -> 0

-- | Get a row as an Integer bit pattern.
row :: Matrix -> Int -> Integer
row (Matrix rs) i = fromMaybe 0 (IM.lookup i rs)

-- | Get a column as a list of bits.
col :: Matrix -> Int -> [Int]
col (Matrix rs) j = map (\(_, r) -> if testBit r j then 1 else 0) (IM.toList rs)

-- | Transpose a matrix.
transpose :: Matrix -> Int -> Matrix
transpose (Matrix rs) numCols =
  Matrix $ IM.fromList [(j, packCol j) | j <- [0..numCols-1]]
  where
    packCol j = foldl' (\acc (i, r) -> if testBit r j then setBit acc i else acc) 0 (IM.toList rs)

-- | Create an n×n identity matrix.
identity :: Int -> Matrix
identity n = Matrix $ IM.fromList [(i, setBit 0 i) | i <- [0..n-1]]

-- | Add two vectors over Z2 (element-wise XOR).
vectorAddZ2 :: Integer -> Integer -> Integer
vectorAddZ2 = xor

-- | Gaussian elimination: transform matrix to row-echelon form.
--   Returns (rank, transformed matrix, pivot columns, transformation matrix).
--   The transformation matrix T satisfies: T * A = R where R is in REF.
gaussianEliminate :: Matrix -> Int -> (Int, Matrix, [Int], Matrix)
gaussianEliminate (Matrix rs) numCols = go 0 0 (rs, initT, [])
  where
    numRows = IM.size rs
    -- Transformation starts as the identity matrix.
    initT = IM.fromList [(i, setBit 0 i) | i <- IM.keys rs]

    -- State: (current matrix rows, transformation matrix rows, pivot columns)
    go :: Int -> Int -> (IntMap Integer, IntMap Integer, [Int]) -> (Int, Matrix, [Int], Matrix)
    go !r !c (mRows, tRows, pivots)
      | r >= numRows || c >= numCols =
          -- In row-echelon form the number of pivots equals the rank.
          (length pivots, Matrix mRows, reverse pivots, Matrix tRows)
      | otherwise =
          case findPivot r c mRows of
            Nothing -> go r (c + 1) (mRows, tRows, pivots)
            Just pivotRow ->
              let -- Swap rows if necessary
                  (mRows1, tRows1) = if pivotRow == r
                                     then (mRows, tRows)
                                     else (swapRows r pivotRow mRows, swapRows r pivotRow tRows)
                  -- Eliminate below
                  pivotVal = fromMaybe 0 (IM.lookup r mRows1)
                  (mRows2, tRows2) = foldl' (eliminateRow r c pivotVal) (mRows1, tRows1) [r+1..numRows-1]
              in go (r + 1) (c + 1) (mRows2, tRows2, c : pivots)
    
    findPivot :: Int -> Int -> IntMap Integer -> Maybe Int
    findPivot startRow c m = 
      listToMaybe [i | (i, rowVal) <- IM.toList m, i >= startRow, testBit rowVal c]
    
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x
    
    swapRows :: Int -> Int -> IntMap Integer -> IntMap Integer
    swapRows i j m =
      let rowI = IM.lookup i m
          rowJ = IM.lookup j m
      in case (rowI, rowJ) of
           (Just ri, Just rj) -> IM.insert i rj (IM.insert j ri m)
           (Just ri, Nothing) -> IM.insert j ri (IM.delete i m)
           (Nothing, Just rj) -> IM.insert i rj (IM.delete j m)
           (Nothing, Nothing) -> m
    
    eliminateRow :: Int -> Int -> Integer -> (IntMap Integer, IntMap Integer) -> Int -> (IntMap Integer, IntMap Integer)
    eliminateRow pivotRow c pivotVal (m, t) i =
      case IM.lookup i m of
        Just rowVal | testBit rowVal c ->
          let newRow = rowVal `xor` pivotVal
              -- Track transformation: T[i] = T[i] `xor` T[pivotRow]
              tPivot = fromMaybe 0 (IM.lookup pivotRow t)
              tI = fromMaybe 0 (IM.lookup i t)
              newTI = tI `xor` tPivot
          in (IM.insert i newRow m, IM.insert i newTI t)
        _ -> (m, t)

-- | Compute the rank of a matrix.
rank :: Matrix -> Int -> Int
rank m numCols = let (r, _, _, _) = gaussianEliminate m numCols in r

-- | Compute a basis for the nullspace of a matrix.
--   Returns a list of vectors (each as a bit-packed Integer).
nullspaceBasis :: Matrix -> Int -> [Integer]
nullspaceBasis (Matrix rs) numCols =
  let (r, Matrix mRows, pivotCols, _) = gaussianEliminate (Matrix rs) numCols
      pivotSet = IM.fromList (zip pivotCols [0..])
      freeCols = [c | c <- [0..numCols-1], not (IM.member c pivotSet)]
  in map (nullspaceVector mRows pivotCols) freeCols
  where
    nullspaceVector :: IntMap Integer -> [Int] -> Int -> Integer
    nullspaceVector mRows pivotCols freeCol =
      let -- Start with 1 at the free column
          vec = setBit 0 freeCol
          -- Back-substitute to find pivot values
          backSub = foldl' (computePivot mRows freeCol) vec (reverse (zip [0..] pivotCols))
      in backSub
    
    computePivot :: IntMap Integer -> Int -> Integer -> (Int, Int) -> Integer
    computePivot mRows freeCol vec (rowIdx, colIdx) =
      case IM.lookup rowIdx mRows of
        Just rowVal ->
          let -- Compute dot product of row with current vector
              dot = popCount (rowVal .&. vec) `mod` 2
          in if dot == 1 then setBit vec colIdx else vec
        Nothing -> vec
    
    popCount :: Integer -> Int
    popCount 0 = 0
    popCount n = fromIntegral (n .&. 1) + popCount (n `shiftR` 1)

-- | Solve a linear system Ax = b over Z2.
--   Returns Nothing if no solution exists, Just solution if one exists.
--   Note: This returns a particular solution, not the general solution.
solveLinearSystem :: Matrix -> [Int] -> Int -> Maybe [Int]
solveLinearSystem (Matrix rs) b numCols =
  let -- Augment matrix with b
      augmented = IM.mapWithKey (\i rowVal ->
        let bi = if i < length b then b !! i else 0
        in if bi == 1 then setBit rowVal numCols else rowVal) rs
      (r, Matrix mRows, _, _) = gaussianEliminate (Matrix augmented) (numCols + 1)
      -- Check for inconsistency: row of all zeros in A but 1 in augmented column
      isConsistent = all (\(_, rowVal) ->
        let aPart = clearBit rowVal numCols
            bPart = if testBit rowVal numCols then 1 else 0
        in not (aPart == 0 && bPart == 1)) (IM.toList mRows)
  in if not isConsistent
     then Nothing
     else Just (backSubstitute mRows numCols)
  where
    backSubstitute :: IntMap Integer -> Int -> [Int]
    backSubstitute mRows n =
      let -- Find pivots
          pivots = [(i, c) | (i, rowVal) <- IM.toList mRows, 
                            c <- [0..n-1], testBit rowVal c, 
                            all (\c' -> c' == c || not (testBit rowVal c')) [0..c-1]]
          -- Initialize solution with zeros
          sol = replicate n 0
          -- Back-substitute
      in foldl' (updateSol mRows) sol pivots
    
    updateSol :: IntMap Integer -> [Int] -> (Int, Int) -> [Int]
    updateSol mRows sol (rowIdx, colIdx) =
      case IM.lookup rowIdx mRows of
        Just rowVal ->
          let -- Compute sum of known variables
              knownSum = sum [if testBit rowVal j && j /= colIdx then sol !! j else 0 | j <- [0..length sol-1]] `mod` 2
              -- Pivot variable value = b - knownSum
              bVal = if testBit rowVal (length sol) then 1 else 0
              val = (bVal - knownSum) `mod` 2
          in take colIdx sol ++ [val] ++ drop (colIdx + 1) sol
        Nothing -> sol

-- | Multiply two matrices over Z2.
matrixMultZ2 :: Matrix -> Matrix -> Int -> Matrix
matrixMultZ2 (Matrix aRows) (Matrix bRows) bNumCols =
  let result = IM.mapWithKey (\i aRow ->
        let rowResult = foldl' (\acc j ->
              let dot = computeDot aRow bRows j
              in if dot == 1 then setBit acc j else acc) 0 (take bNumCols [0..])
        in rowResult) aRows
  in Matrix result
  where
    computeDot :: Integer -> IntMap Integer -> Int -> Int
    computeDot aRow bRows' colIdx =
      let col = foldl' (\acc (i, bRow) ->
            if testBit bRow colIdx then setBit acc i else acc) 0 (IM.toList bRows')
      in popCount (aRow .&. col) `mod` 2
    
    popCount :: Integer -> Int
    popCount 0 = 0
    popCount n = fromIntegral (n .&. 1) + popCount (n `shiftR` 1)
