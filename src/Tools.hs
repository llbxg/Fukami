    
module Tools
( randomMatrix
, makeBatchList
, encorderForDat2
, encorderForDat
, length'
, pickUpNum
, setElem
, oneHotEncoder
, oneHotEncoder'
) where

-------------------------------------------------------------
--module
-------------------------------------------------------------

import Numeric.LinearAlgebra
import System.Random
import Data.List


import Durstenfeld

------------------------------------------------

-------------------------------------------------------------
--Tools
-------------------------------------------------------------

--重みの初期化用
--要素が0。0から1.0の間の数で、指定した行数、列数のMatrixを生成する。
randomMatrix :: RandomGen g => Int -> Int -> g -> Matrix Double
randomMatrix row col g = matrix col $ take (row*col) $ randomRs (0.0, 0.01) g

--ex) randomMatrix 3 4 (mkStdGen 100)

makeBatchList :: (Eq t, Num t) => Int -> t -> Int -> Int -> [[Int]]
makeBatchList _ 0 _ _ = []
makeBatchList g num ds bs = batch' : makeBatchList (g - 1) (num - 1) ds bs
    where data_size = ds
          batch_size = bs
          batch' = randomChoice data_size batch_size (mkStdGen g )

--数字のリストのリストをファイルに書き込むために、良い感じにいじっている。
encorderForDat :: Show a => [[a]] -> [Char]
encorderForDat xs = intercalate "\n" $  fmap (intercalate " " . fmap show) xs

--リストを書き込む用
encorderForDat2 :: Show a => [a] -> [Char]
encorderForDat2 xs = intercalate "\n" $  fmap show xs 

length' :: [a] -> Double
length' [] = 0.0
length' (x:xs) = 1.0 + length' xs

pickUpNum :: [Int] -> [a] -> [a]
pickUpNum [] _ = []
pickUpNum (x:xs) list = number : pickUpNum xs list
    where number = list !! x

setElem1 :: Matrix R -> (Int, Int) -> Int -> Int -> R -> Matrix R
setElem1 m (row, col) maxrow maxcol newElem = newM
    where v  = flatten m
          num = row * maxcol + (col+1) --要素が何番目か
          v1 = subVector 0 (num-1) v
          v2 = subVector (num) (maxcol*maxrow-num) v
          v3 = vector [newElem]
          newV = vjoin [v1,v3,v2]
          newM = reshape maxcol newV

--〇
setElem :: Matrix R -> (Int, Int) -> R -> Matrix R
setElem m (row, col) newElem = setElem1 m (row, col) maxrow maxcol newElem
    where maxrow = rows m
          maxcol = cols m

--ラベル(0~9)をOne-Hot表現のMatrix(1行10列)に変換
--ex) 9 --> 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 
oneHotEncoder :: Int -> Matrix Double
oneHotEncoder num = setElem zeroM (0 ,num) 1.0 
    where zeroM = row [0.0,0,0,0,0,0,0,0,0,0]

-- =============================================

--setElem1' :: Matrix R -> (Int, Int) -> Int -> Int -> R -> Matrix R
setElem' v col newElem = newV
    where v1 = subVector 0 (col-1) v
          v2 = subVector col (10-col) v
          v3 = vector [newElem]
          newV = vjoin [v1,v3,v2]

oneHotEncoder' :: Int -> Vector R
oneHotEncoder' num = setElem' zeroV (num+1) 1.0 
    where zeroV = fromList [0.0,0,0,0,0,0,0,0,0,0]


------------------------------------------------