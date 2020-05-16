module Durstenfeld
 ( exchange
 , durstenfeld
 , randomChoice
 ) where

-------------------------------------------------------------
--module
-------------------------------------------------------------

import System.Random

-------------------------------------------------------------
-------------------------------------------------------------
--ランダム抽出(重複無し乱数生成)　Durstenfeld

--リストを受け取り[Int]、指定した番号(num)の要素と一番最後の要素を入れ替えたリストを返す。
exchange :: Int -> [Int] -> ([Int],[Int])
exchaneg num [] = []
exchange num list = (init xs ++ [last ys] ++ init ys  , [last xs])
    where (xs,ys) = splitAt num list

--指定した長さ(num)の重複無しの乱数[Int]を作成する。
--(mkStdGen　Int)の乱数ジェネレータを最初に与える必要がある。
durstenfeld :: Int -> [Int] -> StdGen -> [Int]
durstenfeld _ [] _ = []
durstenfeld 0 x _ = x
durstenfeld n list g = new ++ durstenfeld (n-1) nokori g2
    where (ransu , g2) = randomR (1,n) g
          (nokori , new) = exchange ransu list

--dataSize(ex.60000)からbatchSize(ex.100)を抽出する。
randomChoice :: Int -> Int -> StdGen -> [Int]
randomChoice dataSize batchSize gen = take batchSize $ durstenfeld (dataSize - 1) list gen
    where list = take dataSize [0..]

-------------------------------------------------------------