module ForMNIST
 ( makeImageMatrix
 , loadImage 
 , loadLabel
 , datasize
 , loadImageTest
 , loadLabelTest
 ) where   

-------------------------------------------------------------
--module
-------------------------------------------------------------

import qualified Data.ByteString.Lazy as B
import Numeric.LinearAlgebra
import Tools

------------------------------------------------

-------------------------------------------------------------
--MNISTのデータを回収する。
-------------------------------------------------------------
datasize = 10000

--与えられたリスト[a]から指定した要素数(num)ごとに分割してリストのリスト[[a]]を作る。
divideEqually :: Int -> [a] -> [[a]]
divideEqually _ [] = []
divideEqually num xs = xs1 : (divideEqually num xs2)
    where (xs1, xs2) = splitAt num xs

--画像イメージのBytestring(xs)から指定した長さを切り取ってリスト[Double]を作成する。
convertImage :: B.ByteString -> [Double]
convertImage xs = fmap toEnum $ fmap fromEnum . B.unpack $ B.drop byte xs
    where byte = 16  + 784*60000 -784 * datasize

--リストのリスト[[a]]からMatrixのリスト[[Matrix]]を作成する。
makeImageMatrix :: [[Double]] -> [Matrix Double]
makeImageMatrix [] = []
makeImageMatrix (x:xs) = (row x) : (makeImageMatrix xs)

loadImage1 :: B.ByteString -> [[Double]]
loadImage1 xs = divideEqually 784 (convertImage xs)

--画像イメージのBytestring(xs)から指定した長さを切り取ってMatrixのリスト[[Matrix]]を作成する。
loadImage :: B.ByteString -> [Matrix Double]
loadImage xs = makeImageMatrix $ divideEqually 784 (convertImage xs)

--ビジュアル化用なのでIntで出力する。
convertImage2 :: B.ByteString -> [Int]
convertImage2 xs = fmap fromEnum . B.unpack $ B.drop byte xs
    where byte = 16  + 784*10000 -784 * datasize

--ラベルのBytestring(xs)から指定した長さを切り取ってリスト[Int]を作成する。
loadLabel :: B.ByteString -> [Int]
loadLabel xs = fmap fromEnum . B.unpack $ B.drop byte xs
    where byte = 8  + 1*60000 - 1 * datasize

loadLabelTest :: B.ByteString -> [Int]
loadLabelTest xs = fmap fromEnum . B.unpack $ B.drop byte xs
    where byte = 8  + 1*10000 - 1 * 1000

convertImageTest :: B.ByteString -> [Double]
convertImageTest xs = fmap toEnum $ fmap fromEnum . B.unpack $ B.drop byte xs
    where byte = 16  + 784*10000 -784 * 1000

loadImageTest :: B.ByteString -> [Matrix Double]
loadImageTest xs = makeImageMatrix $ divideEqually 784 (convertImageTest xs)

------------------------------------------------