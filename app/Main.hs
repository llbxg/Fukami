module Main where
import Numeric.LinearAlgebra
import System.Random
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Time

import Tools
import Durstenfeld
import ForMNIST
import Gosadenban


main :: IO ()
main = do
    main3