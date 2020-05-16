module Gosadenban 
( main3

) where

-------------------------------------------------------------
--module
-------------------------------------------------------------

import Numeric.LinearAlgebra
import System.Random
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Time

import Tools
import Durstenfeld
import ForMNIST

------------------------------------------------

data ActivationFunction = ActivationFunction
    { func  :: Double -> Double , 
      func' :: Double -> Double ,
      description :: String
    }

data Params = Params
    { weight :: Matrix Double ,
      bias :: Matrix Double
    } deriving (Show)

data DParams = DParams
    { dweight :: Matrix Double ,
      dbias :: Matrix Double
    } deriving (Show)

data LayerProp = LayerProp
    { params :: Params ,
      af :: ActivationFunction
    }

data HyperProp = HyperProp
    { vw0 :: Matrix Double ,
      vb0 :: Matrix Double
    }

data Hyperparameter = Hyperparameter
    { learningrate :: Double ,
      alpha0 :: Double ,
      beta0  :: Double ,
      beta1  :: Double
    }

data Network = Network
    { layers :: [LayerProp] ,
      hprop  :: [HyperProp] ,
      hpara  :: Hyperparameter
    }

data PropagateLayer 
    = PropagateLayer
        { inData  :: Matrix Double ,
          outData :: Matrix Double ,
          pAf :: ActivationFunction ,
          pParams :: Params
        }
    | PropagateSensorLayer
        { outData :: Matrix Double
        }

data BackPropagateLayer
    = BackPropagateLayer
        { inGrad  :: Matrix Double ,
          outGrad :: Matrix Double ,
          bpAf :: ActivationFunction ,
          bpParams :: Params ,
          dParams :: DParams
        }
    | BackPropagateLastLayer
        { outGrad :: Matrix Double
        }

data ErrorLayer
    = ErrorLayer
        { lastData :: Matrix Double ,
          teachData :: Matrix Double ,
          errorValue :: Double ,
          grad :: Matrix Double
        } deriving (Show)

-------------------------------------------------------------

propagate :: PropagateLayer -> LayerProp -> PropagateLayer
propagate dataJ layerK = PropagateLayer
    { inData  = x ,
      outData = y ,
      pAf     = functions ,
      pParams = p
    }
    where x = outData dataJ
          p = params layerK
          w = weight p
          b = bias p 
          a = add b $ x Numeric.LinearAlgebra.<> w
          functions =  af layerK
          h = func functions
          y = cmap h a 

propagateNetwork :: Matrix Double -> Network -> [PropagateLayer]
propagateNetwork input net = calcs
    where calcs = scanl propagate layer0 (layers net)
          layer0 = PropagateSensorLayer { outData = input}

-------------------------------------------------------------

backpropagate :: PropagateLayer -> BackPropagateLayer -> BackPropagateLayer
backpropagate pLayerJ bpLayerK = BackPropagateLayer
    { inGrad = indx ,
      outGrad = dx ,
      bpAf = af ,
      bpParams  = p ,
      dParams = dP
    }
    where af = pAf pLayerJ
          p = pParams pLayerJ
          h' = func' af
          indx = outGrad bpLayerK
          y = outData pLayerJ
          dL' = cmap h' y
          dL = indx * dL'
          db = dL
          x = inData pLayerJ
          w = weight p
          dw = (tr x) Numeric.LinearAlgebra.<> dL
          dx = dL Numeric.LinearAlgebra.<> (tr w)
          dP = DParams { dweight = dw, dbias = db}
          

backpropagateNetwork :: ErrorLayer -> [PropagateLayer] ->[BackPropagateLayer]
backpropagateNetwork el prL = tail ans1
    where ans1 = scanr backpropagate layerFin prL
          layerFin = BackPropagateLastLayer {outGrad = grad0}
          grad0 = grad el

-------------------------------------------------------------

--Softmax & cross entropy error
errorNetwork :: Matrix Double -> Matrix Double -> ErrorLayer 
errorNetwork ld td = ErrorLayer
    { lastData = dataFromSoftmaxFunction ,
      teachData = td ,
      errorValue = e / len ,
      grad = g
    }
    where dataFromSoftmaxFunction = softmaxBatch' ld
          g = add dataFromSoftmaxFunction $ scale (-1) td
          y = map asRow $ toRows dataFromSoftmaxFunction
          t = map asRow $ toRows td
          e' = zipWith cee t y
          e = sum e'
          len = length' t

-------------------------------------------------------------

inference :: Matrix Double -> Int -> Double
inference x t = ans
    where (r, c) = maxIndex x
          ans = if c == t then 1.0 else 0.0

accuracyNet :: Network -> Matrix Double -> Int -> Double
accuracyNet net x t = a
    where ans' = propagateNetwork x net
          ans  = last $ map outData ans'
          a    = inference ans t 

accuracyNetwork :: Network -> [Matrix Double] -> [Int] -> Double
accuracyNetwork net xs ts = sumans / lenans
    where f x t = accuracyNet net x t 
          ans = zipWith f xs ts
          sumans = sum ans
          lenans = length' xs

----------------------------------------------------------------------
sgd :: Matrix Double -> Matrix Double -> Hyperparameter -> Matrix Double
sgd x dx hp = x - scale eta dx
    where eta = learningrate hp



update :: Hyperparameter -> Params -> DParams -> Params
update hp p dp = newParam
    where b  = bias p
          w  = weight p
          db = dbias dp
          dw = dweight dp
          new_b = sgd b db hp
          new_w = sgd w dw hp
          newParam = Params {bias = new_b, weight = new_w}

updateNetowrk :: Network -> [DParams] -> Network
updateNetowrk net dp = new_Network
    where layerList = layers net
          hp = hpara net
          hpp = hprop net
          paramList = map params layerList
          afList = map af layerList
          f = update hp
          new_Para = zipWith f paramList dp
          new_Layer = zipWith makeLayerProp new_Para afList
          new_Network = Network{layers = new_Layer, hpara = hp, hprop = hpp  }


momentum :: Matrix Double -> Matrix Double -> Hyperparameter -> Matrix Double -> ( Matrix Double, Matrix Double)
momentum x dx hp v = (x + new_v, new_v)
    where eta = learningrate hp
          alpha = alpha0 hp
          new_v = (scale alpha v) - scale eta dx

adagrad :: Matrix Double -> Matrix Double -> Hyperparameter -> Matrix Double -> ( Matrix Double, Matrix Double)
adagrad x dx hp v = (new_x, new_v')
    where eta = learningrate hp
          alpha = alpha0 hp
          new_v' = v + dx*dx
          new_v = cmap sqrt new_v'
          new_v'' = cmap devdev new_v
          new_x = x - scale eta ( dx/new_v'' )

devdev :: Double -> Double
devdev x =
    if x == 0 
        then 1.0
        else x

update' :: Hyperparameter -> (HyperProp , Params) -> DParams -> (Params, HyperProp)
update' hp (hpp, p) dp = (newParam, newHpp)
    where b  = bias p
          w  = weight p
          db = dbias dp
          dw = dweight dp
          vw = vw0 hpp
          vb = vb0 hpp
          (new_w, new_vw) = momentum w dw hp vw
          (new_b, new_vb) = momentum b db hp vb
          newParam = Params {bias = new_b, weight = new_w}
          newHpp = HyperProp {vw0 = new_vw, vb0 = new_vb}

updateNetowrk' :: Network -> [DParams] -> Network
updateNetowrk' net dp = new_Network
    where layerList = layers net
          hp = hpara net
          hpp = hprop net
          paramList = map params layerList
          afList = map af layerList
          f = update' hp 
          hppp = zip hpp paramList
          new_Para_And_Hpp = zipWith f hppp dp
          new_Para = map fst new_Para_And_Hpp
          new_Hpp  = map snd new_Para_And_Hpp 
          new_Layer = zipWith makeLayerProp new_Para afList
          new_Network = Network{layers = new_Layer, hpara = hp, hprop = new_Hpp  }

----------------------------------------------------------------------
 
makeLayerProp :: Params -> ActivationFunction -> LayerProp
makeLayerProp p aF = LayerProp {params = p, af = aF }

----------------------------------------------------------------------

loadL :: B.ByteString -> [Matrix Double]
loadL xs = tL4
    where tLabel  = loadLabel xs
          f x = pickUpNum x tLabel
          tL1 = map f batch_List
          tL2 = map ( map oneHotEncoder ) tL1
          tL3 = map ( map flatten ) tL2
          tL4 = map fromRows tL3

loadI :: B.ByteString -> [Matrix Double]        
loadI xs = tI4
    where tImage  = loadImage xs
          g x = pickUpNum x tImage
          tI1 = map g batch_List
          tI2 = map ( map (scale (1/255.0) )) tI1
          tI3 = map ( map flatten ) tI2
          tI4 = map fromRows tI3

----------------------------------------------------------------------

finPara :: Matrix Double -> Matrix Double -> Params
finPara b w = Params {bias = b, weight = w}
 
finLayer :: Params -> ActivationFunction -> LayerProp
finLayer p a = LayerProp {params = p, af = a }

----------------------------------------------------------------------

sigmoidFunction :: Floating a => a -> a
sigmoidFunction x = 1 / (1 + exp (-x) )

sigmoidFunctionBack :: Floating a => a -> a
sigmoidFunctionBack  x = x*(1-x) 

sigmoidFunc :: ActivationFunction
sigmoidFunc = ActivationFunction
    { func  = sigmoidFunction, 
      func' = sigmoidFunctionBack,
      description = "Sigmoid Function"
    }
  
   
reluFunction :: Double -> Double
reluFunction x
    | x <= 0 = 0.0
    | x > 0  = x

reluFunctionBack :: Double -> Double
reluFunctionBack x
    | x <= 0 = 0.0
    | x > 0  = 1.0

reluFunc :: ActivationFunction
reluFunc = ActivationFunction
    { func  = reluFunction,
      func' = reluFunctionBack,
      description = "Relu Function"
    }


identity :: ActivationFunction
identity = ActivationFunction
    { func  = id,
      func' = backOne,
      description = "id"
    } 

backOne :: Double -> Double
backOne x = 1.0

-------------------------------------------------------------


softmax :: (Linear t c, Floating t, Container c t) => c t -> c t
softmax m = scale  (1/bunbo) newM 
    where maxValue = maxElement m
          newM     = cmap  (\x -> exp $ x - maxValue ) m
          bunbo    = sumElements newM

softmaxBatch' :: Matrix Double -> Matrix Double
softmaxBatch' m  =  m1
    where list  = toRows m
          list1 = fmap softmax list
          m1 = fromRows list1

cee :: Matrix Double -> Matrix Double -> Double
cee t y = (-1.0)*e
    where delta = 0.0000001
          y1 = cmap (\x -> log (x + delta) ) y
          y2 = flatten y1
          y3 = diag y2
          e = sumElements (t Numeric.LinearAlgebra.<> y3)

----------------------------------------------------------------------


checkIt (e0, net) (x ,t) = (e, newNetwork)
    where ans' = propagateNetwork x net
          ans  = map outData ans'
          answer  = last ans
          answer2 = errorNetwork answer t
          e = errorValue answer2
          finalAnswer' = backpropagateNetwork answer2 ans'
          finalA = map dParams $ init finalAnswer'
          newNetwork = updateNetowrk' net finalA

checking net0 lt = ans
    where ans = scanl checkIt net0 lt 

checking' net0 lt = ans
    where ans = foldl checkIt net0 lt 


-------------------------------------------------------------
--Parameter
-------------------------------------------------------------

data_size = 10000
batch_size = 100
trials_num = 2000

input_size  = 784
hidden_size = 50
output_size = 10

------------------------------------------------

-------------------------------------------------------------
--Initial Condition (?)
-------------------------------------------------------------

nw1 = randomMatrix input_size  hidden_size (mkStdGen 101) :: (Matrix Double)
nw2 = randomMatrix hidden_size output_size (mkStdGen 102) :: (Matrix Double)

nb1 = matrix hidden_size $ take (batch_size * hidden_size) $ repeat 0.0
nb2 = matrix output_size $ take (batch_size * output_size) $ repeat 0.0

nv_w1 =  matrix hidden_size $ take (input_size  * hidden_size) $ repeat 0.0
nv_w2 =  matrix output_size $ take (hidden_size * output_size) $ repeat 0.0

nv_b1 =  matrix hidden_size $ take (batch_size * hidden_size) $ repeat 0.0
nv_b2 =  matrix output_size $ take (batch_size * output_size) $ repeat 0.0

------------------------------------------------

param1 = Params {bias = nb1, weight = nw1}
layer1 = LayerProp {params = param1, af = reluFunc}
hprop1 = HyperProp { vw0 = nv_w1, vb0 = nv_b1 }

param2 = Params {bias = nb2, weight = nw2}
layer2 = LayerProp {params = param2, af = identity }
hprop2  = HyperProp { vw0 = nv_w2, vb0 = nv_b2}

hyperParam = Hyperparameter {learningrate = 0.0001, alpha0 = 0.9, beta0 = 0.0, beta1 = 0.0}


networkA = Network {layers = [layer1,layer2], hpara = hyperParam, hprop = [hprop1,hprop2]}

--makeBatchList gen num_of_trials data_size batch_size
batch_List = makeBatchList 100000 trials_num data_size batch_size

main3 = do

    z <- getZonedTime
    print z

    x <- getCurrentTime

    contents1 <- B.readFile "MNIST/train-labels.idx1-ubyte" 
    contents2 <- B.readFile "MNIST/train-images.idx3-ubyte"

    let tL4 = loadL contents1
        --tL4 = [[Batch X 10],[Batch X 10],...] :: [Matrix Double]

        tI4 = loadI contents2
        --tI4 = [[Batch X data_size],[Batch X data_size],...] :: [Matrix Double]

        test_Image_and_Label = zip tI4 tL4

        --e1 = checking' (0.0,networkA) test_Image_and_Label
        --e2 =  fst e1

        e1' =  checking (0.0,networkA) test_Image_and_Label
        e2' =  tail $ map fst e1'
        net1' = last $ map snd e1'

        
    --print e2
    writeFile "logs/e_mmt-0.0001.dat" $ encorderForDat2 e2'
    
    contents3 <- B.readFile "MNIST/t10k-labels.idx1-ubyte" 
    contents4 <- B.readFile "MNIST/t10k-images.idx3-ubyte"

    let xs = loadImageTest contents4
        ts = loadLabelTest contents3
        --a = accuracyNet net1' xs ts
        --acc = accuracyNetwork net1' xs ts
        bs = map toRows $ map bias $ map params $ layers net1'  
        bs' = map (foldl1 add) bs
        bsl = map recip $ map length' bs
        newbs = map asRow $ zipWith scale bsl bs'
        w = map weight $ map params $ layers net1' 
        para = zipWith finPara newbs w
        acfunc = map af $ layers net1'  
        newLayer = zipWith finLayer para acfunc
        finNetwork = Network {layers = newLayer, hpara = Hyperparameter {learningrate = 1.0, alpha0 = 0.0, beta0 = 0.0, beta1 = 0.0},hprop = [hprop1,hprop2]}

        acc = accuracyNetwork finNetwork xs ts
    

    print acc

    

    y <- getCurrentTime
    print$ diffUTCTime y x


----------------------------------------------------------------------  
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------

loopCheck x net t = (e, finalA, newNetwork)
    where ans' = propagateNetwork x net
          ans  = map outData ans'
          answer  = last ans
          answer2 = errorNetwork answer t
          e = errorValue answer2
          finalAnswer' = backpropagateNetwork answer2 ans'
          finalA = map dParams $ init finalAnswer'
          newNetwork = updateNetowrk net finalA

loop _  _  _ 0 = []
loop x network t n = e : loop x newNetwork t (n-1)
    where  (e, finalA, newNetwork) = loopCheck x network t


randomList = randomChoice 10 10 (mkStdGen 10 )

----------------------------------------------------------------------