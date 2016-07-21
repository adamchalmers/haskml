import System.IO
import Data.List.Split
import Data.Maybe
import Data.List
import Data.List.Extra hiding (splitOn, chunksOf)

type IrisVec = (Float, Float, Float, Float)
type Label = String
type Model = [Iris]

data Iris = Iris 
    { vec :: IrisVec
    , label :: Label
    }

instance Show Iris where
    show Iris {vec=(a,b,c,d), label=l} = "Iris [" ++l++ "] ("++show a++", "++show b++", "++show c++", "++show d++")"

-- Given a model and an input vector, predicts the vector's label using kNN on the model.
predict :: Int -> Model -> IrisVec -> Label
predict k model inVec = mostCommon $ take k $ map label $ sortOn f model
    where
        f :: Iris -> Float
        f Iris {vec=v} = dist inVec v

-- Distance metric for comparing Iris measurements
dist :: IrisVec -> IrisVec -> Float
dist (a,b,c,d) (p,q,r,s) = (sum $ map (\x -> (abs x) ** 2) [a-p, b-q, c-r, d-s]) ** 0.5

-- Evaluate the accuracy of a prediction method on a dataset
-- eval :: [Iris] -> Float
-- eval dataset =
--     where
--         setSplits = sublistAndRest
        
-- Converts the entire datafile contents to a list of Iris vectors.
parseIris :: String -> [Iris]
parseIris rows = catMaybes $ map (toIrisVec . splitOn ",") (lines rows)
    where
        toIrisVec :: [String] -> Maybe Iris
        toIrisVec (a:b:c:d:species:[]) = Just Iris 
            { vec = (read a, read b, read c, read d)
            , label = species
            }
        toIrisVec _ = Nothing

main = do
    withFile "data/iris/iris.data" ReadMode (\handle -> do
        model <- fmap parseIris $ hGetContents handle
        putStr $ unlines
            [ ("Loaded " ++ (show $ length model) ++ " rows of data.\n")
            , ("Prediction for setosa: " ++ predict 1 model inSeto)
            , ("Prediction for versicolor: " ++ predict 1 model inVers)
            , ("Prediction for virginica: " ++ predict 1 model inVirg)
            ]
        )
    where
        inSeto = (5.8, 4.0, 1.2, 0.2)
        inVers = (6.2, 2.4, 4.5, 1.5)
        inVirg = (6.7, 3.1, 5.6, 2.4)

-- Helper functions

-- Returns the most frequently occuring element of the list.
mostCommon :: (Ord a) => [a] -> a
mostCommon xs = last $ map snd $ sort $ map (\x->(length x, head x)) (group $ sort xs)

ncross :: Int -> [a] -> [([a], [a])]
ncross n xs = map (\i -> parts (i*len)) [0..n-1]  
    where
        len = (round $ (fromIntegral $ length xs) / (fromIntegral n))
        parts i = sublistAndRest len i xs

-- Return the len-length sublist starting at i, and all other elements.
sublistAndRest :: Int -> Int -> [a] -> ([a], [a])
sublistAndRest len i xs = (p0++p2, p1)
    where
        (p, p2) = splitAt (i + len) xs
        (p0, p1) = splitAt i p