import System.IO
import Data.List
import Text.Printf

rFloat :: String -> Float
rFloat str = read str ::Float

toFloatVector :: String -> [[Float]]
toFloatVector str = map (map rFloat) $ map words $ lines str

dist :: [Float] -> [Float] -> [Float]
dist (x:xs) (y:ys) = (x-y)^2 : dist xs ys
dist _ _  = []

knn :: String -> [Float] -> [[Float]] -> [(Float,String)]
knn name flt1 flt2 = zip (map sqrt $ map sum $ map (dist flt1) flt2) $ repeat name

inputData ::  [Float] -> String-> IO[(Float,String)]
inputData flt str = do file <- readFile $ str ++ ".txt"
                       return $ knn str flt $ toFloatVector file
                       
main :: IO()
main = do 
    test_file <- getLine
    let test_data = concat.concat.map toFloatVector $ words test_file
        name = ["cocoa","chino","rise","chiya","sharo"]
    knndd <- mapM (inputData test_data) name
    let k = 5::Int
        knnd = take k $ sort $ concat knndd
        knnd' = group.sort $ map snd knnd
        result = snd.head.reverse.sort.zip (map length knnd') $ map head knnd'
    print knnd
    printf "最近傍:%s\n" (snd $ head knnd)
    printf "k=%d:%s\n" k result