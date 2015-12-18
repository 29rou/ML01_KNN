import System.IO
import System.Environment (getArgs)
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
knn str flt1 flt2 = zip (map sqrt $ map sum $ map (dist flt1) flt2) $ repeat str

inputData ::  [Float] -> String-> IO[(Float,String)]
inputData flt str = do file <- readFile $ "./traind/"++str ++ ".txt"
                       return $ knn str flt $ toFloatVector file
                       
mainLoop ::  Int -> [Float] -> IO String
mainLoop k flt = do let name = ["cocoa","chino","rise","chiya","sharo"]
                    knnd <- mapM (inputData flt) name
                    let knnd' = group.sort $ map snd $ take k $ sort $ concat knnd
                        result = snd.head.reverse.sort.zip (map length knnd') $ map head knnd'
                    return result

main :: IO()
main = do 
    args <- getArgs
    test_file <- readFile $ head args
    let k = 5::Int
    result <- mapM (mainLoop k) (toFloatVector test_file) 
    let miss = fromIntegral (head.sort $ map length $ group $ sort result) ::Float
        all = fromIntegral (length result) :: Float
        accuracy_rate = if (length.group $ sort result) == 1 then 1 else (1 - miss/ all) 
    printf "k=%d:正答率%.2f%%\n" k (accuracy_rate*100)
    print result