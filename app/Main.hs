module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import System.CPUTime (getCPUTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)

sumOfSquares :: Integer -> Integer
sumOfSquares n = sum' 0 [1..n]
  where
    sum' acc [] = acc
    sum' acc (x:xs) = sum' (acc + x^2) xs

parallelSumOfSquares :: Integer -> Integer
parallelSumOfSquares n = sum $ parMap rdeepseq (\x -> x^2) [1..n]

timeAction :: IO a -> IO (a, NominalDiffTime)
timeAction action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    return (result, diffUTCTime end start)

main :: IO ()
main = do
    let n = 10000000

    putStrLn "Послідовне виконання:"
    (seqResult, seqTime) <- timeAction $ return $ sumOfSquares n
    print seqResult
    putStrLn $ "Час виконання: " ++ show (seqTime) ++ " сек"

    putStrLn "\nПаралельне виконання:"
    (parResult, parTime) <- timeAction $ return $ parallelSumOfSquares n
    print parResult
    putStrLn $ "Час виконання: " ++ show (parTime) ++ " сек"
