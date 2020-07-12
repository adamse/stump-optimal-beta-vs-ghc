module Main where
import System.Environment

f x y = (x ^ y)

main :: IO ()
main = do
  (a : b : c : _) <- getArgs
  let n = read a :: Integer
  let m = read b :: Integer
  let res = f n m
  let k = read c :: Int
  let s = sum $ replicate k res

  putStrLn $ if even s then "even" else "odd"
