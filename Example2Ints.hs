module Main where
import System.Environment

f x y z = (x ^ y)

main :: IO ()
main = do
  (a : b : c : _) <- getArgs
  let n = read a :: Int
  let m = read b :: Int
  let k = read c :: Int
  let s = sum $ map (f n m) [0..k]

  putStrLn $ if even s then "even" else "odd"
