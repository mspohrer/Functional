{-# LANGUAGE NoImplicitPrelude #-}
--import Data.List
import System.Random

import Prelude hiding
  ( (<>), Monoid(..), 
  transpose
  )


-- Pulled from HMW 2
class Monoid a where
  infixr 6 <>
  (<>)   :: a -> a -> a
  mempty :: a

newtype Matrix = M [[Int]]

-----------------------------------------------------------
-----------------------------------------------------------
--                   Default Matrices                    --
-----------------------------------------------------------
-----------------------------------------------------------
-- 3 x 2
v :: Matrix
v =  M [[0,1], [1,2], [2,3]]
-- 3 x 3
x :: Matrix
x =  M [[1..3], [2..4],[3..5]]
-- 3 x 3
y :: Matrix
y =  M [[7..9], [4..6],[1..3]]
-- 2 x 3
z :: Matrix
z =  M [[10..12], [11..13]]

-- Int vesion of Matrices for testing 
v' :: [[Int]]
v' =  [[0,1], [1,2], [2,3]]
x' :: [[Int]]
x' =  [[1..3], [2..4],[3..5]]
y' :: [[Int]]
y' =  [[7..9], [4..6],[1..3]]
z' :: [[Int]]
z' =  [[10..12], [11..13]]

-----------------------------------------------------------
-----------------------------------------------------------
--                     Instances                         --
-----------------------------------------------------------
-----------------------------------------------------------

-- To test Equality
instance Eq Matrix where
  M xs == M ys = xs == ys

-- Best in show (well, probably not the best, but,
-- as I said in my project proposal, this will be the world's
-- worst Matrix Module
instance Show Matrix where
  show (M [[0]])  = show "Matrices' sizes incompatible"
  show (M (x:[])) = "( " ++ printR x ++ " )"
  show (M (x:xs)) = "( " ++ printR x ++ " )\n" ++ show (M xs)

-- because why not?
instance Monoid Matrix where
  (M x) <> (M y) = M (x ++ y)
  mempty         = M [[]]

-----------------------------------------------------------
-----------------------------------------------------------
--                     Functions                         --
-----------------------------------------------------------
-----------------------------------------------------------

-- TODO make commutative
-- Scalar Multiplication
(|*$|) :: Matrix -> Int -> Matrix
(|*$|) (M m) n = M (map (map (*n)) m)

-- Matrix Addition
(|+|) :: Matrix -> Matrix -> Matrix 
(|+|) (M xs) (M ys) = M (add' xs ys)
-- Helper for the Matrix Addition operator
add' :: (Num a) => [[a]] -> [[a]] -> [[a]]
add' (x:[]) (y:_)  = (zipWith (+) x y):[]
add' (x:_) (y:[])  = (zipWith (+) x y):[]
add' (x:xs) (y:ys) = (zipWith (+) x y):(add' xs ys) 

-- Matrix Subtraction
(|-|) :: Matrix -> Matrix -> Matrix 
(|-|) (M xs) (M ys) = M (sub' xs ys)
-- Helper for the Matrix Subtraction operator
sub' :: (Num a) => [[a]] -> [[a]] -> [[a]]
sub' (x:[]) (y:_)  = (zipWith (-) x y):[]
sub' (x:_) (y:[])  = (zipWith (-) x y):[]
sub' (x:xs) (y:ys) = (zipWith (-) x y):(sub' xs ys) 

-- Matrix Transposition
transpose :: Matrix -> Matrix
transpose (M xss) = M (transpose' xss)
-- Helper for the transpose operator.
transpose' :: [[Int]] -> [[Int]]
transpose' xss  
  | (length $ head xss) == 1 = [ head xs | xs <- xss] : []
  | otherwise = [ head xs | xs <- xss] : transpose' ([tail xs | xs <- xss])

-- Matrix Multiplication
(|*|) :: Matrix -> Matrix -> Matrix
(|*|) (M xss) (M yss) = M (multLists xss yss)
-- Helper for Matrix Multiplication
multLists xss yss 
  | (length yss) /= (length $ head xss) = [[0]]
  | otherwise = [[ sum $ zipWith (*) xs ys' | ys' <- (transpose' yss) ] | xs <- xss ]

-- Tensor Product -- here's a fun one for ya
(|@|) :: Matrix -> Matrix -> Matrix
(|@|) (M xss) (M yss) = M (tensor' xss yss)
tensor' :: [[Int]] -> [[Int]] -> [[Int]]
tensor' xss yss = concat [ [ concat [map (*x) ys | x <- xs] | ys <- yss] | xs <- xss] 

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
infixl 7 |@|
infixl 7 |*$|

-- Pretty print each row of the matrix
printR :: (Ord a, Show a, Num a) => [a] -> [Char]
printR (x:[])
  | x < 10     = "   " ++ show x 
  | x < 100    = "  " ++ show x 
  | x < 1000   = " " ++ show x
  | x < -1000  = show x
  | x < -100   = " " ++ show x 
  | x < -10    = "  " ++ show x 
  | otherwise = show x
printR (x:xs)  
  | x < 10     = "   " ++ show x ++ " " ++ printR xs 
  | x < 100    = "  " ++ show x ++ " " ++ printR xs 
  | x < 1000   = " " ++ show x ++ " " ++ printR xs 
  | x < -1000  = show x ++ " " ++ printR xs 
  | x < -100   = " " ++ show x ++ " " ++ printR xs 
  | x < -10    = "  " ++ show x ++ " " ++ printR xs 
  | otherwise = show x ++ " " ++ printR xs 

main :: IO ()
main = do
  putStrLn "\nAs a not to the grader, while I have provided the some"
  putStrLn "funcitonality, I really wish I had more time to play with"
  putStrLn "monads (not in list comprehension) and foldMapping and whatnot.\n\n"
  putStrLn "Welcome to the worlds worst matrix library!"
  putStrLn "I will now give you a quick walkthrough of the functions."
  putStrLn "They will be in the following format:"
  putStrLn "Function: <argument> |operator| <argument>"
  putStrLn "or"
  putStrLn "Function: <function name> <argument>"
  putStrLn "Scalar Multiplication:  <matrix> |*$| <integer>"
  putStrLn "Matrix Addition:        <matrix> |+|  <matrix>"
  putStrLn "Matrix subtraction:     <matrix> |-|  <matrix>"
  putStrLn "Matrix Multiplication:  <matrix> |*|  <matrix>"
  putStrLn "Matrix Tensor Product:  <matrix> |@|  <matrix>"
  putStrLn "Matrix Transposition:   transpose     <matrix>"
  putStrLn "I also implemented a monoidal instance so feel free to"
  putStrLn "use <matrix> <> <matrix> but its functionally kind of"
  putStrLn "pointless.\n"
  putStrLn "The default matrices are:"
  putStrLn "x:"
  print x
  putStrLn "y:"
  print y
  putStrLn "z:"
  print z
  putStrLn "v:"
  print v
-- TODO's
{-
add = do 
  putStrLn "Enter the first matrix"
  m <- getLine
  putStrLn "Enter the second matrix"
  n <- getLine
  show n ++ m

offWeGo :: IO ()
offWeGo n
  | n == 1    = add
  | n == 3    = do 
    putStrLn "Enter the scalar"

  | otherwise = putStrLn "Enter the scalar"

intro :: IO ()
intro = do
  putStrLn "You can use this program but I can almost"
  putStrLn "gurantee you it will go faster if you just call"
  putStrLn "the functions yourself. There are many"
  putStrLn "here but I wasn't trying to rock Haskell's"
  putStrLn "world, just trying to write a simple Haskell"
  putStrLn "program!\n"
  putStrLn "Default Matrices:"
  putStrLn "\nx ="
  print x
  putStrLn "\ny ="
  print y
  putStrLn "\nz ="
  print z
  putStrLn "\nWhat would you like to do? "
  putStrLn "1. Add two matrices"
  putStrLn "2. Subtract a matrix from a matrix"
  putStrLn "3. Multiply a matrix buy a scalar"
  putStrLn "4. Multiply two matrices"

main = do
  intro
 -- choice <- getLine
 -- offWeGo choice
  


randomList :: (Int, Int) -> IO [Int]
randomList interval =
  newStdGen >>= return . unfoldr (Just . randomR interval)

i :: IO ()
i  = do
  ls <- randomList (1, 6)
  let rs = take 10 ls
  putStrLn $ show $ map (+10) rs

ls = randomList (1, 9)

newtype Index = Index (Integer, Integer)
-}
