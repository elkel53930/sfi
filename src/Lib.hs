module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Map
import Type

program :: [Block]
program = [
    Mov "x" (Num 10)
  , Get "y"
  , Mov "x" (Add (Var "x") (Var "y"))
  , Loop (Num 10) [Mov "x" (Sub (Var "x") (Num 1))]
  , If (Eq (Num 2) (Var "x")) [PutS "x is just ",Put (Num 2)] []
  , Put (Var "x")
  ]

someFunc :: IO ()
someFunc = do
  putStrLn "start"
  execs empty program
  putStrLn "end"

eval :: State -> Expression -> Int
eval st (Num n) = n
eval st (Var n) = st ! n
eval st (Add e1 e2) = eval st e1 + eval st e2
eval st (Sub e1 e2) = eval st e1 - eval st e2
eval st (Eq  e1 e2) = if eval st e1 == eval st e2 then 1 else 0

exec :: State -> Block -> IO State
exec st (Mov v e) = return $ insert v (eval st e) st
exec st (Put e)   = do
  putStrLn $ show (eval st e)
  return st
exec st (PutS s) = do
  putStrLn s
  return st
exec st (Get v)   = do
  n <- getLine
  return $ insert v (read n) st
exec st (Loop e bs) = do
  let l = eval st e
  foldM exec st . concat $ replicate l bs
exec st (If e th el) = do
  let c = eval st e
  if c /= 0
    then execs st th
    else execs st el

execs :: State -> [Block] -> IO State
execs s bs = foldM exec s bs
