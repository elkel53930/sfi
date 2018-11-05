module Type where

import Data.Map

type VarName = String

data Expression =
    Num Int
  | Var VarName
  | Add Expression Expression
  | Sub Expression Expression
  | Eq  Expression Expression

data Block =
    Mov VarName Expression
  | Put Expression
  | PutS String
  | Get VarName
  | Loop Expression [Block]
  | If Expression [Block] [Block]

type State = Map VarName Int
