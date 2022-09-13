{-# LANGUAGE StrictData #-}

module Expr where

import Data.Word


data Expr
    = Bin Op Expr Expr
    | Num Word64
  deriving (Eq, Ord, Show)


data Op
    = Add
    | Sub
    | Mul
    | Div
  deriving (Eq, Ord, Show)
