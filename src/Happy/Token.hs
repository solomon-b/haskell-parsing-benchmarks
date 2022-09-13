{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Happy.Token where

--------------------------------------------------------------------------------

import Data.Char (ord)
import Data.Word
import Expr
import Happy.Span

--------------------------------------------------------------------------------

data Symbol = TParBeg | TParEnd | TOp Op
  deriving (Eq, Ord, Show)

data Token
  = TSymbol (Loc Symbol)
  | TNum (Loc Word64)
  | TEndOfFile Span
  deriving (Eq, Ord, Show)

instance Located Token where
  locate = \case
    TSymbol sym -> locate sym
    TNum n -> locate n
    TEndOfFile s -> s

digit :: Char -> Word64
digit c = fromIntegral $ ord c - ord '0'
