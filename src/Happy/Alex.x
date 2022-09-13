{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE StrictData #-}
module Happy.Alex where

import Expr
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Word

import Control.Monad.State (gets)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Happy.Token
import Happy.Error
import Happy.Monad
import Happy.Span
}

$digit = 0-9

token :-
  $white+      ;
  \+           { symbol $ TOp Add }
  \-           { symbol $ TOp Sub }
  \*           { symbol $ TOp Mul }
  \/           { symbol $ TOp Div }
  \(           { symbol $ TParBeg }
  \)           { symbol $ TParEnd }
  $digit+  { token (\loc -> TNum (read . T.unpack <$> loc)) }


{
-- | The monadic wrapper for 'alexScan'. The 'Parser' type is defined
-- in 'Happy.Monad' as a transformer stack of 'StateT' and
-- 'Except'. The Start Code Stack, current 'Span', and 'AlexInput' are
-- tracked in 'StateT'.
--
-- 'scan' recursively consumes the consumes the 'AlexInput' from state
-- until it produces a 'Token', an error, or reaches the end of the
-- file.
scan :: Parser Token
scan = do
  input <- getInput
  code <- startCode
  src <- gets parseSource
  sp <- location
  case alexScan input code of
    AlexEOF -> pure (TEndOfFile sp)
    AlexError (AlexInput pos _ _ _) ->
      parseError $ InvalidLexeme pos src
    AlexSkip rest _ -> do
      advance rest
      scan
    AlexToken rest nbytes action -> do
      advance rest
      action (slice nbytes input)

-- | The entry point to the lexer. Recursively calls 'scan' to yield
-- tokens unti we hit EOF.
lexer :: Parser [Token]
lexer = do
  tok <- scan
  case tok of
    TEndOfFile _ -> pure []
    x -> (x :) <$> lexer

}

