{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Happy.Error
  ( -- * Parser Errors
    ParseError (..),
    parseError,

    -- * Error Serialization
    ErrorCode (..),
    SerializedError (..),
    serialize,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError (..))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Happy.Span qualified as S
import Happy.Token qualified as T

--------------------------------------------------------------------------------

data ParseError
  = EmptyTokenStream S.Span BS.ByteString
  | UnexpectedToken T.Token BS.ByteString
  | InvalidLexeme S.AlexSourcePos BS.ByteString
  deriving stock (Eq, Ord, Show)

parseError :: MonadError ParseError m => ParseError -> m a
parseError = throwError

--------------------------------------------------------------------------------

-- | The serialized representation of internal errors.
data SerializedError = SerializedError {_seCode :: ErrorCode, _seMessage :: T.Text, _seSpan :: S.Span}
  deriving (Show)

data ErrorCode
  = EmptyTokenStreamCode
  | UnexpectedTokenCode
  | InvalidLexemeCode
  deriving (Show)

--------------------------------------------------------------------------------

-- | Convert a 'ParserError' into the serialization type
-- 'SerializedError'. We distinguish these types to allow the internal
-- representation and serialation form to diverge structurally.
serialize :: ParseError -> SerializedError
serialize = \case
  EmptyTokenStream sp _src -> SerializedError {_seCode = EmptyTokenStreamCode, _seMessage = "Unexpected end of input.", _seSpan = sp}
  UnexpectedToken tok _src -> SerializedError {_seCode = EmptyTokenStreamCode, _seMessage = "Unexpecteed token.", _seSpan = S.locate tok}
  InvalidLexeme start src -> SerializedError {_seCode = EmptyTokenStreamCode, _seMessage = "Invalid Lexeme.", _seSpan = S.Span start (eofPos src)}

-- | Calculate the 'S.AlexSourcePos' of the EOF.
eofPos :: BS.ByteString -> S.AlexSourcePos
eofPos src = S.AlexSourcePos (length $ BS.split 10 src) (BS.length $ snd $ BS.spanEnd (/= 10) src)
