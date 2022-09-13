{
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module Happy.Happy (parseFile) where

import qualified Data.ByteString as B
import Control.Monad.State (gets)
import Data.Coerce
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Happy.Error
import Happy.Monad
import Happy.Span
import Happy.Token

import Happy.Alex qualified as Alex
import Expr
}

%name parser Expr
%monad { Parser }
%tokentype { Token }
%error { failure }

%token
    '+' { TSymbol (Loc $$ (TOp Add)) }
    '-' { TSymbol (Loc $$ (TOp Sub)) }
    '*' { TSymbol (Loc $$ (TOp Mul)) }
    '/' { TSymbol (Loc $$ (TOp Div)) }
    '(' { TSymbol (Loc $$ TParBeg) }
    ')' { TSymbol (Loc $$ TParEnd) }
    num { TNum $$ }


%left '+' '-'
%left '*' '/'
%%

Expr : Expr '+' Expr  { Bin Add $1 $3 }
     | Expr '-' Expr  { Bin Sub $1 $3 }
     | Expr '*' Expr  { Bin Mul $1 $3 }
     | Expr '/' Expr  { Bin Div $1 $3 }
     | '(' Expr ')'   { $2 }
     | num            { Num (unLoc $1) }

{
failure :: [Token] -> Parser a
failure [] = do
  sp <- location
  src <- gets parseSource
  parseError $ EmptyTokenStream sp src
failure (tok:_) = do
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken tok src

runParser' :: B.ByteString -> Either Happy.Error.ParseError Expr
runParser' xs = do
  runParser [] xs $ do
    toks <- Alex.lexer
    parser toks

parseFile :: FilePath -> IO (Maybe Expr)
parseFile filepath = do
  result <- runParser' <$> B.readFile filepath
   
  either (\err -> fail $ show err) (pure . Just) result
}
