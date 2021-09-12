module ParserH(parseLiterated) where

import qualified Data.Text as T
import Data.Text(Text,pack,unpack)

import Control.Applicative hiding (some, many)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO

type Parser = Parsec Void Text



plainSpace :: Parser Char
plainSpace = char ' '

indent :: Parser ()
indent = do 
  char ' '
  char ' '
  return ()

nonLineBreak :: Parser Text
nonLineBreak = takeWhileP (Just "non line break") (/='\n')

lineBreak :: Parser Char
lineBreak = char '\n'



slash = char '\\'

lbrace :: Parser Char
lbrace = char '{'

rbrace :: Parser Char
rbrace =  char '}'

code :: Parser Text
code =  string (pack "code")

begin :: Parser Text
begin =  string (pack "begin")

end :: Parser Text
end =  string (pack "end")


codeLine = do
  indent
  x <- nonLineBreak
  lineBreak
  return x

codeBegins :: Parser [a]
codeBegins = do
  slash
  begin
  lbrace
  many plainSpace
  code
  many plainSpace
  rbrace
  return []


codeEnds :: Parser [a]
codeEnds = do
  slash
  end
  lbrace
  many plainSpace
  code
  many plainSpace
  rbrace
  return []

innerCode :: Parser Text
innerCode = do
  codeBegins
  lineBreak
  x <- many codeLine
  codeEnds
  lineBreak
  return $T.snoc (T.intercalate (pack "\n") x ) '\n'

nonSlash :: Parser Text
nonSlash = takeWhile1P (Just "non \\") (/='\\')


parseAll :: Parser Text
parseAll =  nonSlash <|> try innerCode <|> (do slash;return $ pack "\\")

parseLiterated :: String -> String
parseLiterated s = 
  case runParser (many parseAll) "hi" (pack s)  of
    Right x -> unpack $ T.concat x
    Left x ->show x


parseFile = do
  input <- readFile "posts/2021-09-04-some.lhs"
  putStrLn $ parseLiterated input
