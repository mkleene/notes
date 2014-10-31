{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

rawStr = between quote quote stringChars where
	    quote = char '"'
	    normalChar = noneOf "\"\\"
	    stringChars = many1 $ normalChar <|> escapeSeq
	    escapeSeq = char '\\' *> oneOf "\"\\" 

ws parser = spaces *> parser <* spaces

str = JString <$> rawStr 
 
nl = (\_ -> JNull) <$> string "null"	

nm = JNumber . read <$> many1 digit

bl = JBool . ((==) "true") <$> bVal where 
    bVal = string "true" <|> string "false" 

arr = JArray <$> between (char '[') (char ']') (sepBy val $ char ',')

obj = JObject <$> between (char '{') (char '}') pairs where
    pair = (,) <$> ws rawStr <* char ':' <*> val
    pairs = sepBy pair (char ',')

val = ws $ str <|> nl <|> nm <|> bl <|> arr <|> obj
	
parseVal:: String -> Either ParseError JValue
parseVal s = parse val "At the root" s
