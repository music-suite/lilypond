{-# LANGUAGE 
     NoMonomorphismRestriction
   #-}


--copied from http://hackage.haskell.org/package/parsec-numbers-0.0.4/docs/src/Text-ParserCombinators-Parsec-Number.html
--generalized to ParsecT

module NumberGen where

import Text.Parsec
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Control.Monad (liftM, ap)

-- * floats

-- | parse a decimal unsigned floating point number containing a dot, e or E
--floating :: Floating f => CharParser st f
floating = do
  n <- decimal
  fractExponent n

-- | parse a floating point number possibly containing a decimal dot, e or E
--floating2 :: Floating f => Bool -> CharParser st f
floating2 = liftM (either fromInteger id) . decFloat

{- | parse a floating point number possibly starting with a decimal dot.
Note, that a single decimal point or a number starting with @.E@ is illegal.
-}
--floating3 :: Floating f => Bool -> CharParser st f
floating3 b = genFractAndExp 0 (fraction True) exponentFactor <|> floating2 b

{- | same as 'floating' but returns a non-negative integral wrapped by Left if
a fractional part and exponent is missing -}
--decimalFloat :: (Integral i, Floating f) => CharParser st (Either i f)
decimalFloat = decFloat True

{- | same as 'floating' but returns a non-negative integral wrapped by Left if
a fractional part and exponent is missing -}
--decFloat :: (Integral i, Floating f) => Bool -> CharParser st (Either i f)
decFloat b = do
  n <- decimal
  option (Left n) $ liftM Right $ fractExp (toInteger n) b

-- | parse a hexadecimal floating point number
--hexFloat :: (Integral i, Floating f) => Bool -> CharParser st (Either i f)
hexFloat b = do
  n <- hexnum
  option (Left n) $ liftM Right $ hexFractExp (toInteger n) b

-- | parse hexadecimal, octal or decimal integrals or 'floating'
--natFloat :: (Integral i, Floating f) => CharParser st (Either i f)
natFloat = (char '0' >> zeroNumFloat) <|> decimalFloat

-- ** float parts

{- | parse any hexadecimal, octal, decimal or floating point number following
a zero -}
--zeroNumFloat :: (Integral i, Floating f) => CharParser st (Either i f)
zeroNumFloat =
  liftM Left hexOrOct
  <|> decimalFloat
  <|> liftM Right (fractExponent 0)
  <|> return (Left 0)

-- | parse a floating point number given the number before a dot, e or E
--fractExponent :: Floating f => Integer -> CharParser st f
fractExponent i = fractExp i True

-- | parse a hex floating point number given the number before a dot or p
--hexFractExp :: Floating f => Integer -> Bool -> CharParser st f
hexFractExp i b = genFractExp i (hexFraction b) hexExponentFactor

-- | parse a floating point number given the number before a dot, e or E
--fractExp :: Floating f => Integer -> Bool -> CharParser st f
fractExp i b = genFractExp i (fraction b) exponentFactor

{- | parse a floating point number given the number before the fraction and
exponent -}
--genFractExp :: Floating f => Integer -> CharParser st f
--  -> CharParser st (f -> f) -> CharParser st f
genFractExp i frac expo = case fromInteger i of
  f -> genFractAndExp f frac expo <|> liftM ($ f) expo

{- | parse a floating point number given the number before the fraction and
exponent that must follow the fraction -}
--genFractAndExp :: Floating f => f -> CharParser st f
--  -> CharParser st (f -> f) -> CharParser st f
genFractAndExp f frac = ap (liftM (flip id . (f +)) frac) . option id

-- | parse a floating point exponent starting with e or E
--exponentFactor :: Floating f => CharParser st (f -> f)
exponentFactor = oneOf "eE" >> extExponentFactor 10 <?> "exponent"

-- | pare a hexadecimal floating point starting with p (IEEE 754)
--hexExponentFactor :: Floating f => CharParser st (f -> f)
hexExponentFactor = char 'p' >> extExponentFactor 2 <?> "hex-exponent"

{- | parse a signed decimal and compute the exponent factor given a base.
For hexadecimal exponential notation (IEEE 754) the base is 2 and the
leading character a p. -}
--extExponentFactor :: Floating f => Int -> CharParser st (f -> f)
extExponentFactor base =
  liftM (flip (*) . exponentValue base) (ap sign (decimal <?> "exponent"))

{- | compute the factor given by the number following e or E. This
implementation uses @**@ rather than @^@ for more efficiency for large
integers. -}
--exponentValue :: Floating f => Int -> Integer -> f
exponentValue base = (fromIntegral base **) . fromInteger

-- * fractional numbers (with just a decimal point between digits)

-- | parse a fractional number containing a decimal dot
--fractional :: Fractional f => CharParser st f
fractional = do
  n <- decimal
  fractFract n True

-- | parse a fractional number possibly containing a decimal dot
--fractional2 :: Fractional f => Bool -> CharParser st f
fractional2 = liftM (either fromInteger id) . decFract

-- | parse a fractional number possibly starting with a decimal dot
--fractional3 :: Fractional f => Bool -> CharParser st f
fractional3 b = fractFract 0 True <|> fractional2 b

-- | a decimal fractional
--decFract :: (Integral i, Fractional f) => Bool -> CharParser st (Either i f)
decFract b = do
  n <- decimal
  option (Left n) $ liftM Right $ fractFract (toInteger n) b

-- | a hexadecimal fractional
--hexFract :: (Integral i, Fractional f) => Bool -> CharParser st (Either i f)
hexFract b = do
  n <- hexnum
  option (Left n) $ liftM Right $ genFractFract (toInteger n) $ hexFraction b

{- | same as 'fractional' but returns a non-negative integral wrapped by Left if
a fractional part is missing -}
--decimalFract :: (Integral i, Fractional f) => CharParser st (Either i f)
decimalFract = decFract True

-- | parse hexadecimal, octal or decimal integrals or 'fractional'
--natFract :: (Integral i, Fractional f) => CharParser st (Either i f)
natFract = (char '0' >> zeroNumFract) <|> decimalFract

{- | parse any hexadecimal, octal, decimal or fractional number following
a zero -}
--zeroNumFract :: (Integral i, Fractional f) => CharParser st (Either i f)
zeroNumFract =
  liftM Left hexOrOct
  <|> decimalFract
  <|> liftM Right (fractFract 0 True)
  <|> return (Left 0)

-- ** fractional parts

-- | parse a fractional number given the number before the dot
--fractFract :: Fractional f => Integer -> Bool -> CharParser st f
fractFract i = genFractFract i . fraction

{- | combine the given number before the dot with a parser for the fractional
part -}
--genFractFract :: Fractional f => Integer -> CharParser st f -> CharParser st f
genFractFract i = liftM (fromInteger i +)

-- | parse a dot followed by decimal digits as fractional part
--fraction :: Fractional f => Bool -> CharParser st f
fraction b = baseFraction b 10 digit

-- | parse a dot followed by hexadecimal digits as fractional part
--hexFraction :: Fractional f => Bool -> CharParser st f
hexFraction b = baseFraction b 16 hexDigit

-- | parse a dot followed by base dependent digits as fractional part
--baseFraction :: Fractional f => Bool -> Int -> CharParser st Char
--  -> CharParser st f
baseFraction requireDigit base baseDigit = char '.' >>
  liftM (fractionValue base)
    ((if requireDigit then many1 else many) baseDigit <?> "fraction")
  <?> "fraction"

{- | compute the fraction given by a sequence of digits following the dot.
Only one division is performed and trailing zeros are ignored. -}
--fractionValue :: Fractional f => Int -> String -> f
fractionValue base = uncurry (/)
  . foldl (\ (s, p) d ->
           (p * fromIntegral (digitToInt d) + s, p * fromIntegral base))
    (0, 1) . dropWhile (== '0') . reverse

-- * integers and naturals

{- | parse an optional 'sign' immediately followed by a 'nat'. Note, that in
Daan Leijen's code the sign was wrapped as lexeme in order to skip comments
and spaces in between. -}
--int :: Integral i => CharParser st i
int = ap sign nat

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
--sign :: Num a => CharParser st (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

{- | parse plain non-negative decimal numbers given by a non-empty sequence
of digits -}
--decimal :: Integral i => CharParser st i
decimal :: (Monad m, Integral i) => ParsecT [Char] st m i
decimal = number 10 digit

-- | parse a binary number
--binary :: Integral i => CharParser st i
--binary = number 2 $ oneOf "01"

-- | parse non-negative hexadecimal, octal or decimal numbers
--nat :: Integral i => CharParser st i
nat = zeroNumber <|> decimal

-- ** natural parts

-- | parse a 'nat' syntactically starting with a zero
--zeroNumber :: Integral i => CharParser st i
zeroNumber =
  char '0' >> (hexOrOct <|> decimal <|> return 0) <?> ""

-- | hexadecimal or octal number
--hexOrOct :: Integral i => CharParser st i
--hexOrOct = hexadecimal <|> octal
hexOrOct = undefined

-- | parse a hexadecimal number preceded by an x or X character
--hexadecimal :: Integral i => CharParser st i
--hexadecimal = oneOf "xX" >> hexnum

-- | parse a hexadecimal number
--hexnum :: Integral i => CharParser st i
--hexnum = number 16 hexDigit
hexnum = undefined

-- | parse an octal number preceded by an o or O character
--octal :: Integral i => CharParser st i
--octal = oneOf "oO" >> number 8 octDigit

-- | parse a non-negative number given a base and a parser for the digits
--number :: Integral i => Int -> GenParser tok st Char -> GenParser tok st i
number base baseDigit = do
  n <- liftM (numberValue base) (many1 baseDigit)
  seq n (return n)

-- | compute the value from a string of digits using a base
--numberValue :: Integral i => Int -> String -> i
numberValue base =
  foldl (\ x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0

