{-# LANGUAGE 
   NoMonomorphismRestriction #-}

import Music.Lilypond hiding (Rest, rest)
import Music.Lilypond.IO
import Music.Lilypond.Pitch
import Data.AdditiveGroup

import Data.Ratio
import Numeric.Natural hiding (natural)

import Text.Parsec
--import Text.Parsec.ByteString.Lazy -- haskellDef below seems to preclude this...
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Number -- cabal install parsec-numbers

import Data.Char
-- import Data.Word
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative hiding (many, (<|>))
import Control.Arrow

main = do
  s <- parseFromFile transcript $ f ++ ".dt" -- dt = 'degreeTranscript format?'
  putStrLn $ show s
  writeMusic f m
  where 
    m =     Clef Treble
        ^+^ Time 3 4
        ^+^ Key (PitchClass A Flat) Minor
        ^+^ Sequential [
              Note (NotePitch (Pitch (PitchClass A Sharp, 4)) Nothing {-(Just OctaveCheck)-}) (Just $ 3/4) []
            ]
    f = "tears"

data Transcription = Transcription TranscriptionTitle TranscriptionComposer TranscriptionYear TranscriptionKey TranscriptionTime TranscriptionStart TranscriptionPattern [TranscriptionSection]
  deriving (Eq,Show)
data TranscriptionKey = TranscriptionKey PitchClass Mode
  deriving (Eq,Show)
type TranscriptionTime = Natural
type TranscriptionStart = Ratio Natural
type TranscriptionPattern = String
type TranscriptionTitle = String
type TranscriptionComposer = String
type TranscriptionYear = String
data TranscriptionSection = TranscriptionSection Char [DegreeNote]
  deriving (Eq,Show)
data DegreeNote = DegreeNote Accidental Degree Duration | Rest Duration
  deriving (Eq,Show)
data Degree = First | Second | Third | Fourth | Fifth | Sixth | Seventh
  deriving (Eq,Show,Enum,Bounded)

lexxer = P.makeTokenParser haskellDef -- haskellDef seems to lock us into Strings, no ByteStrings/Text...
whiteSpace = P.whiteSpace lexxer
-- natural' = toNatural . (fromInteger :: Integer -> Word) <$> P.natural lexxer
natural = fromIntegral <$> P.natural lexxer
naturalOrFloat = P.naturalOrFloat lexxer

-- a $> b = a >> return b
($>) = flip (<$)
(<<) = flip (>>)

tryChoice :: [Parser a] -> Parser a
tryChoice = choice . (try <$>)

line = manyTill anyChar newline

transcript :: Parser Transcription
transcript = Transcription <$> line <*> line <*> line <*> key <*> time <*> start <*> pattern <*> many section <* (whiteSpace >> eof)

pitchClass :: Parser PitchClass
pitchClass = whiteSpace >> PitchClass <$> whiteKey <*> accidental

whiteKey :: Parser WhiteKey
whiteKey' = read . pure . toUpper <$> tryChoice (char <$> ws ++ (toLower <$> ws)) -- <?> "WhiteKey"
  where ws = head . show <$> ([minBound .. maxBound]::[WhiteKey])
whiteKey = tryChoice $ (enum <$> ([minBound .. maxBound]::[WhiteKey]))
        where enum s = s <$ (tryChoice $ char <$> [u, toLower u])
               where u = head $ show s

mode :: Parser Mode
mode = whiteSpace >> tryChoice [ string "min" $> Minor
                               , string "maj" $> Major
                               ] -- <?> "Mode"

key :: Parser TranscriptionKey
key = whiteSpace >> TranscriptionKey <$> pitchClass <*> mode

time :: Parser TranscriptionTime
time = whiteSpace >> natural

start :: Parser TranscriptionStart
start = frac

pattern :: Parser TranscriptionPattern
pattern = whiteSpace >> many1 letter

section :: Parser TranscriptionSection
section = try $ TranscriptionSection <$> (whiteSpace >> letter) <*> (many $ tryChoice [degreeNote, rest])

-- factor out whiteSpace >> duration from rest/degreeNote?
degreeNote,rest :: Parser DegreeNote
degreeNote = DegreeNote <$> (whiteSpace >> accidental) <*> degree <*> (whiteSpace >> duration)
rest = Rest <$> (whiteSpace >> char 'R' >> whiteSpace >> duration)

degree :: Parser Degree
degree = fromJust . (flip M.lookup) m <$> (tryChoice $ string <$> M.keys m)
  where m = M.fromList $ zip (show <$> [1..]) ([minBound .. maxBound] :: [Degree])

accidental :: Parser Accidental
accidental = option Natural (tryChoice [
    DoubleFlat  <$ string "bb" -- must try first!
  , Flat        <$ char   'b'
  , Sharp       <$ char   '#'
  , DoubleSharp <$ char   'x'
  ]) -- <?> "Accidental"

duration :: Parser Duration
duration = frac

frac :: (Fractional a) => Parser a
frac' = (fromRational . toRational ||| fromRational . toRational) <$> (whiteSpace >> naturalOrFloat)
frac = fromRational . toRational <$> (whiteSpace >> floating3 False)
