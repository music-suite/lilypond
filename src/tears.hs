import Music.Lilypond
import Music.Lilypond.IO
import Data.AdditiveGroup
import Data.Ratio
import Numeric.Natural hiding (natural)
import Text.Parsec
--import Text.Parsec.ByteString.Lazy
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative hiding (many, (<|>))
import Data.Char

main = do
  putStrLn . show $ parse key "" "bbb maj {- hi -} 12"
  s <- parseFromFile transcript "tears.dt"
  putStrLn $ show s
  writeMusic {-"/home/hiccup/Desktop/tearsOut"-} "tearsOut" m
  where 
    m =     Clef Treble
        ^+^ Time 3 4
        ^+^ Key (PitchClass A Flat) Minor
        ^+^ Sequential [
              Note (NotePitch (Pitch (PitchClass A Sharp, 4)) Nothing {-(Just OctaveCheck)-}) (Just $ 3/4) []
            ]

data Transcription = Transcription TranscriptionKey TranscriptionTime TranscriptionStart TranscriptionPattern [TranscriptionSection]
  deriving (Eq,Show)
data TranscriptionKey = TranscriptionKey PitchClass Mode
  deriving (Eq,Show)
type TranscriptionTime = Natural
type TranscriptionStart = Ratio Natural
type TranscriptionPattern = String
data TranscriptionSection = TranscriptionSection Char [DegreeNote]
  deriving (Eq,Show)
data DegreeNote = DegreeNote Degree Accidental Duration
  deriving (Eq,Show)
data Degree = First | Second | Third | Fourth | Fifth | Sixth | Seventh
  deriving (Eq,Show,Enum,Bounded)

lexxer = P.makeTokenParser haskellDef
whiteSpace = P.whiteSpace lexxer
natural = toNatural . fromInteger <$> P.natural lexxer

-- a $> b = a >> return b
($>) = flip (<$)

tryChoice :: [Parser a] -> Parser a
tryChoice = choice . map try

transcript :: Parser Transcription
transcript = Transcription <$> key <*> time <*> start <*> pattern <*> many section

pitchClass :: Parser PitchClass
pitchClass = whiteSpace >> PitchClass <$> whiteKey <*> accidental

{-
whiteKey :: Parser WhiteKey
whiteKey = tryChoice $ map enum ([minBound .. maxBound]::[WhiteKey])
        where enum s = s <$ string (map toLower $ show s)
-}

whiteKey :: Parser WhiteKey
whiteKey = read . pure . toUpper <$> anyChar <?> "WhiteKey"

mode :: Parser Mode
mode = whiteSpace >> tryChoice [ string "min" $> Minor
                               , string "maj" $> Major
                               ] <?> "Mode"

key :: Parser TranscriptionKey
key = whiteSpace >> TranscriptionKey <$> pitchClass <*> mode

time :: Parser TranscriptionTime
time = natural

start :: Parser TranscriptionStart
start = undefined

pattern :: Parser TranscriptionPattern
pattern = undefined

section :: Parser TranscriptionSection
section = undefined

degreeNote :: Parser DegreeNote
degreeNote = undefined

degree :: Parser Degree
degree = undefined

accidental :: Parser Accidental
accidental = option Natural (tryChoice [
    DoubleFlat  <$ string "bb" -- must try first!
  , Flat        <$ char   'b'
  , Sharp       <$ char   '#'
  , DoubleSharp <$ char   'x'
  ]) <?> "Accidental"

duration :: Parser Duration
duration = undefined
