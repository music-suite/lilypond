import Music.Lilypond
import Music.Lilypond.IO
import Data.AdditiveGroup
import Data.Ratio
import Numeric.Natural
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Control.Applicative hiding (many)

main = do
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

transcript :: Parser Transcription
transcript = Transcription <$> key <*> time <*> start <*> pattern <*> many section

key :: Parser TranscriptionKey
key = undefined

time :: Parser TranscriptionTime
time = undefined 

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
accidental = undefined

duration :: Parser Duration
duration = undefined
