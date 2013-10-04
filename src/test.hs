{-# LANGUAGE 
   NoMonomorphismRestriction #-}

import qualified Music.Lilypond as L
import Music.Lilypond hiding (Rest, rest, Time, Key)
import Music.Lilypond.Pitch

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Number -- cabal install parsec-numbers

import Data.Fixed
import Data.Ratio
import Numeric.Natural hiding (natural)
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad

main = either print debug =<< parseFromFile sectionP (f ++ ".dt")
  where f = "test"
        debug = writeFile (f ++ ".debug") . show

bars :: Duration -> [DegreeNote] -> [DegreeNote]
bars t ns = bars' t 0 ns []
bars' _ _  []     out = reverse out
bars' t t' (n:ns) out = bars' t t'' ns' $ this : out
        where (t'', this, ns') = if now <= t 
                then (mod' now t, n    ,          ns) 
                else (0         , first, second : ns)
              now  = t' + dur n
              (first,second) = splitDur (t - t') n
              splitDur d (DegreeNote c a g o d' _) = (DegreeNote c a g o d True, DegreeNote c a g Nothing (d' - d) False)
              splitDur d (Rest       c       d'  ) = (Rest       c       d     , Rest       c            $ d' - d       )
              dur (DegreeNote _ _ _ _ d _) = d
              dur (Rest _ d) = d

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]
degrees = enum :: [Degree]
whiteKeys = enum :: [WhiteKey]

data Key = Key { 
        pc   :: PitchClass 
      , mode :: Mode
  } deriving (Eq,Show)
type Time = Natural
data Section = Section {
        label :: Char 
      , notes :: [[DegreeNote]]
  } deriving (Eq)
instance Show Section 
  where show (Section l n) = "\nlabel: " ++ pure l ++ "\n" ++ unlines (show <$> n)
data DegreeNote = DegreeNote Column Accidental Degree Octave Duration Tie | Rest Column Duration
  deriving (Eq)
instance Show DegreeNote
  where show (DegreeNote c a d o dur t) = show c
        show (Rest c dur) = show c
data Degree = First | Second | Third | Fourth | Fifth | Sixth | Seventh
  deriving (Eq,Show,Enum,Bounded)
type Octave = Maybe Int
type Tie = Bool

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

line :: Parser String
line = manyTill anyChar newline -- (newline <|> (eof >> return '\n'))

pitchClassP :: Parser PitchClass
pitchClassP = whiteSpace >> PitchClass <$> whiteKeyP <*> accidentalP

whiteKeyP, whiteKeyP' :: Parser WhiteKey
whiteKeyP' = read . pure . toUpper <$> tryChoice (char <$> ws ++ (toLower <$> ws)) -- <?> "WhiteKey"
  where ws = head . show <$> whiteKeys
whiteKeyP = tryChoice (enum' <$> enum)
        where enum' s = s <$ tryChoice (char <$> [u, toLower u])
               where u = head $ show s

sectionP :: Parser Section
sectionP = try $ Section <$> (whiteSpace >> letter <* whiteSpace) <*> (sepEndBy1 parts whiteSpace) <* eof

parts :: Parser [DegreeNote]
parts = do
   col (== 1) "duration must fall at beginning of line in column 1"
   d <- durationP
   ns <- many1 $ tryChoice [degreeNoteP, restP]
   return ns

degreeNoteP,restP :: Parser DegreeNote
degreeNoteP = DegreeNote <$> (whiteSpace >> col (/= 1) err) <*> accidentalP <*> degreeP <*> octaveP <*> pure undefined <*> pure False
restP = Rest <$> (whiteSpace >> col (/= 1) err) <*> (char 'R' >> pure undefined)

err = "first item in line must be duration, not note"

col f s = do 
   c <- fromIntegral . sourceColumn <$> getPosition
   if f c
        then return c
        else unexpected s

degreeP :: Parser Degree
degreeP = fromJust . flip M.lookup m <$> tryChoice (string <$> M.keys m)
  where m = M.fromList $ zip (show <$> [1..]) degrees

accidentalP :: Parser Accidental
accidentalP = option Natural $ tryChoice [
    DoubleFlat  <$ string "bb" -- must try first!
  , Flat        <$ char   'b'
  , Sharp       <$ char   '#'
  , DoubleSharp <$ char   'x'
  ] -- <?> "Accidental"

octaveP :: Parser Octave
octaveP = optionMaybe $ tryChoice [ 
               countChar '+'
  , negate <$> countChar '-'
  ]

countChar = (length <$>) . many1 . char

durationP :: Parser Duration
durationP = (/ 4) <$> frac

frac :: (Fractional a) => Parser a
frac' = (fromRational . toRational ||| fromRational . toRational) <$> naturalOrFloat
frac = fromRational . toRational <$> floating3 False
