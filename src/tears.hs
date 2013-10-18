{-# LANGUAGE 
     NoMonomorphismRestriction
   , FlexibleContexts 
   , RankNTypes #-}

-- remove unix dependency in music-dynamics-literal

import qualified Music.Lilypond as L
import Music.Lilypond hiding (Rest, rest, Time, Key)
import Music.Lilypond.IO
import Music.Lilypond.Pitch
import Data.AdditiveGroup

import Text.Parsec
--import Text.Parsec.ByteString.Lazy -- haskellDef below seems to preclude this...
import Text.Parsec.String
import qualified Text.Parsec.Token as P
-- import Text.Parsec.Language (haskellDef)
--import Text.ParserCombinators.Parsec.Number -- cabal install parsec-numbers
import NumberGen -- my generalization of parsec-numbers to ParsecT

import Data.Ratio
import Numeric.Natural hiding (natural)
import Data.Fixed
import Data.Char
import Data.List hiding (transpose)
-- import Data.Word
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import GHC.Exts

main = either print debug =<< parseFromFile transcript (f ++ ".dt") -- dt = 'degreeTranscript format'
  where f = "tears"
        engrave = writeParts f
        debug = writeFile (f ++ ".debug") . show

writeParts f t = do 
  mapM_ (writeMusic f . lily t) v
  writeScore f t
  where v = concat $ (\xs -> if (length xs > 1) then ((\(x@(Voice _ i@(Instrument s _ _)),n) -> x{instrument = i{name = s ++ "(" ++ show n ++ ")"}}) <$> zip xs [1..]) else xs) <$> (groupWith (show . instrument) $ voices t)

writeScore = undefined

lily :: Transcription -> Voice -> (Music,String,String)
lily t v = (m,ex,i)
  where m =     Clef Treble
            ^+^ L.Time (time t) 4
            ^+^ L.Key (transpose (key t) outKey First Natural) (mode $ key t)
            ^+^ Relative (Pitch (PitchClass C Natural, Just $ 2 + (maybe 0 id $ oct $ instrument v))) (sumV (lily' (time t) (key t) outKey <$> sections t))
        ex = " \\header { title = " ++ show (title t) ++ " composer = " ++ show (composer t) ++ " instrument = " ++ show i ++ "}"
        outKey = iKey $ instrument v
        i = show $ instrument v

instance Show Instrument where
  show (Instrument s k oct) = s ++ " in " ++ show k

lily' :: Time -> Key -> PitchClass -> Section -> Music
-- lily' t inKM outK s = Sequential [sumV $ lily'' inKM outK <$> bars (Duration $ (fromIntegral t) / 4) (notes s)]
lily' t inKM outK s = Sequential [L.Rest (Just 1) []]

bars :: Duration -> [DegreeNote] -> [DegreeNote]
bars = undefined
{-
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

lily'' :: Key -> PitchClass -> DegreeNote -> Music
lily'' _    _    (Rest _ dur)             = L.Rest                                      (Just dur) []
lily'' inKM outK (DegreeNote _ a d o dur t) = Note (NotePitch (Pitch (pc', o)) Nothing) (Just dur) [Tie | t] -- silly hlint comprehension trick
        where pc' = transpose inKM outK d a
-}

transpose :: Key -> PitchClass -> Degree -> Accidental -> PitchClass
transpose (Key inK m) outK d a = tone (Key (step (head $ steps outK inK) (PitchClass C Natural)) m) a d

step (0,0) = id
step (0,h) = step (0,h-1) . inc Half
step (w,h) = step (w-1,h) . inc Whole

-- poor strategy, how fix?
steps k1 k2 = filter ((k2 ==) . flip step k1) [(w,h) | w <- [0..5], h <- [0..2]]

diff d1 d2 = pos d2 - pos d1
pos = fromJust . flip elemIndex enum
get xs = (xs !!) . flip mod (length xs)

enum :: (Enum a, Bounded a) => [a]
enum = [minBound .. maxBound]
degrees = enum :: [Degree]
whiteKeys = enum :: [WhiteKey]

data Step = Whole | Half deriving Show
major = [Whole, Whole, Half, Whole, Whole, Whole, Half]
minor = modal 6 major
modal n s = take (length s) $ drop (n-1) $ cycle s

scale k = tone k Natural <$> enum

tone :: Key -> Accidental -> Degree -> PitchClass
tone (Key pc m) acc d = getNote pc (pos d) acc $ 
        case m of Major -> major
                  Minor -> minor

getNote :: PitchClass -> Int -> Accidental -> [Step] -> PitchClass
getNote (PitchClass w a) 0 acc _         = PitchClass w $ enum !! ((pos a) + diff Natural acc)
getNote p                d acc (s:steps) = getNote (inc s p) (d - 1) acc steps

inc s (PitchClass w a) = PitchClass w' a'
        where w' = get enum $ (pos w) + 1
              a' = fix s $ get major $ pos w -- depends on WhiteKeys as [C .. B]
              fix Half Whole = adj (-) a
              fix Whole Half = adj (+) a
              fix _     _    = a -- args are equal
              adj op n = enum !! op (pos n) 1 -- op can't be sectioned to pointfree this?

data Transcription = Transcription {
        title    :: Title 
      , composer :: Composer 
      , year     :: Year 
      , key      :: Key 
      , time     :: Time 
      , start    :: Start 
      , pattern  :: Pattern 
      , voices   :: [Voice]
      , sections :: [Section]
  } deriving (Eq,Show)
data Voice = Voice {
    columnVoice :: Column
  , instrument :: Instrument
  } deriving (Eq,Show) 
data Instrument = Instrument {
      name :: String 
    , iKey :: PitchClass 
    , oct  :: Octave 
  } deriving (Eq)
data Key = Key { 
        pc   :: PitchClass 
      , mode :: Mode
  } deriving (Eq,Show)
type Time = Natural
type Start = Ratio Natural
type Pattern = String
type Title = String
type Composer = String
type Year = String
data Section = Section {
        label :: Char 
      , notes :: [[Element]]
  } deriving (Eq)
instance Show Section 
  where show (Section l n) = "\nlabel: " ++ pure l ++ "\n" ++ unlines (show <$> n)
data DegreeNote = DegreeNote Accidental Degree Octave Tie | Rest
  deriving (Eq,Show)
data Element = Element {
    note     :: DegreeNote
  , columnElement   :: Column
  , duration :: Duration
  } deriving (Eq,Show)
data Degree = First | Second | Third | Fourth | Fifth | Sixth | Seventh
  deriving (Eq,Show,Enum,Bounded)
type Octave = Maybe Int
type Tie = Bool

lexxer = P.makeTokenParser haskellDef' -- haskellDef seems to lock us into Strings, no ByteStrings/Text...
whiteSpace = P.whiteSpace lexxer
-- natural' = toNatural . (fromInteger :: Integer -> Word) <$> P.natural lexxer
natural = fromIntegral <$> P.natural lexxer
naturalOrFloat = P.naturalOrFloat lexxer

-- stolen from http://hackage.haskell.org/package/parsec-3.1.3/docs/src/Text-Parsec-Language.html#haskellStyle
-- since haskellDef locks us in to Strings and Identity
haskellDef' :: P.GenLanguageDef s u m
haskellDef' = P.LanguageDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                , P.nestedComments = True               
                , P.identStart     = undefined -- letter
                , P.identLetter    = undefined -- alphaNum <|> oneOf "_'"
                , P.opStart        = undefined -- P.opLetter haskellDef''
                , P.opLetter       = undefined -- oneOf ":!#$%&*+./<=>?@\\^|-~"
                , P.reservedOpNames= undefined -- []
                , P.reservedNames  = undefined -- []
                , P.caseSensitive  = undefined -- True                
                }

-- a $> b = a >> return b
($>) = flip (<$)

-- tryChoice :: [Parser a] -> Parser a
tryChoice = choice . (try <$>)

line = manyTill anyChar newline -- (newline <|> (eof >> return '\n'))

--transcript :: Parser Transcription
transcript = do 
  title    <- line
  composer <- line
  year     <- line
  key      <- keyP
  time     <- timeP
  start    <- startP
  pattern  <- patternP
  voices   <- many1 voiceP
--  sections <- runPTReader (many1 sectionP <* (whiteSpace >> eof)) (columnVoice <$> voices)
  sections <- runRP (columnVoice <$> voices) (many1 sectionP) 
  whiteSpace >> eof
  return $ Transcription title composer year key time start pattern voices sections

-- shouldn't this be parsec's =<<?
runPTReader p v = either (error . show) id <$> flip runReader v <$> (runParserT p <$> getState <*> (sourceName <$> getPosition) <*> getInput)
           
-- from saizan           
mapParsecT :: (Functor m, Functor n, Monad m, Monad n) => (forall a. m a -> n a) -> ParsecT s u m a -> ParsecT s u n a
mapParsecT f p = mkPT $ \ s -> f $ (f <$>) <$> runParsecT p s

--runRP :: r -> ParsecT s u (Reader r) a -> Parsec s u a
runPTR r = mapParsecT $ flip runReaderT r

--voiceP :: Parser Voice
voiceP = try $ whiteSpace >> Voice <$> col (/= 1) "voice spec can't be in column 1 (which is for durations)" <*> instrumentP

--instrumentP :: Parser Instrument
instrumentP = Instrument <$> manyTill anyChar (try $ string " in ") <*> pitchClassP <*> octaveP

--pitchClassP :: Parser PitchClass
pitchClassP = whiteSpace >> PitchClass <$> whiteKeyP <*> accidentalP

--whiteKeyP, whiteKeyP' :: Parser WhiteKey
whiteKeyP' = read . pure . toUpper <$> tryChoice (char <$> ws ++ (toLower <$> ws)) -- <?> "WhiteKey"
  where ws = head . show <$> whiteKeys
whiteKeyP = tryChoice (enum' <$> enum)
        where enum' s = s <$ tryChoice (char <$> [u, toLower u])
               where u = head $ show s

--modeP :: Parser Mode
modeP = whiteSpace >> tryChoice [ string "min" $> Minor
                                , string "maj" $> Major
                                ] -- <?> "Mode"

--keyP :: Parser Key
keyP = whiteSpace >> Key <$> pitchClassP <*> modeP

--timeP :: Parser Time
timeP = whiteSpace >> natural

--startP :: Parser Start
startP = frac

--patternP :: Parser Pattern
patternP = whiteSpace >> many1 letter

--sectionP :: Parser Section
--sectionP :: (Eq b, Num b, MonadReader [b] m) => ParsecT String u m Section
sectionP :: ParsecT String u (Reader [Column]) Section
sectionP = try $ Section <$> (whiteSpace >> letter <* whiteSpace) <*> (sepEndBy1 parts whiteSpace)

--parts :: Parser [Element]
--parts :: (Eq b, Num b, MonadReader [b] m) => ParsecT s u m [Element]
--parts :: ParsecT s u (Reader [Column]) [Element]
parts = do
   col (== 1) "duration must fall at beginning of line in column 1"
   d <- durationP
   ns <- many1 $ tryChoice [degreeNoteP, restP]
   return $ (\(c,x) -> Element x c d) <$> ns

-- degreeNoteP,restP :: Parser (Column, DegreeNote)
--degreeNoteP,restP :: (Eq a, Num a) => ReaderT [a] (ParsecT String () Identity) (a, DegreeNote)
degreeNoteP = w' $ DegreeNote <$> accidentalP <*> degreeP <*> octaveP <*> pure False
restP = w' $ Rest <$ char 'R'

--w' :: (Eq a, Num a, Stream s m Char) => ParsecT s u m b -> ParsecT s u (Reader [a]) (a, b)
w' = (((,) <$> (whiteSpace >> col (/= 1) "first item in line must be duration, not note") <* (flip col "non-part column, do you have some naughty tabs?" . flip elem =<< lift ask) ) <*>) 

--w :: (Eq a, Num a) => b -> ParsecT String u (Reader [a]) (a, b)
w d = do
  c <- whiteSpace >> col (/= 1) "first item in line must be duration, not note"
  cs <- ask
  col (flip elem cs) "non-part column, do you have some naughty tabs?"
  return (c,d)

--col :: (Num b, Stream s m t) => (b -> Bool) -> String -> ParsecT s u m b
col f s = do 
   c <- fromIntegral . sourceColumn <$> getPosition
   if f c then return c
          else unexpected s

-- degreeP :: Parser Degree
degreeP = fromJust . flip M.lookup m <$> tryChoice (string <$> M.keys m)
  where m = M.fromList $ zip (show <$> [1..]) degrees

-- accidentalP :: Parser Accidental
accidentalP = option Natural $ tryChoice [
    DoubleFlat  <$ string "bb" -- must try first!
  , Flat        <$ char   'b'
  , Sharp       <$ char   '#'
  , DoubleSharp <$ char   'x'
  ] -- <?> "Accidental"

-- octaveP :: Parser Octave
octaveP = optionMaybe $ tryChoice [ 
               countChar '+'
  , negate <$> countChar '-'
  ]

countChar = (length <$>) . many1 . char

--durationP :: Parser Duration
durationP = (/ 4) <$> frac

--frac :: (Fractional a) => Parser a
--frac = (fromRational . toRational ||| fromRational . toRational) <$> (whiteSpace >> naturalOrFloat) -- requires leading 0. :(
frac = fromRational . toRational <$> floating3 False -- text.numbers didn't use parsect :(
