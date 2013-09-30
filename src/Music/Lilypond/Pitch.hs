
{-# LANGUAGE 
        OverloadedStrings #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------

module Music.Lilypond.Pitch (
        Pitch(..),
        PitchClass(..),
        WhiteKey(..),
        Accidental(..),
        Octaves(..),
        Mode(..),
        OctaveCheck(..),
  ) where

import Data.Char
import Text.Pretty hiding (Mode)
import Music.Pitch.Literal

data WhiteKey = C | D | E | F | G | A | B -- don't change this order!  our transposing depends on it.
    deriving (Eq, Ord, Show, Enum, Read, Bounded)

newtype Pitch = Pitch { getPitch :: (PitchClass, Octaves) }
    deriving (Eq, Ord, Show)

data PitchClass = PitchClass WhiteKey Accidental -- see wikipedia, pitch class includes accidental
    deriving (Eq, Ord, Show, Bounded {-, Enum-}) -- can't derive Enum even though isomorphic to (Enum,Enum)?
-- deriving instance Enum PitchClass

instance Pretty PitchClass where
    pretty (PitchClass w a) = string $ pc w ++ acc a
        where
--            pc C = "c" ; pc D = "d" ; pc E = "e" ; pc F = "f"
--            pc G = "g" ; pc A = "a" ; pc B = "b"     
            pc = fmap toLower . show       
            acc DoubleFlat = "eses"
            acc Flat = "es"
            acc Natural = ""
            acc Sharp = "is"
            acc DoubleSharp = "isis"

instance Pretty Pitch where
    pretty (Pitch (c,o)) = pretty c <+> (string $ oct (o-4))
        where
            oct n | n <  0  =  concat $ replicate (negate n) ","
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate n "'"

instance IsPitch Pitch where
    fromPitch (PitchL (c, Nothing, o)) = Pitch (PitchClass (toEnum c) Natural           , o)                 
    fromPitch (PitchL (c, Just a, o))  = Pitch (PitchClass (toEnum c) (toEnum $ round a), o)

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp  -- don't change this order!  our transposing depends on it.
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Number of octaves raised (positive) or flattened (negative).
type Octaves    = Int 

-- | Mode (for key signatures).
data Mode = Major | Minor
    deriving (Eq, Show)

instance Pretty Mode where
    pretty Major = "\\major"
    pretty Minor = "\\minor"

data OctaveCheck = OctaveCheck
    deriving (Eq, Show)

