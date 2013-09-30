
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Music.Lilypond.IO -- (
  -- )
where

import Music.Lilypond
import System.Cmd
import System.Process
import System.IO
import Text.Pretty
import Control.Monad

writeMusic :: FilePath -> Music -> IO ()
writeMusic path m = do
    v <- readProcess exe ["-v"] ""
    let s = "\\version \"" ++ (last . words . head . lines) v ++ "\" " ++ (show $ pretty m)      
--  putStrLn s
    (Just h_in, _,  _, p) <- createProcess (proc exe ["-o" ++ path, "-"]){ std_in = CreatePipe }
    hSetBinaryMode h_in False
    hPutStr h_in s
    hClose h_in
    putStrLn . show =<< waitForProcess p
    void $ rawSystem "xdg-open" [path ++ ".pdf"]
  where exe = "lilypond"

data Format = PDF | PNG | PS

data EngraveOptions
    = EngraveOptions {
        format   :: Format,    
        include  :: FilePath,
        initFile :: FilePath,
        logFile  :: FilePath,
        logLevel :: Int
    }

writeAndEngraveMusic :: FilePath -> EngraveOptions -> Music -> IO ()
writeAndEngraveMusic = error "writeAndEngraveMusic: Not implemented"
