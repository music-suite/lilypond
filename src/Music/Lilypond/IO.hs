
{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.IO -- (
  -- )
where

import Music.Lilypond
import System.Cmd
import System.Process
import System.IO
import Text.Pretty
import Control.Monad

-- writeMusic :: FilePath -> Music -> IO ()
writeMusic path (m,ex) = do
    v <- readProcess exe ["-v"] ""
    let s = "\\version \"" ++ (last . words . head . lines) v ++ "\" " ++ ex ++ show (pretty m)      
--  putStrLn s
    writeFile (path ++ ".ly") s
    (Just h_in, _,  _, p) <- createProcess (proc exe ["-o" ++ path, "-"]){ std_in = CreatePipe }
    hSetBinaryMode h_in False
    hPutStr h_in s
    hClose h_in
    print =<< waitForProcess p
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
