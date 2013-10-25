
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
import Control.Applicative
import Data.Maybe
import System.Directory

import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)
import System.Info
import Data.Char
import Data.List

-- from http://stackoverflow.com/questions/8502201/remove-file-if-it-exists-in-haskell
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

writeMusic :: FilePath -> Bool -> (Music,String,String) -> IO ()
writeMusic path' b (m,ex,t) = do
    flip when (error $ "couldn't find " ++ exe ++ " on system path") =<< isNothing <$> findExecutable exe
    --permission denied in windows for createProcess/readProcess/rawSystem for exe not on PATH
    v <- readProcess exe ["-v"] "" 
    let s = "\\version \"" ++ (last . words . head . lines) v ++ "\" " ++ ex ++ show (pretty m)      
    mapM_ removeIfExists [ly, pdf']

    if False
        then do
            writeFile ly s            
            print =<< rawSystem exe [out,ly]
        else do -- cooler to skip file system
            (Just h_in, _,  _, p) <- createProcess (proc exe [out, "-"]){ std_in = CreatePipe }
            hSetBinaryMode h_in False
            hPutStr h_in s
            hClose h_in
            print =<< waitForProcess p       
            
    when b $ print =<< if windows 
        then system $ "start" ++ " " ++ "\"\"" ++ " " ++ pdf -- weird, that middle empty string isn't necessary on some windows versions/configs or something?
                -- system pdf -- waits for pdf viewer to close
                -- rawSystem "start" [pdf] -- createProcess says no such file/directory as 'start'
                -- rawSystem pdf [] -- would work, but createProcess says exec format error            
        else rawSystem "xdg-open" [pdf] -- probably won't work on osx...

    putStrLn ""     

{-
C:\eflister\lilypond\src>"C:\Program Files (x86)\LilyPond\usr\bin\LilyPond" "tears.tenor recorder in C.ly"
GNU LilyPond 2.16.2
...

C:\eflister\lilypond\src>ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
...
Prelude> import System.Cmd
Prelude System.Cmd> system "\"C:\\Program Files (x86)\\LilyPond\\usr\\bin\\Lilypond\" \"tears.tenor recorder in C.ly\""
...
'C:\Program' is not recognized as an internal or external command,
operable program or batch file.
ExitFailure 1
Prelude System.Cmd> system "\"C:\\Program Files (x86)\\LilyPond\\usr\\bin\\Lilypond\" tears.tenor.recorder.in.C.ly"
GNU LilyPond 2.16.2
...
Prelude System.Cmd> system "\"C:\\Program Files (x86)\\LilyPond\\usr\\bin\\Lilypond\""
GNU LilyPond 2.16.2
...
Prelude System.Cmd> rawSystem "\"C:\\Program Files (x86)\\LilyPond\\usr\\bin\\Lilypond\"" []
*** Exception: "C:\Program Files (x86)\LilyPond\usr\bin\Lilypond": createProcess: permission denied (Permission denied)
Prelude System.Cmd> rawSystem "\"C:\\Program Files (x86)\\LilyPond\\usr\\bin\\Lilypond\"" ["\"tears.tenor recorder in C.ly\""]
*** Exception: "C:\Program Files (x86)\LilyPond\usr\bin\Lilypond": createProcess: permission denied (Permission denied)
-}

  where exe = "lilypond"
        windows = "mingw" `isPrefixOf` (toLower <$> os)
        pdf' = path ++ ".pdf"
        pdf = wrap "\"" pdf'
        ly = path ++ ".ly"
--        ly' = wrap "\"" ly
        path = path' ++ "." ++ t
        wrap c s = c ++ s ++ c
--        cmd' = wrap "\"" exe ++ " " ++ ly'
--        cmd = exe ++ " " ++ ly'
        out = "-o" ++ path
--        out' = "-o=" ++ wrap "\"" path
--        cmd'' = cmd ++ " " ++ out'

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
