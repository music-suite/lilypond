
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
    v <- if windows 
        then return "2.16.2" --permission denied in windows for createProcess
        else readProcess exe ["-v"] "" 
    let s = "\\version \"" ++ (last . words . head . lines) v ++ "\" " ++ ex ++ show (pretty m)      
--  putStrLn s
    mapM_ removeIfExists [ly, pdf']
    writeFile ly s

    if windows 
        then do
            putStrLn $ '\n' : cmd
            void $ system cmd
                -- rawSystem exe [ly'] -- createProcess: permission denied
            when b $ void $ system $ "start" ++ " " ++ "\"\"" ++ " " ++ pdf -- weird, that middle empty string isn't necessary on some windows versions/configs or something?
                -- system pdf -- waits for pdf viewer to close
                -- rawSystem "start" [pdf] -- createProcess says no such file/directory as 'start'
                -- rawSystem pdf [] -- would work, but createProcess says exec format error
        else do -- cooler to skip file system
            (Just h_in, _,  _, p) <- createProcess (proc exe ["-o" ++ path, "-"]){ std_in = CreatePipe }
            hSetBinaryMode h_in False
            hPutStr h_in s
            hClose h_in
            print =<< waitForProcess p
            when b $ void $ rawSystem "xdg-open" [pdf]

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
        ly' = wrap "\"" ly
        path = path' ++ "." ++ t
        wrap c s = c ++ s ++ c
        cmd' = wrap "\"" exe ++ " " ++ ly'
        cmd = exe ++ " " ++ ly'

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
