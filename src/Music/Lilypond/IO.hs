
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
writeMusic path' (m,ex,t) = do
    v <- if windows 
        then return "2.16.2" --permission denied in windows for createProcess
        else readProcess exe ["-v"] "" 
    let s = "\\version \"" ++ (last . words . head . lines) v ++ "\" " ++ ex ++ show (pretty m)      
--  putStrLn s
    writeFile ly s

    if windows 
        then do
            putStrLn cmd
            void $ system cmd
                -- rawSystem exe [ly'] -- createProcess: permission denied
            void $ system $ "start" ++ " " ++ "\"\"" ++ " " ++ pdf -- weird, that middle empty string isn't necessary on some windows versions/configs or something?
                -- system pdf -- waits for pdf viewer to close
                -- rawSystem "start" [pdf] -- createProcess says no such file/directory as 'start'
                -- rawSystem pdf [] -- would work, but createProcess says exec format error
        else do -- cooler to skip file system
            (Just h_in, _,  _, p) <- createProcess (proc exe ["-o" ++ path, "-"]){ std_in = CreatePipe }
            hSetBinaryMode h_in False
            hPutStr h_in s
            hClose h_in
            print =<< waitForProcess p
            void $ rawSystem "xdg-open" [pdf]

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

  where exe = if False && windows then "C:\\Program Files (x86)\\LilyPond\\usr\\bin\\lilypond" -- system doesn't seem to allow spaces in both executable and args (but either independently OK), even w/double or single quotes, so must add to path
                                  else "lilypond"
        windows = True
        pdf = wrap "\"" $ path ++ ".pdf"
        ly = path ++ ".ly"
        ly' = wrap "\"" ly
        path = path' ++ "." ++ t
        wrap c s = c ++ s ++ c
        cmd' = (wrap "\"" exe) ++ " " ++ ly'
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
