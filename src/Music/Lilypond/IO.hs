
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
    v <- if windows 
        then return "2.16.2" --permission denied in windows for createProcess
        else readProcess exe ["-v"] "" 
    let s = "\\version \"" ++ (last . words . head . lines) v ++ "\" " ++ ex ++ show (pretty m)      
--  putStrLn s
    writeFile ly s

    if windows 
        then do
            void $ system $ exe ++ " " ++ ly
                -- rawSystem exe [ly]
            void $ system $ "start" ++ " " ++ pdf
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

  where exe = if windows then "\"C:\\Program Files (x86)\\LilyPond\\usr\\bin\\lilypond\""
                         else "lilypond"
        windows = True
        pdf = path ++ ".pdf"
        ly = path ++ ".ly"

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
