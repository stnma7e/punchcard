import System.IO
import System.Environment
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T

help :: String
help = "USAGE: clock <file> <in|out>"

error :: [String] -> IO ()
error args = do
    pname <- getProgName
    putStrLn $ "ERROR: invalid usage: `" ++ pname ++ ' ':(sperse " " args) ++ "`"
    putStrLn help
        where sperse :: String -> [String] -> String
              sperse _ [] = ""
              sperse _ (x:[]) = x
              sperse c (x:xs) = x ++ c ++ (sperse c xs)

printInOut :: (Show a) => InOut a -> IO String
printInOut s = do
    now <- getCurrentTime
    let time = formatTime defaultTimeLocale "%c" now
    return $ time ++ " " ++ case s of
        i@(In a)  -> "In  " ++ (show a)
        o@(Out a) -> "Out " ++ (show a)
getLastInOut :: (Read a) => String -> InOut a
getLastInOut s = read . concat . reverse . take 2 . reverse . words . last . lines $ s

writeNextPunch :: InOut String -> [String] -> IO ()
writeNextPunch lastPunch args = do
    let event = case length args of
                    1 -> ""
                    2 -> args !! 1
                    _ -> Prelude.error help

    hClockFileAppend <- openFile (args !! 0) AppendMode

    inout <- printInOut . Main.not lastPunch $ event
    hPutStrLn hClockFileAppend inout
    hClose hClockFileAppend

inout :: [String]
inout = ["in","out"]

data InOut a = In a
             | Out a
               deriving (Show, Read)

not :: InOut a -> (a -> InOut a)
not (In _)  = Out
not (Out _) = In

main :: IO ()
main = do
    args <- getArgs
    if (length args) < 1
    then Main.error args
    else do
        now <- getCurrentTime
        let time = formatTime defaultTimeLocale "%c" now

        fileExists <- doesFileExist (args !! 0)
        if fileExists
        then do
            conts <- T.readFile (args !! 0)
            let lastPunch = getLastInOut (T.unpack conts)

            writeNextPunch lastPunch args
        else writeNextPunch (Out "") args
