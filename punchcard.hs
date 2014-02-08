import System.IO
import System.Environment
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.List

help :: String
help = "USAGE: clock <file> <in|out>"

error :: [String] -> IO ()
error args = do
    pname <- getProgName
    putStrLn $ "ERROR: invalid usage: `" ++ pname ++ ' ':(sperse " " args) ++ "`"
    putStrLn help
        where sperse :: String -> [String] -> String
              sperse _ (x:[]) = x
              sperse c (x:xs) = x ++ c ++ (sperse c xs)

inout :: [String]
inout = ["in","out"]

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> do
            hClockFile <- openFile (args !! 0) AppendMode
            case (args !! 1) `elem` inout of
                True  -> do
                    now <- getCurrentTime
                    let time = formatTime defaultTimeLocale "%c" now
                    hPutStrLn hClockFile $ time ++ ' ':(args !! 1)
                _ -> Main.error args
            hClose hClockFile
        _ -> Main.error args
