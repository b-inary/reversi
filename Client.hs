{-# LANGUAGE BangPatterns #-}

module Main where

import Board
import Search
import Command
import System.Environment (getArgs)
import Network
import Text.Printf
import System.IO
import Control.Monad
import System.Console.GetOpt

data Config
    = Config { host :: HostName,
               port :: PortNumber,
               playerName :: String,
               verbose :: Bool,
               helpMode :: Bool }

defaultConf = Config "localhost" 3000 "Binary-AI" False False

options :: [OptDescr (Config -> Config)]
options =
    [ Option ['v'] ["verbose"]
             (NoArg $ \conf -> conf { verbose = True })
             "verbose mode"
    , Option ['H'] ["host"]
             (ReqArg (\s conf -> conf { host = s }) "HOST")
             "host name of a server"
    , Option ['p'] ["port"]
             (ReqArg (\s conf -> conf { port = fromIntegral (read s :: Int) }) "PORT")
             "port number of a server"
    , Option ['n'] ["name"]
             (ReqArg (\s conf -> conf { playerName = s }) "NAME")
             "player name"
    , Option ['h','?'] ["help"]
             (NoArg (\conf -> conf { helpMode = True }))
             "show this help"
    ]

usageMessage :: String
usageMessage = usageInfo header options
    where
      header =
          "Usage: \n"
          ++ "    reversi -H HOST -p PORT -n NAME ...\n"

parseArg :: [String] -> IO (Config, [String])
parseArg args =
    case getOpt Permute options args of
      (o, n, [])  -> return (foldl (flip ($)) defaultConf o, n)
      (_, _, err) -> ioError (userError (concat err ++ usageMessage))

hGetCommand :: Handle -> IO Command
hGetCommand h =
    do { r <- hGetLine h
       ; putStrLn $ "Received: " ++ r
       ; return $ either error id $ parseCommand r
       }

hGetCommand' :: Handle -> IO Command
hGetCommand' h =
    do { c <- hGetCommand h
       ; case c of
           Empty -> hGetCommand' h
           _     -> return c }



hPutCommand :: Show a => Handle -> a -> IO ()
hPutCommand h c =
    do hPutStr h (show c)
       hPutStr h "\n"
       hFlush h
       putStrLn $ "Sent: " ++ show c

-- History
data OPMove = OMove Mv | PMove Mv

instance Show OPMove where
    show (OMove mv) = "-" ++ show mv
    show (PMove mv) = "+" ++ show mv
type Hist = [OPMove]

showHist :: [OPMove] -> String
showHist hist =
    foldr (\a r -> show a ++ " " ++ r) "" $ reverse hist


showScores :: [(String, (Int, Int, Int))] -> String
showScores scores =
    foldr (\(n,(s,w,l)) r ->
               take (len+1) (n ++ ":" ++ cycle " ")
                    ++ replicate (1 + slen - length (show s)) ' ' ++ show s
                    ++ " (Win " ++ show w ++ ", Lose " ++ show l ++ ")\n" ++ r) "" scores
    where
      len  = maximum $ map (length . fst) scores
      slen = maximum $ map (length . show . (\(a,b,c) -> a) . snd) scores

-- クライアント
client :: Config -> IO ()
client !conf =
    do { putStrLn $ "Connecting to " ++ host conf ++ " " ++ show (port conf)
       ; h <- connectTo (host conf) (PortNumber $ port conf)
       ; putStrLn "Connection Ok."
       ; hPutCommand h $ Open (playerName conf)
       ; waitStart h conf }

waitStart :: Handle -> Config -> IO ()
waitStart !h !conf =
    do { c <- hGetCommand' h
       ; case c of
           Bye scores ->
               putStr $ showScores scores
           Start color opname mytime ->
               do { let board = initBoard
                  ; if color == black then
                        performMyMove h board color [] opname mytime conf
                    else
                        waitOpponentMove h board color [] opname mytime conf }
           _ ->
               error $ "Invalid Command: " ++ show c
       }

performMyMove :: Handle -> Board -> Color -> [OPMove] -> String -> Int -> Config -> IO ()
performMyMove h board color hist opname mytime conf =
    do { pmove <- play board color mytime
       ; board <- doMove board pmove color
       ; hPutCommand h $ Move pmove
       ; when (verbose conf) $ putStrLn $ replicate 80 '-'
       ; when (verbose conf) $ putStrLn ("PMove: " ++ show pmove ++ " " ++ showColor color)
       ; when (verbose conf) (putBoard board)
       ; c <- hGetCommand' h
       ; case c of
           Ack mytime' ->
               waitOpponentMove h board color (PMove pmove:hist) opname mytime' conf
           End wl n m r ->
               procEnd h board color hist opname wl n m r conf
           _ ->
               error $ "Invalid Command: " ++ show c
       }

waitOpponentMove :: Handle -> Board -> Color -> [OPMove] -> String -> Int -> Config -> IO ()
waitOpponentMove h board color hist opname mytime conf =
    do { c <- hGetCommand' h
       ; case c of
           Move omove ->
               do { board <- doMove board omove (oppColor color)
                  ; when (verbose conf) $ putStrLn $ replicate 80 '-'
                  ; when (verbose conf) $ putStrLn ("OMove: " ++ show omove ++ " " ++ showColor color)
                  ; when (verbose conf) (putBoard board)
                  ; performMyMove h board color (OMove omove:hist) opname mytime conf }
           End wl n m r ->
               procEnd h board color hist opname wl n m r conf
           _ ->
               error $ "Invalid Command: " ++ show c
       }

procEnd :: Handle -> Board -> Color -> [OPMove] -> String -> WL -> Int -> Int -> String -> Config -> IO ()
procEnd h board color hist opname wl n m r conf =
    do { case wl of
           Win ->
               putStrLn $ printf "You win! (%d vs. %d) -- %s." n m r
           Lose ->
               putStrLn $ printf "You lose! (%d vs. %d) -- %s." n m r
           Tie ->
               putStrLn $ printf "Draw (%d vs. %d) -- %s." n m r
       ; putStrLn $ printf "Your name: %s (%s)  Oppnent name: %s (%s)."
                      (playerName conf) (showColor color)
                      opname (showColor (oppColor color))
       ; putBoard board
       ; putStrLn $ showHist hist
       ; waitStart h conf }

main :: IO ()
main = withSocketsDo $
       do { args <- getArgs
          ; (conf, rest) <- parseArg args
          ; if helpMode conf then
                putStrLn usageMessage
            else
                client conf }
