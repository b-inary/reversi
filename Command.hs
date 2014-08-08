--{-# LANGUAGE CPP #-}

module Command where

import Board
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)


data Command = Open String
             | End WL Int Int String
             | Move Mv
             | Start Color String Int
             | Ack Int
             | Bye [(String,(Int,Int,Int))]
             | Empty

data WL = Win | Lose | Tie
data Mv = M Int | Pass | GiveUp

instance Show Command where
    show (Open str)      = "OPEN " ++ str
    show (End  wl i j s) = "END " ++ show wl ++ " " ++ show i ++ " " ++ show j
                           ++ " " ++ s
    show (Move mv)       = "MOVE " ++ show mv
    show (Start c s i)   = "START " ++ showColor c ++ " " ++ s ++ " " ++ show i
    show (Ack i)         = "ACK " ++ show i
    show (Bye ls)        = "BYE " ++ unwords (map showScore ls)
        where
          showScore (n,(s,w,l)) = n ++ " " ++ unwords [show s, show w, show l]
        -- where
        --   showBye []         = ""
        --   showBye [(n,i)]    = n ++ " " ++ show i
        --   showBye ((n,i):ls) = n ++ " " ++ show i ++ " "++ showBye ls
    show Empty           = ""

instance Show WL where
    show Win  = "WIN"
    show Lose = "LOSE"
    show Tie  = "TIE"

instance Show Mv where
    show Pass    = "PASS"
    show GiveUp  = "GIVEUP"
    show (M x) = ["ABCDEFGH" !! (x `mod` 8), "12345678" !! (x `div` 8)]

runParser :: ReadP a -> String -> [a]
runParser p s =
    do { (a,[]) <- readP_to_S p s
       ; return a }

parseCommand s =
    case runParser (pCommand <* skipSpaces <* eof) s of
      [c] -> Right c
      _   -> Left  $ "Invalid Message: " ++ s

pCommand =
    ( Open <$
       string "OPEN" <* skipSpaces <*> pNWString)
    +++
    ( End <$
       string "END" <* skipSpaces <*> pWL <* skipSpaces <*>
            pInt <* skipSpaces <*> pInt <* skipSpaces <*> pNWString)
    +++
    ( Move <$
       string "MOVE" <* skipSpaces <*> pMove)
    +++
    ( Start <$
       string "START" <* skipSpaces <*> pColor <* skipSpaces
              <*> pNWString <* skipSpaces <*> pInt )
    +++
    ( Ack <$
       string "ACK" <* skipSpaces <*> pInt )
    +++
    ( Bye <$ string "BYE" <* skipSpaces <*> pByeList )
    +++
    pure Empty

pByeList :: ReadP [(String,(Int,Int,Int))]
pByeList =
    many ((\a b c d -> (a,(b,c,d))) <$>
        pNWString <* skipSpaces <*> pInt <* skipSpaces <*> pInt <* skipSpaces <*> pInt <* skipSpaces)

pNWString = many1 (satisfy (not. isSpace))

pInt :: ReadP Int
pInt = readS_to_P reads


pColor = (black <$ string "BLACK")
         +++
         (white <$ string "WHITE")

pWL = (Win <$ string "WIN")
      +++
      (Lose <$ string "LOSE")
      +++
      (Tie <$ string "TIE")

pMove
    = ( Pass <$ string "PASS"  )
      +++
      ( GiveUp <$ string "GIVEUP" )
      +++
      ( (\i _ j -> M $ i + j * 8) <$> pColumn <*> skipSpaces <*> pRow )
    where
      pColumn = (\c -> fromJust (elemIndex c "ABCDEFGH"))
                    <$> satisfy (`elem` "ABCDEFGH")
      pRow = (\c -> fromJust (elemIndex c "12345678"))
                    <$> satisfy (`elem` "12345678")


