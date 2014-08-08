
{-# LANGUAGE BangPatterns #-}

module Board where

import Data.Word
import Data.Bits
import Data.Char (toUpper)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed


infixl 8 <<<, >>>
(<<<) :: Bits a => a -> Int -> a
(<<<) = unsafeShiftL
(>>>) :: Bits a => a -> Int -> a
(>>>) = unsafeShiftR

infixl 6 .^.
(.^.) :: Bits a => a -> a -> a
(.^.) = xor


type Color = Int

none  :: Color
black :: Color
white :: Color
none  = 0
black = 1
white = 2

oppColor :: Color -> Color
oppColor c = 3 - c

showColor :: Color -> String
showColor c | c == white = "WHITE"
showColor c | c == black = "BLACK"



-- 64bit整数を2つ用いた bitboard によるボードの表現
-- 8x8のマスを各ビットに割り当て、石のあるマスのビットを立てる
data Board = Board { blk :: !Word64, wht :: !Word64 } deriving Show

initBoard :: Board
initBoard = Board 0x0000000810000000 0x0000001008000000


-- 石を置ける位置のビットが立った64bit整数を返す
getMobility :: Word64 -> Word64 -> Word64
getMobility !p !o =
    let !e  = complement $ p .|. o
        !o' = o .&. 0x7e7e7e7e7e7e7e7e
        !r1 = aux p o' 1
        !r8 = aux p o  8
        !r7 = aux p o' 7
        !r9 = aux p o' 9 in
    (r1 .|. r8 .|. r7 .|. r9) .&. e
    where
        aux !p' !m !d1 =
            let !fl1 = p' .|. (m .&. (p' <<< d1))
                !fr1 = p' .|. (m .&. (p' >>> d1))
                !ml1 = m .&. (m <<< d1)
                !mr1 = m .&. (m >>> d1)
                !d2  = d1 + d1
                !fl2 = fl1 .|. (ml1 .&. (fl1 <<< d2))
                !fr2 = fr1 .|. (mr1 .&. (fr1 >>> d2))
                !ml2 = ml1 .&. (ml1 <<< d2)
                !mr2 = mr1 .&. (mr1 >>> d2)
                !d3  = d2 + d2
                !fl3 = fl2 .|. (ml2 .&. (fl2 <<< d3))
                !fr3 = fr2 .|. (mr2 .&. (fr2 >>> d3)) in
            ((fl3 .&. m) <<< d1) .|. ((fr3 .&. m) >>> d1)


-- 第3引数の位置に石を置いた場合に反転する位置のビットが立った64bit整数を返す
getFlip :: Word64 -> Word64 -> Word64 -> Word64
getFlip !p !o !k =
    let !t1 = g1 $! getFlipLine (f1 p) (f1 o) x
        !t2 = g2 $! getFlipLine (f2 p) (f2 o) (7 - y)
        !t3 = g3 $! getFlipLine (f3 p) (f3 $! o .&. (rotateL c5 d3)) x
        !t4 = g4 $! getFlipLine (f4 p) (f4 $! o .&. (rotateR c5 d4)) x in
    t1 .|. t2 .|. t3 .|. t4
    where
        c1 = 0x0101010101010101
        c2 = 0x8080808080808080
        c3 = 0x0102040810204080
        c4 = 0x8040201008040201
        c5 = 0x7e7e7e7e7e7e7e7e
        i = log2 k
        (x, y) = (i .&. 7, i >>> 3)
        d3 = (x + y - 7) .&. 7
        d4 = (y - x)     .&. 7
        f1 b = (b >>> (i - x)) .&. 255
        f2 b = (((b >>> x) .&. c1) * c4) >>> 56
        f3 b = (((rotateR b (d3 <<< 3)) .&. c3) * c1) >>> 56
        f4 b = (((rotateR b (d4 <<< 3)) .&. c4) * c1) >>> 56
        g1 b = b <<< (i - x)
        g2 b = ((b * c4) .&. c2) >>> (7 - x)
        g3 b = rotateL ((b * c1) .&. c3) (d3 <<< 3)
        g4 b = rotateL ((b * c1) .&. c4) (d4 <<< 3)

getFlipLine :: Word64 -> Word64 -> Int -> Word64
getFlipLine !p !o !i =
    let !a = unsafeAt outFlank $! (fromIntegral $! (o .&. 0x7e) <<< 2) + i
        !b = unsafeAt flipped  $! (fromIntegral $! (fromIntegral a .&. p) <<< 3) + i in
    fromIntegral b

outFlank :: UArray Int Word8
outFlank = listArray (0, 511) $ map f [0..511] where
    f :: Int -> Word8
    f a =
        let (o, x) = ((a >>> 3) <<< 1, a .&. 7) in
        if o .&. (bit x) /= 0 then 0 else g o x (+1) .|. g o x (subtract 1)
    g o x y | y x == -1 || y x == 8  = 0
            | o .&. (bit $ y x) /= 0 = h o (y x) y
            | otherwise              = 0
    h o x y | x == -1 || x == 8  = 0
            | o .&. (bit x) /= 0 = h o (y x) y
            | otherwise          = bit x

flipped :: UArray Word64 Word8
flipped = listArray (0, 1095) $ map f [0..1095] where
    f :: Int -> Word8
    f a =
        let (o, x) = (a >>> 3, a .&. 7) in
        if o .&. (bit x) /= 0 then 0 else g o x (+1) 0 .|. g o x (subtract 1) 0
    g o x y r | y x == -1 || y x == 8  = 0
              | o .&. (bit $ y x) == 0 = g o (y x) y (r .|. (bit $ y x))
              | otherwise              = r


-- potential mobility を計算する
getPMobility :: Word64 -> Word64 -> Word64
getPMobility !p !o =
    let f !x !d = (x <<< d) .|. (x >>> d)
        !r1 = f (o .&. 0x7e7e7e7e7e7e7e7e) 1
        !r8 = f (o .&. 0x00ffffffffffff00) 8
        !r7 = f (o .&. 0x007e7e7e7e7e7e00) 7
        !r9 = f (o .&. 0x007e7e7e7e7e7e00) 9 in
    (r1 .|. r8 .|. r7 .|. r9) .&. (complement $ p .|. o)


-- 64文字の文字列から Board を作成する
makeBoard :: String -> Board
makeBoard str = f str 0 0 0 where
    f _     64 b w = Board b w
    f (c:cs) i b w | toUpper c == 'X' = f cs (i + 1) (b + (bit i)) w
                   | toUpper c == 'O' = f cs (i + 1) b (w + (bit i))
                   | otherwise        = f cs (i + 1) b w


-- Board を整形して出力
putBoard :: Board -> IO ()
putBoard b =
    do putStrLn "   | A B C D E F G H "
       putStrLn " --+------------------"
       mapM_ putBoardLine [0..7]
    where
        putC c | c == none  = putStr " "
               | c == black = putStr "X"
               | c == white = putStr "O"
        putBoardLine j =
            do putStr $ " " ++ show (j + 1) ++ " | "
               mapM_ (\i -> let x = i + j * 8
                                c = ((blk b >>> x) .&. 1) + 2 * ((wht b >>> x) .&. 1) in
                            do putC (fromIntegral c) >> putStr " ") [0..7]
               putStrLn ""

putWord :: Word64 -> IO ()
putWord n = putBoard $ Board n 0



-- log2 (bit i) = i を満たす関数
-- log2 x = popCount (x - 1) とするより若干高速か
log2 :: Word64 -> Int
log2 x = unsafeAt t $! fromIntegral $! (x * c) >>> 58 where
    c = 151050438420815295
    t = listArray (0, 63)
        [ 0,  1,  2,  7,  3, 13,  8, 19,  4, 25, 14, 28,  9, 34, 20, 40,
          5, 17, 26, 38, 15, 46, 29, 48, 10, 31, 35, 54, 21, 50, 41, 57,
         63,  6, 12, 18, 24, 27, 33, 39, 16, 37, 45, 47, 30, 53, 49, 56,
         62, 11, 23, 32, 36, 44, 52, 55, 61, 22, 43, 51, 60, 42, 59, 58]
        :: UArray Int Int


