
{-# LANGUAGE BangPatterns #-}

module Eval where

import Board
import Data.Word
import Data.Bits
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed


-- パラメータが手打ちなので、評価関数の精度はあまり良くない... --


eval :: Word64 -> Word64 -> Int -> Int
eval !p !o !turn =
    (edgePoint p o - edgePoint o p) * 100
  + (popCount (getMobility  p o) - popCount (getMobility  o p)) * (100 - turn)
  + (popCount (getPMobility p o) - popCount (getPMobility o p)) * (130 - 2 * turn)

edgePoint :: Word64 -> Word64 -> Int
edgePoint !p !o =
    let !t = p .|. o
        !m = (-5) * popCount
            (p .&. 0x0042000000004200
               .&. (complement $! t .&. 0x0000000000000001) <<< 9
               .&. (complement $! t .&. 0x0000000000000080) <<< 7
               .&. (complement $! t .&. 0x0100000000000000) >>> 7
               .&. (complement $! t .&. 0x8000000000000000) >>> 9)
        !e1 = unsafeAt edgeTable (pack (f1 p) (f1 o))
        !e2 = unsafeAt edgeTable (pack (f2 p) (f2 o))
        !e3 = unsafeAt edgeTable (pack (f3 p) (f3 o))
        !e4 = unsafeAt edgeTable (pack (f4 p) (f4 o)) in
    m + e1 + e2 + e3 + e4 where
        f1 x = x .&. 255
        f2 x = x >>> 56
        f3 x = ((x .&. 0x0101010101010101) * 0x0102040810204080) >>> 56
        f4 x = ((x .&. 0x8080808080808080) * 0x0002040810204081) >>> 56

pack :: Word64 -> Word64 -> Int
pack !p !o = unsafeAt t1 (fromIntegral p) + unsafeAt t2 (fromIntegral o) where
    t1 = listArray (0, 255) $ map f1 [0..255] :: UArray Int Int
    t2 = listArray (0, 255) $ map f2 [0..255] :: UArray Int Int
    f1 0 = 0
    f1 x = let l = x .&. (-x) in 3 ^ (log2 l) + f1 (x .^. l)
    f2 0 = 0
    f2 x = let l = x .&. (-x) in 2 * 3 ^ (log2 l) + f2 (x .^. l)

unpack :: Int -> (Int, Int)
unpack x = loop x 0 (0, 0) where
    loop :: Int -> Int -> (Int, Int) -> (Int, Int)
    loop _ 8 r      = r
    loop y i (a, b) | mod y 3 == 0 = loop (div y 3) (i + 1) (a, b)
                    | mod y 3 == 1 = loop (div y 3) (i + 1) (a + 2 ^ i, b)
                    | mod y 3 == 2 = loop (div y 3) (i + 1) (a, b + 2 ^ i)

edgeTable :: UArray Int Int
edgeTable = listArray (0, 6560) $ map (f . unpack) [0..6560] where
    f (p, o) = corner p + c p o + wing p + mount p o + stable p o
    corner p   = ((p .&. 1) + (p >>> 7)) * 5
    c      p o = (  (if p .&.  2 ==  2 && (p .|. o) .&.   1 == 0 && p .&. 252 /= 252 then 1 else 0)
                  + (if p .&. 64 == 64 && (p .|. o) .&. 128 == 0 && p .&.  63 /=  63 then 1 else 0)) * (-4)
    wing   p   = ((if p .&. 127 == 62 then 1 else 0) + (if p .&. 254 == 124 then 1 else 0)) * (-4)
    mount  p o = if p == 126 && o == 0 then 8 else 0
    stable p o = (if p .|. o == 255 then popCount (p .&. 126) else loop p o 1) * 4
    loop _ _ 7 = 0
    loop p o i =
        let k = bit i; l = k - 1; m = 255 - k - l in
        loop p o (i + 1) + (if p .&. k == k && (p .&. l == l || p .&. m == m) then 1 else 0)

