
{-# LANGUAGE BangPatterns #-}

module Eval where

import Board
import Data.Word
import Data.Bits
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed


{-
    パラメータが手打ちなので、評価関数の精度はあまり良くないが...
    今のところ、評価には「隅・辺に関する得点」、「着手可能数」、「敵石に隣接する空白マス数」を用いている。
    「隅・辺に関する得点」は、隅が550点、空白な隅に隣接している確定石でないCが-450点、Xが-300点、
    隅でない確定石が150点、ウイングが300点(Cの減点と合わせて-150点)、山が900点(Cの減点と合わせて0点)。
    一応 1石差 = 100点 くらいのフィーリングでいるつもり(自分有利に評価していることが多いが)。
-}

eval :: Word64 -> Word64 -> Int -> Int
eval !p !o !turn =
    edgePoint p o
  + (popCount (getMobility  p o) - popCount (getMobility  o p)) * 4 * turn
  + (popCount (getPMobility p o) - popCount (getPMobility o p)) * (300 - 5 * turn)

edgePoint :: Word64 -> Word64 -> Int
edgePoint !p !o = (countX p - countX o) * (-300) + e1 + e2 + e3 + e4 where
        !t = p .|. o
        !m = 0x0042000000004200 .&. (complement $! t .&. 0x0000000000000001) <<< 9
                                .&. (complement $! t .&. 0x0000000000000080) <<< 7
                                .&. (complement $! t .&. 0x0100000000000000) >>> 7
                                .&. (complement $! t .&. 0x8000000000000000) >>> 9
        !e1 = unsafeAt edgeTable (pack (f1 p) (f1 o))
        !e2 = unsafeAt edgeTable (pack (f2 p) (f2 o))
        !e3 = unsafeAt edgeTable (pack (f3 p) (f3 o))
        !e4 = unsafeAt edgeTable (pack (f4 p) (f4 o))
        countX x = popCount (x .&. m)
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
    f (p, o) = g p o - g o p
    g p o = corner p + c p o + wing p + mount p o + stable p o
    corner p   = ((p .&. 1) + (p >>> 7)) * 250
    c      p o = (  (if p .&.  2 ==  2 && (p .|. o) .&.   1 == 0 && p .&. 252 /= 252 then 1 else 0)
                  + (if p .&. 64 == 64 && (p .|. o) .&. 128 == 0 && p .&.  63 /=  63 then 1 else 0)) * (-450)
    wing   p   = ((if p .&. 127 == 62 then 1 else 0) + (if p .&. 254 == 124 then 1 else 0)) * 300
    mount  p o = (if p == 126 && o == 0 then 1 else 0) * 900
    stable p o = popCount (getStable p o p) * 150

getStable :: Int -> Int -> Int -> Int
getStable p o s = let s' = p .&. s in if s' == 0 || e == 0 then s' else iter 0 s' where
    e = complement (p .|. o) .&. 255
    iter 8 stable = stable
    iter i stable | e .&. (bit i) == 0 = iter (i + 1) stable
    iter i stable =
        let rp = fromIntegral $ getFlipLine (fromIntegral p) (fromIntegral o) i
            sp = getStable (xor rp p .|. (bit i)) (xor rp o) stable
            ro = fromIntegral $ getFlipLine (fromIntegral o) (fromIntegral p) i
            so = getStable (xor ro p) (xor ro o .|. (bit i)) sp in
        iter (i + 1) so

