
{-# LANGUAGE BangPatterns #-}

module Search where

import Board
import Eval
import Command
import Data.Bits
import Data.Word
import Data.List (sort)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed


-- 探索は fail-soft な NegaScout 法を基本とする
-- NegaScout 法は、単純なアルファベータ法より2割〜5割ほど早かった
-- fail-soft による高速化は2%程度に留まる


inf :: Int
inf = 1000064

depthNormal :: Int
depthFast   :: Int
depthExact  :: Int
depthNormal = 12
depthFast   =  8
depthExact  = 20


search :: Word64 -> Word64 -> Int -> IO Mv
search p o time =
    let turn  = popCount p + popCount o - 4 in
    if (turn >= 60 - depthExact     && time >= 30000) ||
       (turn >= 60 - depthExact + 2 && time >= 10000) ||
        turn >= 60 - depthExact + 4 then
        searchLast p o
    else if turn == 0 then
        return $ M 37
    else if time >= 10000 then
        searchAux p o depthNormal turn
    else
        searchAux p o depthFast turn


searchAux :: Word64 -> Word64 -> Int -> Int -> IO Mv
searchAux !p !o !depth !turn = iterFirst (moveOrdering p o (getMobility p o) (oDepth depth) turn) where
    iterFirst ((_, !m, !p', !o') : ms) = do
        let !e   = - (alphaBeta o' p' (depth - 1) (turn + 1) (-inf) inf False)
        iter ms e (log2 m)
    iter [] !best !i = do
        putStrLn $ "score = " ++ (show best)
        return $ M i
    iter ((_, !m, !p', !o') : ms) !best !i = do
        let !t   = - (alphaBeta o' p' (depth - 1) (turn + 1) (-best - 1) (-best) False)
            !e   = if best < t then
                   - (alphaBeta o' p' (depth - 1) (turn + 1) (-inf) (-t) False)
                   else t
        if e > best then iter ms e (log2 m) else iter ms best i

alphaBeta :: Word64 -> Word64 -> Int -> Int -> Int -> Int -> Bool -> Int
alphaBeta !p !o !limit !turn !alpha !beta !pass =
    if limit == 0 then eval p o turn else
    let !mov = getMobility p o in
    if mov == 0 then
        if pass then evalLast p o else
        - (alphaBeta o p (limit - 1) turn (-beta) (-alpha) True)
    else if limit <= 3 then
        iterSimple mov (-inf)
    else
        iterFirst (moveOrdering p o mov (oDepth limit) turn)
    where
        iterSimple 0 !best = best
        iterSimple !mov' !best =
            let !m   = mov' .&. (-mov')
                !rev = getFlip p o m
                !e   = - (alphaBeta (xor rev o) (xor rev p .|. m) (limit - 1) (turn + 1) (-beta) (- (max alpha best)) False)
                !best' = max best e in
            if best' >= beta then best' else iterSimple (xor m mov') best'
        iterFirst ((_, _, !p', !o') : ms) =
            let !e = - (alphaBeta o' p' (limit - 1) (turn + 1) (-beta) (-alpha) False) in
            if e >= beta then e else iter ms (max alpha e) e
        iter [] _ !best = best
        iter ((_, _, !p', !o') : ms) !a !best =
            let !t = - (alphaBeta o' p' (limit - 1) (turn + 1) (-a - 1) (-a) False)
                !e = if a < t && t < beta then
                     - (alphaBeta o' p' (limit - 1) (turn + 1) (-beta) (-t) False)
                     else t
                !best' = max best e in
            if best' >= beta then best' else iter ms (max a best') best'

moveOrdering :: Word64 -> Word64 -> Word64 -> Int -> Int -> [(Int, Word64, Word64, Word64)]
moveOrdering !p !o !m !depth !t = sort (f m []) where
    f 0  !r = r
    f !x !r =
        let !n = x .&. (-x)
            !rev = getFlip p o n
            !p' = xor rev p .|. n
            !o' = xor rev o in
        f (xor x n) ((alphaBeta o' p' depth (t + 1) (-inf) inf False, n, p', o') : r)

oDepth :: Int -> Int
oDepth d = unsafeAt a d where
    a = listArray (0, 12) [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 3, 3, 7] :: UArray Int Int




-- 完全読み切りを行う
-- move ordering は相手の着手可能数が小さい順とする
-- 評価関数や move ordering が軽いため、1秒あたり600万ノード程度を探索できる
-- 20手読み切りで遅くとも20秒程度か? (学科PC上にて)

searchLast :: Word64 -> Word64 -> IO Mv
searchLast !p !o =
    if discs >= 51 then
        iterFirst (moveOrderingLast p o (getMobility p o))
    else
        iterFirst2 (moveOrdering p o (getMobility p o) (oDepthLast discs) (discs - 4))
    where
        !discs = popCount p + popCount o
        iterFirst ((_, !m, !p', !o', !mov') : ms) = do
            let !e = - (alphaBetaLast o' p' mov' (discs + 1) (-inf) inf)
            iter ms e (log2 m)
        iter [] !best !i = do
            putStrLn $ "score = " ++ (show best)
            return $ M i
        iter ((_, !m, !p', !o', !mov') : ms) !best !i =
            if best == inf then iter [] best i else do
            let !t = - (alphaBetaLast o' p' mov' (discs + 1) (-best - 1) (-best))
                !e = if best < t then - (alphaBetaLast o' p' mov' (discs + 1) (-inf) (-t)) else t
            if e > best then iter ms e (log2 m) else iter ms best i
        iterFirst2 ((_, !m, !p', !o') : ms) = do
            let !e = - (alphaBetaLast o' p' (getMobility o' p') (discs + 1) (-inf) inf)
            iter2 ms e (log2 m)
        iter2 [] !best !i = do
            putStrLn $ "score = " ++ (show best)
            return $ M i
        iter2 ((_, !m, !p', !o') : ms) !best !i =
            if best == inf then iter [] best i else do
            let !mov' = getMobility o' p'
                !t = - (alphaBetaLast o' p' mov' (discs + 1) (-best - 1) (-best))
                !e = if best < t then - (alphaBetaLast o' p' mov' (discs + 1) (-inf) (-t)) else t
            if e > best then iter2 ms e (log2 m) else iter2 ms best i


alphaBetaLast :: Word64 -> Word64 -> Word64 -> Int -> Int -> Int -> Int
alphaBetaLast !p !o !mov !discs !alpha !beta =
    if mov == 0 then
        let !mov' = getMobility o p in
        if mov' == 0 then evalLast p o else
        - (alphaBetaLast o p mov' discs (-beta) (-alpha))
    else
        if discs == 62 then
            finalCalc
        else if discs >= 59 then
            iterSimple mov (-inf)
        else if discs >= 51 then
            iterFirst (moveOrderingLast p o mov)
        else
            iterFirst2 (moveOrdering p o mov (oDepthLast discs) (discs - 4))
    where
        finalCalc =
            let !m1 = mov .&. (-mov)
                !r1 = getFlip p o m1
                !p1 = xor r1 p .|. m1
                !o1 = xor r1 o
                !n1 = complement (p1 .|. o1)
                !s1 = getFlip o1 p1 n1
                !e1 = if s1 > 0 then evalLast (xor s1 p1) (xor s1 o1 .|. n1) else
                      let !s1' = getFlip p1 o1 n1 in
                      if s1' > 0 then evalLast (xor s1' p1 .|. n1) (xor s1' o1) else evalLast p1 o1
                !m2 = xor m1 mov in
            if e1 >= beta || m2 == 0 then e1 else
            let !r2 = getFlip p o m2
                !p2 = xor r2 p .|. m2
                !o2 = xor r2 o
                !n2 = complement (p2 .|. o2)
                !s2 = getFlip o2 p2 n2
                !e2 = if s2 > 0 then evalLast (xor s2 p2) (xor s2 o2 .|. n2) else
                      let !s2' = getFlip p2 o2 n2 in
                      if s2' > 0 then evalLast (xor s2' p2 .|. n2) (xor s2' o2) else evalLast p2 o2 in
            max e1 e2
        iterSimple 0 !best = best
        iterSimple !mov' !best =
            let !m   = mov' .&. (-mov')
                !rev = getFlip p o m
                !p'  = xor rev p .|. m
                !o'  = xor rev o
                !e   = - (alphaBetaLast o' p' (getMobility o' p') (discs + 1) (-beta) (- (max alpha best)))
                !best' = max best e in
            if best' >= beta then best' else iterSimple (xor m mov') best'
        iterFirst ((_, _, !p', !o', !mov') : ms) =
            let !e = - (alphaBetaLast o' p' mov' (discs + 1) (-beta) (-alpha)) in
            if e >= beta then e else iter ms (max alpha e) e
        iter [] _ !best = best
        iter ((_, _, !p', !o', !mov') : ms) !a !best =
            let !t = - (alphaBetaLast o' p' mov' (discs + 1) (-a - 1) (-a))
                !e = if a < t && t < beta then - (alphaBetaLast o' p' mov' (discs + 1) (-beta) (-t)) else t
                !best' = max best e in
            if best' >= beta then best' else iter ms (max a e) best'
        iterFirst2 ((_, _, !p', !o') : ms) =
            let !e = - (alphaBetaLast o' p' (getMobility o' p') (discs + 1) (-beta) (-alpha)) in
            if e >= beta then e else iter2 ms (max alpha e) e
        iter2 [] _ !best = best
        iter2 ((_, _, !p', !o') : ms) !a !best =
            let !mov' = getMobility o' p'
                !t = - (alphaBetaLast o' p' mov' (discs + 1) (-a - 1) (-a))
                !e = if a < t && t < beta then - (alphaBetaLast o' p' mov' (discs + 1) (-beta) (-t)) else t
                !best' = max best e in
            if best' >= beta then best' else iter2 ms (max a e) best'

oDepthLast :: Int -> Int
oDepthLast d = unsafeAt a (d - 44) where
    a = listArray (0, 6) [7, 5, 3, 3, 1, 1, 1] :: UArray Int Int

moveOrderingLast :: Word64 -> Word64 -> Word64 -> [(Int, Word64, Word64, Word64, Word64)]
moveOrderingLast !p !o !mov = sort (f mov []) where
    f 0  !r = r
    f !x !r =
        let !m    = x .&. (-x)
            !rev  = getFlip p o m
            !p'   = xor rev p .|. m
            !o'   = xor rev o
            !mov' = getMobility o' p' in
        f (xor m x) ((popCount mov', m, p', o', mov') : r)

evalLast :: Word64 -> Word64 -> Int
evalLast !p !o =
    let !pn = popCount p
        !on = popCount o in
    if pn > on then  inf - 2 * on else
    if pn < on then -inf + 2 * pn else 0





play :: Board -> Color -> Int -> IO Mv
play board color time =
    let (p, o) = if color == black then (blk board, wht board)
                                   else (wht board, blk board)
        m = getMobility p o in
    if m == 0 then return Pass else
    search p o time

doMove :: Board -> Mv -> Color -> IO Board
doMove board Pass  _ = return board
doMove board (M i) color =
    let (p, o) = if color == black then (blk board, wht board)
                                   else (wht board, blk board)
        rev = getFlip p o (bit i)
        p'  = xor rev p .|. (bit i)
        o'  = xor rev o in
    return $ if color == black then Board p' o'
                               else Board o' p'

