
{-# LANGUAGE BangPatterns #-}

module Search where

import Board
import Eval
import Command
import Data.Bits
import Data.Word
import Data.List (sort)


-- 探索は fail-soft な NegaScout 法を基本とする
-- NegaScout 法は、単純なアルファベータ法より2割〜5割ほど早かった
-- fail-soft による高速化は2%程度に留まる


inf :: Int
inf = 2000000

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
searchAux !p !o !depth !turn = iter_first (moveOrdering p o (getMobility p o) turn) where
    iter_first ((_, !m) : ms) = do
        let !rev = getFlip p o m
            !e   = - (alphaBeta (xor rev o) (xor rev p .|. m) (depth - 1) (turn + 1) (-inf) inf False)
        putStrLn $ (show $ M (log2 m)) ++ ": " ++ (show e)
        iter ms e (log2 m)
    iter [] !best !i = do
        putStrLn $ "score = " ++ (show best)
        return $ M i
    iter ((_, !m) : ms) !best !i = do
        let !rev = getFlip p o m
            !t   = - (alphaBeta (xor rev o) (xor rev p .|. m) (depth - 1) (turn + 1) (-best - 1) (-best) False)
            !e   = if best < t then
                   - (alphaBeta (xor rev o) (xor rev p .|. m) (depth - 1) (turn + 1) (-inf) (-t) False)
                   else t
        putStrLn $ (show $ M (log2 m)) ++ ": " ++ (show e)
        if e > best then iter ms e (log2 m) else iter ms best i

alphaBeta :: Word64 -> Word64 -> Int -> Int -> Int -> Int -> Bool -> Int
alphaBeta !p !o !limit !turn !alpha !beta !pass =
    if limit == 0 then eval p o turn else
    let !mov = getMobility p o in
    if mov == 0 then
        if pass then evalLast p o else
        - (alphaBeta o p (limit - 1) turn (-beta) (-alpha) True)
    else if limit <= 3 then
        iter_simple mov (-inf)
    else
        iter_first (moveOrdering p o mov turn)
    where
        iter_simple 0 !best = best
        iter_simple !mov' !best =
            let !m   = mov' .&. (-mov')
                !rev = getFlip p o m
                !e   = - (alphaBeta (xor rev o) (xor rev p .|. m) (limit - 1) (turn + 1) (-beta) (- (max alpha best)) False)
                !best' = max best e in
            if best' >= beta then best' else iter_simple (xor m mov') best'
        iter_first ((_, !m) : ms) =
            let !rev = getFlip p o m
                !e   = - (alphaBeta (xor rev o) (xor rev p .|. m) (limit - 1) (turn + 1) (-beta) (-alpha) False) in
            if e >= beta then e else iter ms (max alpha e) e
        iter [] _ !best = best
        iter ((_, !m) : ms) !a !best =
            let !rev = getFlip p o m
                !t   = - (alphaBeta (xor rev o) (xor rev p .|. m) (limit - 1) (turn + 1) (-a - 1) (-a) False)
                !e   = if a < t && t < beta then
                       - (alphaBeta (xor rev o) (xor rev p .|. m) (limit - 1) (turn + 1) (-beta) (-t) False)
                       else t
                !best' = max best e in
            if best' >= beta then best' else iter ms (max a best') best'

moveOrdering :: Word64 -> Word64 -> Word64 -> Int -> [(Int, Word64)]
moveOrdering !p !o !m !t = sort (f m []) where
    f 0  !r = r
    f !x !r =
        let !n = x .&. (-x)
            !rev = getFlip p o n in
        f (xor x n) ((eval (xor rev o) (xor rev p .|. n) (t + 1), n) : r)




-- 完全読み切りを行う
-- move ordering は相手の着手可能数が小さい順とする
-- 評価関数や move ordering が軽いため、1秒あたり600万ノード程度を探索できる
-- 20手読み切りで遅くとも20秒程度か? (学科PC上にて)

searchLast :: Word64 -> Word64 -> IO Mv
searchLast !p !o = iter_first (moveOrderingLast p o (getMobility p o)) where
    iter_first ((_, !m, !p', !o', !mov') : ms) = do
        let !e = - (alphaBetaLast o' p' mov' (popCount p' + popCount o') (-inf) inf)
        putStrLn $ (show $ M (log2 m)) ++ ": " ++ (show e)
        iter ms e (log2 m)
    iter [] !best !i = do
        putStrLn $ "score = " ++ (show best)
        return $ M i
    iter ((_, !m, !p', !o', !mov') : ms) !best !i =
        if best == 1000064 then iter [] best i else do
        let !discs = (popCount p' + popCount o')
            !t = - (alphaBetaLast o' p' mov' discs (-best - 1) (-best))
            !e = if best < t then - (alphaBetaLast o' p' mov' discs (-inf) (-t)) else t
        putStrLn $ (show $ M (log2 m)) ++ ": " ++ (show e)
        if e > best then iter ms e (log2 m) else iter ms best i

alphaBetaLast :: Word64 -> Word64 -> Word64 -> Int -> Int -> Int -> Int
alphaBetaLast !p !o !mov !discs !alpha !beta =
    if mov == 0 then
        let !mov' = getMobility o p in
        if mov' == 0 then evalLast p o else
        - (alphaBetaLast o p mov' discs (-beta) (-alpha))
    else
        if discs == 63 then
            let !rev = getFlip p o mov in
            evalLast (xor rev p .|. mov) (xor rev o)
        else if discs >= 59 then
            iter_simple mov (-inf)
        else
            iter_first (moveOrderingLast p o mov)
    where
        iter_simple 0 !best = best
        iter_simple !mov' !best =
            let !m   = mov' .&. (-mov')
                !rev = getFlip p o m
                !p'  = xor rev p .|. m
                !o'  = xor rev o
                !e   = - (alphaBetaLast o' p' (getMobility o' p') (discs + 1) (-beta) (- (max alpha best)))
                !best' = max best e in
            if best' >= beta then best' else iter_simple (xor m mov') best'
        iter_first ((_, _, !p', !o', !mov') : ms) =
            let !e = - (alphaBetaLast o' p' mov' (discs + 1) (-beta) (-alpha)) in
            if e >= beta then e else iter ms (max alpha e) e
        iter [] _ !best = best
        iter ((_, _, !p', !o', !mov') : ms) !a !best =
            let !t = - (alphaBetaLast o' p' mov' (discs + 1) (-a - 1) (-a))
                !e = if a < t && t < beta then - (alphaBetaLast o' p' mov' (discs + 1) (-beta) (-t)) else t
                !best' = max best e in
            if best' >= beta then best' else iter ms (max a e) best'

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
    if pn > on then  1000064 - 2 * on else
    if pn < on then -1000064 + 2 * pn else 0





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

