module Helpers where

import Data.List

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x, "")] -> Just x
                otherwise -> Nothing

readInt :: String -> Maybe Integer
readInt = maybeRead

readFloat :: String -> Maybe Float
readFloat = maybeRead

readBool :: String -> Maybe Bool
readBool = maybeRead

headList :: [[a]] -> [a]
headList []     = []
headList (x:xs) = (head x):(headList xs)

tailList :: [[a]] -> [[a]]
tailList []     = []
tailList (x:xs) = (tail x):(tailList xs)

isEmptyLists :: [[a]] -> Bool
isEmptyLists []     = True
isEmptyLists (x:xs) = isEmptyLists xs && null x

doubleListRotate :: [[a]] -> [[a]]
doubleListRotate xs = case isEmptyLists xs of
                        True  -> []
                        False -> (headList xs) : (doubleListRotate (tailList xs))

fstList :: [(a, b)] -> [a]
fstList [] = []
fstList ((x,y):ls) = x : (fstList ls)

sndList :: [(a, b)] -> [b]
sndList [] = []
sndList ((x,y):ls) = y : (sndList ls)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

mapTuple :: ((a -> b),(c -> d)) -> [(a,c)] -> [(b,d)]
mapTuple _ []              = []
mapTuple (fl,fr) ((l,r):s) = (fl l,fr r) : mapTuple (fl,fr) s

moveEleOut :: Int -> [a] -> (a,[a])
moveEleOut 1 (x:xs) = (x,xs)
moveEleOut i (x:xs) = (fst (moveEleOut (i-1) xs), [x] ++ snd (moveEleOut (i-1) xs))

moveEleIn :: Int -> (a,[a]) -> [a]
moveEleIn _ (x,[])     = [x]
moveEleIn 1 (x,ys)     = x:ys
moveEleIn i (x,(y:ys)) = y:moveEleIn (i-1) (x,ys)

merge2 :: [[a]] -> [[a]] -> [[a]]
merge2 xs []         = xs
merge2 [] xs         = xs
merge2 (x:xs) (y:ys) = [x++y] ++ merge2 xs ys

unfold :: [[a]] -> [a]
unfold []     = []
unfold (x:xs) = x ++ unfold xs

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x

