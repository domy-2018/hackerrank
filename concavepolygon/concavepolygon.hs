{-# LANGUAGE TypeApplications #-}

import Control.Monad (forM)
import Control.Monad.State
import Data.List
import Data.Maybe

type Point = (Int, Int)
type Stack = [Point]

-- strategy to check if something is concave or not
-- 1. get the convex hull
-- 2. if the number of points of convex hull is less than the list, then there is a concave point
-- 3. if the number of points are the same, then it is convex.

isConcave :: [Point] -> Bool
isConcave xs = length xs /= length (getQuickHull xs)


main :: IO ()
main = do
    i <- read @Int <$> getLine
    inputs <- map ((\[a,b] -> (a,b)) . map (read @ Int) . words) <$> forM [1..i] (const getLine) 
    --print inputs
    if isConcave inputs then putStrLn "YES" else putStrLn "NO"




-- ******************** --
-- QUICK HULL algorithm --
-- ******************** --

-- note: quick hull algorithm modified below include collinear points, because I'm checking for
-- the number of points to be the same or not above.

-- getMinMaxX will find the points with the min and max x coordinate
findMinMaxX :: Stack -> State (Point, Point) ()
findMinMaxX []            = return ()
findMinMaxX (p@(x, _):xs) = do
    (minp@(minx, _), maxp@(maxx, _)) <- get
    when (x < minx) $ put (p, maxp)
    when (x > maxx) $ put (minp, p)
    findMinMaxX xs

getMinMaxX :: Stack -> (Point, Point)
getMinMaxX []     = error "no points in stack"
getMinMaxX (x:xs) = execState (findMinMaxX xs) (x, x)

-- direction function will give you which side it is on the line of the two points.
direction :: Point -> Point -> Point -> Int
direction (x1, y1) (x2, y2) (x3, y3) = crossProduct (x3 - x1, y3 - y1) (x2 - x1, y2 - y1)

crossProduct :: Point -> Point -> Int
crossProduct (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

-- lineDist will give you the distance between the line formed by the first two points, and the third point
lineDist :: Point -> Point -> Point -> Int
lineDist (x1, y1) (x2, y2) (x3, y3) = abs ((y3 - y1) * (x2 - x1) - (y2 - y1) * (x3 - x1))

-- getMaxDistLinePoint will get the point furthest away from the line
getMaxDistLinePoint :: (Point, Point) -> Stack -> Maybe Point
getMaxDistLinePoint _        []     = Nothing
getMaxDistLinePoint (p1, p2) (x:xs) = foldl' foldLineDist (Just x) xs
  where
    foldLineDist Nothing _                          = error "Nothing in accumulator"
    foldLineDist (Just acc) input
        | lineDist p1 p2 input > lineDist p1 p2 acc = Just input
        | otherwise                                 = Just acc


-- getSideStack will return a pair of Stack which falls on the left side or the right side of the line
-- the State returns the left stack, right stack, and the third one is the collinear stack
findSideStack :: (Point, Point) -> Stack -> State (Stack, Stack, Stack) ()
findSideStack (_, _)   []     = return ()
findSideStack (p1, p2) (x:xs) = do
    (left, right, collinear) <- get
    when (direction p1 p2 x < 0)  (put (x:left, right, collinear))
    when (direction p1 p2 x > 0)  (put (left, x:right, collinear))
    when (direction p1 p2 x == 0 && x /= p1 && x /= p2) (put (left, right, x:collinear))
    findSideStack (p1, p2) xs

getSideStack :: (Point, Point) -> Stack -> (Stack, Stack, Stack)
getSideStack (p1, p2) xs = execState (findSideStack (p1, p2) xs) ([], [], [])

-- recursively find all the points of the convex hull
-- given a stack, getMinxMaxX points, get the left and right Stack using getSideStack.
-- Store the min max X points into the ConvexHull points results stack.
-- for left side, getMaxDistLinePoint and add that to the convexHull points.
--    then get the left side of the stack, and getMaxDistLinePoint with minx and the max point and store that to results
--    then get the right side of the stack, and getMaxDistLinePoint with the maxx and the max point and store that to results

findQuickHullLeft :: (Point, Point) -> Stack -> Stack -> State Stack ()
findQuickHullLeft (minxp, maxxp) xs col = do
    res <- get
    let
        thirdp = getMaxDistLinePoint (minxp, maxxp) xs
    when (isJust thirdp) $ do
        let
            (left1, _, collinear1) = getSideStack (minxp, fromJust thirdp) xs
            (left2, _, collinear2) = getSideStack (fromJust thirdp, maxxp) xs
        put (fromJust thirdp : res)
        findQuickHullLeft (minxp, fromJust thirdp) left1 collinear1
        findQuickHullLeft (fromJust thirdp, maxxp) left2 collinear2
    when (isNothing thirdp) $ do
        put (res ++ col) -- if there is no third point, and there are collinear points, add collinear points to result
        

findQuickHullRight :: (Point, Point) -> Stack -> Stack -> State Stack ()
findQuickHullRight (minxp, maxxp) xs col = do
    res <- get
    let
        thirdp = getMaxDistLinePoint (minxp, maxxp) xs
    when (isJust thirdp) $ do
        let
            (_, right1, collinear1) = getSideStack (minxp, fromJust thirdp) xs
            (_, right2, collinear2) = getSideStack (fromJust thirdp, maxxp) xs
        put (fromJust thirdp : res)
        findQuickHullRight (minxp, fromJust thirdp) right1 collinear1
        findQuickHullRight (fromJust thirdp, maxxp) right2 collinear2
    when (isNothing thirdp) $ do
        put (res ++ col)


getQuickHull :: Stack -> Stack
getQuickHull xs =
    execState (findQuickHullLeft (minx, maxx) lefts collinear) [] ++  [minx, maxx] ++ execState (findQuickHullRight (minx, maxx) rights collinear) []
  where
    (minx, maxx) = getMinMaxX xs

    (lefts, rights, collinear) = getSideStack (minx, maxx) xs
   




