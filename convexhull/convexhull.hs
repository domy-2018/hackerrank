 
import Text.Printf
import Data.List
import Control.Monad.State
import Control.Monad
import Data.Maybe

type Point = (Int, Int)
type Stack = [Point]


-- ******************** --
-- QUICK HULL algorithm --
-- ******************** --

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
findSideStack :: (Point, Point) -> Stack -> State (Stack, Stack) ()
findSideStack (_, _)   []     = return ()
findSideStack (p1, p2) (x:xs) = do
    (left, right) <- get
    when (direction p1 p2 x < 0) (put (x:left, right))
    when (direction p1 p2 x > 0) (put (left, x:right))
    findSideStack (p1, p2) xs

getSideStack :: (Point, Point) -> Stack -> (Stack, Stack)
getSideStack (p1, p2) xs = execState (findSideStack (p1, p2) xs) ([], [])

-- recursively find all the points of the convex hull
-- given a stack, getMinxMaxX points, get the left and right Stack using getSideStack.
-- Store the min max X points into the ConvexHull points results stack.
-- for left side, getMaxDistLinePoint and add that to the convexHull points.
--    then get the left side of the stack, and getMaxDistLinePoint with minx and the max point and store that to results
--    then get the right side of the stack, and getMaxDistLinePoint with the maxx and the max point and store that to results
      
findQuickHullLeft :: (Point, Point) -> Stack -> State Stack ()
findQuickHullLeft (minxp, maxxp) xs = do
    res <- get
    let 
        thirdp = getMaxDistLinePoint (minxp, maxxp) xs
    when (isJust thirdp) $ do
        let
            (left1, _) = getSideStack (minxp, fromJust thirdp) xs
            (left2, _) = getSideStack (fromJust thirdp, maxxp) xs
        put (fromJust thirdp : res)
        findQuickHullLeft (minxp, fromJust thirdp) left1
        findQuickHullLeft (fromJust thirdp, maxxp) left2

findQuickHullRight :: (Point, Point) -> Stack -> State Stack ()
findQuickHullRight (minxp, maxxp) xs = do
    res <- get
    let
        thirdp = getMaxDistLinePoint (minxp, maxxp) xs
    when (isJust thirdp) $ do
        let
            (_, right1) = getSideStack (minxp, fromJust thirdp) xs
            (_, right2) = getSideStack (fromJust thirdp, maxxp) xs
        put (fromJust thirdp : res)
        findQuickHullRight (minxp, fromJust thirdp) right1
        findQuickHullRight (fromJust thirdp, maxxp) right2


getQuickHull :: Stack -> Stack
getQuickHull xs = 
    execState (findQuickHullLeft (minx, maxx) lefts) [] ++  [minx, maxx] ++ execState (findQuickHullRight (minx, maxx) rights) []
  where
    (minx, maxx) = getMinMaxX xs

    (lefts, rights) = getSideStack (minx, maxx) xs

-- calculate length between two points
calcLength :: Point -> Point -> Double
calcLength (x1, y1) (x2, y2) = sqrt $ (fromIntegral x2 - fromIntegral x1) ^ 2 + (fromIntegral y2 - fromIntegral y1) ^ 2 

-- calculate perimeter given a list of points
calcPerimeter :: [Point] -> Double
calcPerimeter (p1:p2:ps) = calcLength p1 p2 + calcPerimeter (p2:ps)
calcPerimeter _          = 0

-- I can't use a normal sort, because if there are equal X coordinate, I need to ensure the larger Y is after
{- problem wasn't on the normal sort
sortByX :: [Point] -> [Point]
sortByX []     = []
sortByX (p:ps) = smallerSorted ++ [p] ++ largerSorted
  where
    smallerSorted = sortByX [x | x <- ps, if fst x == fst p then snd x < snd p else fst x < fst p]
    largerSorted  = sortByX [y | y <- ps, if fst y == fst p then snd y > snd p else fst y > fst p]
-}

-- need to sort according to the 4 quadrants, in the clockwise direction of y axis
-- this list needs to be sorted by x already
{-
sortByY :: [Point] -> [Point]
sortByY []     = []
sortByY (p:ps) = p : filter (\a -> fst a <  dx && snd a >= snd p) ps ++
                     filter (\a -> fst a >= midx && snd a >= snd p) ps ++
                     filter (\a -> fst a >= midx && snd a <  snd p) ps ++
                     filter (\a -> fst a <  midx && snd a <  snd p) ps
  where
    maxYpoint = (fst (last ps) - fst p) `div` 2
-}   


-- solve is the function required in Hacker rank to solve the perimeter
-- firstly get the convex hull points using getQuickHull
-- then sort it
-- then calculate perimeter
solve :: [(Int, Int)] -> Double
solve xs = calcPerimeter ((y:ys) ++ [y])
  where
    (y:ys) = sortClockwise $ getQuickHull xs


-- find the approximate centre of all the points
findCentre :: [Point] -> Point
findCentre xs = (sum (map fst xs) `div` length xs, sum (map snd xs) `div` length xs)


-- this is to get the pzero point to the front of the list
findPZero :: [Point] -> [Point]
findPZero []     = []
findPZero ps = foldl' comparepzero [] ps
  where
    comparepzero :: [Point] -> Point -> [Point]
    comparepzero [] p1        = [p1]
    comparepzero  (p2@(x2, y2):ys) p1@(x1, y1)
        | y1 < y2             = p1:p2:ys
        | y1 == y2 && x1 < x2 = p1:p2:ys
        | p1 == p2            = p2:ys
        | otherwise           = p2:p1:ys

-- quicksort will sort based on angle to (0,0) and remove smaller collinear points
-- can probably improve performance here by iterating the list once rather than 3 times
-- remove the first item before doing quicksort.

quicksort :: [Point] -> [Point]
quicksort []     = []
quicksort (p:ps) =
    let smallerSorted = quicksort [a | a <- ps, crossProduct a p > 0]
        biggerSorted  = quicksort [a | a <- ps, crossProduct a p < 0]
        equalSorted   = [a | a <- ps, crossProduct a p == 0]
    in smallerSorted ++ [maximum (p:equalSorted)] ++ biggerSorted


-- sorting clockwise
-- firstly find the approximate centre
-- then find the point zero, which is the point lowest by y axis
-- then quicksort it by angle to the point (0,0) at increasing angles
-- then combine p zero with the list where the direction is postive
-- use normal sort where direction is negative and above the y axis of the center point
-- and finally the reverse of direction is negative, and y axis is below centre
sortClockwise :: [Point] -> [Point]
sortClockwise xs = pzero : [a | a <- sortedByZeroAngle, direction pzero centre a >= 0] ++ 
                   reverse (sort [c | c <- sortedByZeroAngle, direction pzero centre c < 0 && snd c >= snd centre]) ++ 
                   reverse [b | b <- sortedByZeroAngle, direction pzero centre b < 0 && snd b < snd centre]

  where
    centre = findCentre xs
 
    sortedPZero = findPZero xs

    pzero = head sortedPZero

    sortedByZeroAngle = quicksort (tail sortedPZero)



{-
-- ********************************************************* --
-- GRAHAM SCAN implementation below, couldn't get it to work --
-- ********************************************************* --

push :: Point -> Stack -> Stack
push p ps = p:ps

pop :: Stack -> Stack
pop = tail

--direction :: Point -> Point -> Point -> Int
--direction (x1, y1) (x2, y2) (x3, y3) = crossProduct (x3 - x1, y3 - y1) (x2 - x1, y2 - y1)

crossProduct :: Point -> Point -> Int
crossProduct (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

-- quicksort will sort based on angle to (0,0) and remove smaller collinear points
-- can probably improve performance here by iterating the list once rather than 3 times
-- remove the first item before doing quicksort.

quicksort :: [Point] -> [Point]
quicksort []     = []
quicksort (p:ps) =
    let smallerSorted = quicksort [a | a <- ps, crossProduct a p > 0]
        biggerSorted  = quicksort [a | a <- ps, crossProduct a p < 0]
        equalSorted   = [a | a <- ps, crossProduct a p == 0]
    in smallerSorted ++ [maximum (p:equalSorted)] ++ biggerSorted

-- this is to get the pzero point to the front of the list
findPZero :: [Point] -> [Point]
findPZero []     = []
findPZero ps = foldl' comparepzero [] ps
  where
    comparepzero :: [Point] -> Point -> [Point]
    comparepzero [] p1        = [p1]
    comparepzero  (p2@(x2, y2):ys) p1@(x1, y1)
        | y1 < y2             = p1:p2:ys
        | y1 == y2 && x1 < x2 = p1:p2:ys
        | p1 == p2            = p2:ys
        | otherwise           = p2:p1:ys

-- This will return the list of points which form the convex hull
getConvexHull :: [Point] -> [Point]
getConvexHull ps = getConvexHullSorted $ headList : quicksort tailList
  where
    pzeroList = findPZero ps
    headList  = head pzeroList
    tailList  = tail pzeroList


getConvexHullSorted :: [Point] -> [Point]
getConvexHullSorted (p0:p1:p2:ps) = foldDirection [p2,p1,p0] ps p0
getConvexHullSorted _             = []

filterDirection :: Stack -> Point -> Stack
filterDirection stk@(p2:p1:p0:ps) newp
    | direction p1 p2 newp <  0 = push newp stk 
    | direction p1 p2 newp >= 0 = push newp $ pop $ filterDirection (p1:p0:ps) p2
filterDirection stk@[p1,p0] newp
    | direction p0 p1 newp <= 0 = push newp stk
    | otherwise                 = [p1, p0]
filterDirection stk@[p0] newp   = [newp, p0]
filterDirection _ _             = [] 

filterDirectionLast :: Stack -> Point -> Point -> Stack
filterDirectionLast stk@(p2:p1:ps) newp pzero
    | direction p1 p2 newp <  0 = push newp stk
    | direction p1 p2 newp >= 0 = if direction newp p2 pzero < 0
                                  then push p2 $ push newp $ pop $ pop stk
                                  else push newp $ filterDirection ps p1
filterDirectionLast _ _ _       = []

-- foldDirection takes a function Stack -> Point -> Stack where the first Stack is the accumulator, and gets a Point from the list, and returns the accumulator
-- second argument Stack is the accumulator
-- third argument Stack is the list to fold over
-- fourth argument Point is the p0 point.
-- returns a Stack
foldDirection :: Stack -> Stack -> Point -> Stack
foldDirection acc [] p0     = acc
foldDirection acc [p] p0    = filterDirectionLast acc p p0
foldDirection acc (p:ps) p0 = foldDirection (filterDirection acc p) ps p0



-- calculate length between two points
--calcLength :: Point -> Point -> Double
--calcLength (x1, y1) (x2, y2) = sqrt $ (fromIntegral x2 - fromIntegral x1) ^ 2 + (fromIntegral y2 - fromIntegral y1) ^ 2 
{-
solve :: [(Int, Int)] -> Double
solve points = calcPerimeter (convexHullPoints ++ [head convexHullPoints])
  where
     convexHullPoints :: [Point]
     convexHullPoints = getConvexHull points

     calcPerimeter :: [Point] -> Double
     calcPerimeter (p1:p2:ps) = calcLength p1 p2 + calcPerimeter (p2:ps)
     calcPerimeter _          = 0
-}
-}
getPoints :: IO [Point]
getPoints = do
  n <- readLn :: IO Int
  items <- forM [1..n] (const getLine)
  let
    points = map ((\[x,y] -> (x,y)) . map (read :: String -> Int) . words) items
  --let
  --  points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
  return points


main :: IO ()
main = do
  pts <- getPoints
  let ans = solve pts
  printf "%.1f\n" ans

{-
main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans
-}
