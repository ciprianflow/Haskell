module Curves
(Point,Curve,Line(..),
point,pointX,pointY,curve,
connect,translate,rotate,
reflect,bbox,width,height,
toList,toSVG,toFile,hilbert)
where

import Text.Printf

data Point = Point (Double, Double)
  deriving (Show)

point :: (Double, Double) -> Point
point (x,y) = Point (x, y)

pointX :: Point -> Double
pointX (Point p) = fst p

pointY :: Point -> Double
pointY (Point p) = snd p

instance Eq Point where
  x == y = abs (pointX x - pointX y) < 0.01
        && abs (pointY x - pointY y) < 0.01
  x /= y = x /= y


data Curve = Curve Point [Point]
  deriving (Show)

instance Eq Curve where
  (Curve _ pts1) == (Curve _ pts2) = pts1 == pts2
  x /= y = x /= y

curve :: Point -> [Point] -> Curve
curve p [] = Curve p [p]
curve p points = Curve p $ p : points


connect :: Curve -> Curve -> Curve
connect c1 (Curve _ []) = c1
connect (Curve _ pts1) (Curve p2 pts2) =
   Curve p2 $ pts1 ++ pts2

rotate :: Curve -> Double -> Curve
rotate (Curve p pts) degree =
  Curve p $ mapTuples (rot degree) pts

rot :: Double -> Point -> Point
rot degree p = point (
  pointX p * cos deg + pointY p * sin deg,
  - pointX p * sin deg + pointY p * cos deg )
 where deg = degree * pi / 180

-- map for list of tuples

mapTuples :: (Point->Point) -> [Point] -> [Point]
mapTuples _ [] = []
mapTuples f (x:xs) = f x : mapTuples f xs

translate :: Curve -> Point -> Curve
translate (Curve p pts) tp =
  Curve (trans k p) $ mapTuples (trans k) pts
  where k = point(pointX tp - pointX fp, pointY tp - pointY fp)
        fp = head pts

trans :: Point -> Point -> Point
trans p t =
  point(pointX p + pointX t, pointY p + pointY t)


data Line = Vertical Double | Horizontal Double
  deriving (Show)

reflect :: Curve -> Line -> Curve
reflect (Curve p pts) line =
  Curve p $ mapTuples (ref line) pts

-- d == (+- 2*distance)

distance :: Double -> Double -> Double
distance x y = if x > y then (-ec) else ec
  where ec = abs $ abs x - abs y

ref :: Line -> Point -> Point
ref (Vertical line) p = point (x1 + 2 * distance x1 line, pointY p)
  where x1 = pointX p
ref (Horizontal line) p = point (pointX p, y1 + 2 * distance y1 line)
  where y1 = pointY p

bbox :: Curve -> (Point, Point)
bbox (Curve _ pts) = (p1,p2)
  where p1 = foldl1 getLL pts
        p2 = foldl1 getUR pts

getLL :: Point -> Point -> Point
getLL p1 p2 = p
  where p = point(min (pointX p1) (pointX p2), min (pointY p1) (pointY p2))

getUR :: Point -> Point -> Point
getUR p1 p2 = p
  where p = point(max (pointX p1) (pointX p2), max (pointY p1) (pointY p2))

width :: Curve -> Double
width c = pointX p1 - pointX p2
  where p1 = snd $ bbox c
        p2 = fst $ bbox c

height :: Curve -> Double
height c = pointY p1 - pointY p2
  where p1 = snd $ bbox c
        p2 = fst $ bbox c

toList :: Curve -> [Point]
toList (Curve _ pts) = pts


toSVG :: Curve -> String
toSVG c =
  headStr c ++ bodyStr (takeFirstElem c) (getStartingPoint c) ++ "</g></svg>"

takeFirstElem :: Curve -> Curve
takeFirstElem (Curve p pts) = Curve p $ tail pts

getStartingPoint :: Curve -> Point
getStartingPoint (Curve _ pts) = head pts

headStr :: Curve -> String
headStr c =
  printf "<svg xmlns=\"http://www.w3.org/2000/svg\"\
  \ width=\"%.0fpx\" height=\"%.0fpx\" version=\"1.1\">\
  \<g>" (width c) (height c)

bodyStr :: Curve -> Point -> String
bodyStr (Curve _ []) _ = ""
bodyStr (Curve p (x:xs)) pt =
  printf "<line style=\"stroke-width: 2px; stroke: black; fill:white\"\
  \ x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" /> \n"
  (pointX pt) (pointX x) (pointY pt) (pointY x)
  ++ bodyStr (Curve p xs) (point(pointX x, pointY x))

toFile :: Curve -> FilePath -> IO ()
toFile c fileName =
  writeFile fileName (toSVG c)

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Horizontal 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)
