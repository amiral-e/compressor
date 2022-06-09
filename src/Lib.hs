--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Lib
--

module Lib ( defaultConf, getConf, imageCompressor ) where

import Text.Read ( readMaybe )
import Data.Maybe ( isNothing, fromJust )
import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Text.Printf ( printf )
import Data.List ( intercalate )
import System.Random ( randomRIO )

data Conf = Conf {
    nbColors :: Maybe Int,
    limit :: Maybe Double,
    path :: Maybe String
}

data Point = Point {
    x :: Int,
    y :: Int
}

instance Show Point where
    show (Point x y) = printf "(%d,%d)" x y

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
} deriving (Eq)

instance Show Color where
    show (Color r g b) = printf "(%d,%d,%d)" r g b

data Pixel = Pixel {
    point :: Point,
    color :: Color
}

instance Show Pixel where
    show (Pixel point color) = printf "%s %s" (show point) (show color)

data Cluster = Cluster {
    mean :: Color,
    pixels :: [Pixel]
}

instance Show Cluster where
    show (Cluster mean pixels) =
        printf "--\n%s\n-\n%s" (show mean) (intercalate "\n" (map show pixels))

defaultConf :: Conf
defaultConf = Conf {nbColors = Nothing, limit = Nothing, path = Nothing}

parseConf :: [String] -> Maybe Conf -> Maybe Conf
parseConf _ Nothing = Nothing
parseConf [] (Just conf)
    | isNothing (nbColors conf) = Nothing
    | isNothing (limit conf) = Nothing
    | isNothing (path conf) = Nothing
    | otherwise = Just conf

parseConf ("-n":xs:y) (Just conf) =
    parseConf y (Just conf {nbColors = readMaybe xs::Maybe Int})
parseConf ("-l":xs:y) (Just conf) =
    parseConf y (Just conf {limit = readMaybe xs::Maybe Double})
parseConf ("-f":xs:y) (Just conf) = parseConf y (Just conf {path = Just xs})
parseConf (_:xs:y) _ = Nothing

getConf :: [String] -> Conf -> Maybe Conf
getConf args conf
    | length args /= 6 = Nothing
    | otherwise = parseConf args (Just conf)

parseLines :: [String] -> [Pixel]
parseLines = map parsePixel

parsePixel :: String -> Pixel
parsePixel line = Pixel {
    point = parsePoint (tab!!0),
    color = parseColor (tab!!1)
} where tab = words line

checkPixel :: Pixel -> IO ()
checkPixel pixel
    | r (color pixel) < 0 || r (color pixel) > 255 = exitWith (ExitFailure 84)
    | g (color pixel) < 0 || g (color pixel) > 255 = exitWith (ExitFailure 84)
    | b (color pixel) < 0 || b (color pixel) > 255 = exitWith (ExitFailure 84)
    | otherwise = return ()

parsePoint :: String -> Point
parsePoint str = Point {
    x = parseTuple str 0,
    y = parseTuple str 1
}

parseColor :: String -> Color
parseColor str = Color {
    r = parseTuple str 0,
    g = parseTuple str 1,
    b = parseTuple str 2
}

parseTuple :: String -> Int -> Int
parseTuple str index = read (tab!!index)
    where tab = words (cleanText str ",()")

cleanText :: String -> String -> String
cleanText [] _ = ""
cleanText (x:xs) op
    | x `elem` op = " " ++ cleanText xs op
    | otherwise = [x] ++ cleanText xs op

initClusters :: Int -> [Color] -> [Cluster] -> IO [Cluster]
initClusters _ [] clusters = return clusters
initClusters 0 _ clusters = return clusters
initClusters n colors clusters = do
    index <- randomRIO (0, (length colors) - 1)
    let cluster = Cluster { mean = colors!!index, pixels = [] }
    initClusters (n - 1) (removeNth index colors) (clusters ++ [cluster])

removeNth :: Int -> [Color] -> [Color]
removeNth _ [] = []
removeNth 0 (x:xs) = xs
removeNth n (x:xs) = x : removeNth (n -1) (xs)

getColors :: [Pixel] -> [Color] -> [Color]
getColors [] colors = colors
getColors (pixel:xs) colors
    | (color pixel) `elem` colors = getColors xs colors
    | otherwise = getColors xs (colors ++ [color pixel])

getDistance :: Color -> Color -> Double
getDistance (Color r g b) (Color r2 g2 b2) =
        sqrt(fromIntegral((r - r2)^2 + (g - g2)^2 + (b - b2)^2))

getAverage :: [Pixel] -> Color
getAverage pixels = divColor (getSum pixels) (length pixels)

getSum :: [Pixel] -> Color
getSum [] = Color {r = 0, g = 0, b = 0}
getSum (pixel:xs) = addColor (color pixel) (getSum xs)

addColor :: Color -> Color -> Color
addColor (Color r g b) (Color r2 g2 b2) = Color {
    r = r + r2,
    g = g + g2,
    b = b + b2
}

divColor :: Color -> Int -> Color
divColor (Color r g b) n = Color {
    r = r `div` n,
    g = g `div` n,
    b = b `div` n
}

kMeanAlgo :: [Cluster] -> [Cluster] -> [Pixel] -> Int -> Double -> [Cluster]
kMeanAlgo clusters prevClusters pixels nbColors limit
    | isConverging clusters prevClusters limit = clusters
    | otherwise = kMeanAlgo newClusters clusters pixels nbColors limit
    where
        newClusters = updateColors (movePixels (clearClusters clusters) pixels)

isConverging :: [Cluster] -> [Cluster] -> Double -> Bool
isConverging [] [] _ = True
isConverging _ [] _ = False
isConverging (cluster:xs) (cluster2:ys) limit
    | dist < limit = isConverging xs ys limit
    | otherwise = False
    where dist = getDistance (mean cluster) (mean cluster2)

updateColors :: [Cluster] -> [Cluster]
updateColors [] = []
updateColors (Cluster mean []:xs) = Cluster mean [] : updateColors xs
updateColors (Cluster mean pixels:xs) =
    Cluster (getAverage pixels) pixels : updateColors xs

movePixels :: [Cluster] -> [Pixel] -> [Cluster]
movePixels clusters [] = clusters
movePixels clusters (pixel:xs) =
    movePixels (addPixel clusters pixel
        (findCluster clusters pixel 0 0 442.0)) xs

addPixel :: [Cluster] -> Pixel -> Int -> [Cluster]
addPixel [] _ _ = []
addPixel clusters _ (-1) = clusters
addPixel (Cluster mean pixels:xs) pixel 0 =
    Cluster mean (pixels ++ [pixel]) : addPixel xs pixel (-1)
addPixel (cluster:xs) pixel index = cluster : addPixel xs pixel (index - 1)

findCluster :: [Cluster] -> Pixel -> Int -> Int -> Double -> Int
findCluster [] _ _ pos _ = pos
findCluster (cluster:xs) pixel index pos dist =
    case newDist < dist of
        True -> findCluster xs pixel (index + 1) index newDist
        False -> findCluster xs pixel (index + 1) pos dist
    where newDist = getDistance (mean cluster) (color pixel)

clearClusters :: [Cluster] -> [Cluster]
clearClusters [] = []
clearClusters (cluster:xs) = cluster { pixels = [] } : clearClusters xs

imageCompressor :: Conf -> IO ()
imageCompressor (Conf nbColors limit path ) = do
    content <- readFile (fromJust path)
    let pixels = parseLines (lines $ content)
    mapM_ checkPixel pixels
    let colors = getColors pixels []
    clusters <- initClusters (fromJust nbColors) colors []
    mapM_ print (kMeanAlgo clusters [] pixels
        (fromJust nbColors) (fromJust limit))
