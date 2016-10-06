{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Data.List
import System.Environment
import Data.Aeson

main :: IO ()
main = do
    path <- fmap head getArgs
    inp <- readFile path
    let obj = parseObj inp
    print $ encode obj
-- 
-- writeObj :: Obj -> String
-- writeObj obj = "class Head {\n" ++ (writeVertices (vertices obj)) ++ (writeFaceIdxs (faces obj)) ++ "}"

-- writeVertex :: Vertex -> String
-- writeVertex (x, y, z, w) = "[" ++ (intercalate ", " $ map (\x -> (show x ++ "f")) [x, y, z, w]) ++ "]"

-- writeVertices vs = "float vert[] = {\n" ++ inner ++ "};\n"
--     where inner = intercalate ", " (map (\f -> show f ++ "f\n") (concatMap (\(a, b, c, d) -> [a, b, c, d]) vs))
-- 
-- writeFaceIdxs fs = "short faces[] = {\n" ++ inner ++ "};\n"
--     where inner = intercalate ", " (map (\f -> show f ++ "\n") (concatMap (\((a, _, _), (b, _, _), (c, _, _)) -> [a, b, c]) fs))
-- 

instance ToJSON Obj where
    toJSON o = object 
        [ "verts" .= (vertsToList $ vertices o)
        , "faces" .= (faceToList $ faces o)
        ]

faceToList :: [Face] -> [Integer]
faceToList = concatMap (\((a, _, _), (b, _, _), (c, _, _)) -> [a, b, c])

vertsToList :: [Vertex] -> [Double]
vertsToList = concatMap (\(x, y, z, w) -> [x, y, z, w])
