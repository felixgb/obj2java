{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.State

import Data.Maybe

import Text.Parsec hiding (State)
import Text.Parsec.Number
import Text.Parsec.Language

import qualified Text.Parsec.Token as Tok

type Vertex = (Double, Double, Double, Double)

type TextureCoord = (Double, Double, Double)

type VertexNormal = (Double, Double, Double)

type Vtn = (Integer, Maybe Integer, Maybe Integer)

type Face = (Vtn, Vtn, Vtn)

data Obj = Obj
    { vertices :: [Vertex]
    , textCoords :: [TextureCoord]
    , vertexNormals :: [VertexNormal]
    , faces :: [Face]
    } deriving (Show)

type ObjParser a = ParsecT String () (State Obj) a

lexer :: Tok.GenTokenParser String () (State Obj)
lexer = Tok.makeTokenParser style
    where
        ops = ["/"]
        names = []
        style = Tok.LanguageDef
            { Tok.commentStart = ""
            , Tok.commentEnd = ""
            , Tok.commentLine = "#"
            , Tok.nestedComments = False
            , Tok.identStart = alphaNum <|> char '_'
            , Tok.identLetter = alphaNum <|> char '_'
            , Tok.opStart = oneOf "\\"
            , Tok.opLetter = oneOf "\\"
            , Tok.reservedNames = names
            , Tok.reservedOpNames = ops
            , Tok.caseSensitive = True
            }

reservedOp = Tok.reservedOp lexer

ident = Tok.identifier lexer

reserved = Tok.reserved lexer

integer = Tok.integer lexer

double = do
    whiteSpace
    sign <- optionMaybe $ reservedOp "-"
    f <- floating2 True
    whiteSpace
    return $ case sign of
        Just _ -> negate f
        Nothing -> f

whiteSpace = Tok.whiteSpace lexer

addVertex :: Vertex -> ObjParser ()
addVertex v = do
    obj <- get
    let vs = (vertices obj) ++ [v]
    put $ obj { vertices = vs }

addTextCoord :: TextureCoord -> ObjParser ()
addTextCoord t = do
    obj <- get
    let ts = (textCoords obj) ++ [t]
    put $ obj { textCoords = ts }

addVertexNormal :: VertexNormal -> ObjParser ()
addVertexNormal vn = do
    obj <- get
    let vns = (vertexNormals obj) ++ [vn]
    put $ obj { vertexNormals = vns }

addFace :: Face -> ObjParser ()
addFace f = do
    obj <- get
    let fs = (faces obj) ++ [f]
    put $ obj { faces = fs }


vertex :: ObjParser ()
vertex = do
    reserved "v"
    x <- double
    y <- double
    z <- double
    w <- optionMaybe double
    addVertex (x, y, z, fromMaybe 1.0 w)

textCoord :: ObjParser ()
textCoord = do
    reserved "vt"
    u <- double
    v <- double
    w <- optionMaybe double
    addTextCoord (u, v, fromMaybe 1.0 w)

vertexNormal :: ObjParser ()
vertexNormal = do
    reserved "vn"
    x <- double
    y <- double
    z <- double
    addVertexNormal (x, y, z)

vtn :: ObjParser Vtn
vtn = do
    a <- integer
    optionMaybe $ reservedOp "/"
    b <- optionMaybe integer
    optionMaybe $ reservedOp "/"
    c <- optionMaybe integer
    return (a, b, c)

face :: ObjParser ()
face = do
    reserved "f"
    a <- vtn
    b <- vtn
    c <- vtn
    addFace (a, b, c)

mtllib = do
    reserved "mtllib"
    ident
    return ()

o = do
    reserved "o"
    ident
    return ()

g = do
    reserved "g"
    ident
    return ()

usemtl = do
    reserved "usemtl"
    ident
    return ()

s = do
    reserved "s"
    ident
    return ()

line = vertex
    <|> s
    <|> mtllib
    <|> o
    <|> g
    <|> usemtl
    <|> textCoord
    <|> vertexNormal
    <|> face

document p = do
    whiteSpace
    r <- p
    eof
    return r

emptyObj = Obj [] [] [] []

-- parseExpr :: String -> Obj
runParseState inp = runParserT (document $ many line) () "<in>" inp

parseObj inp = execState (runParseState inp) emptyObj
