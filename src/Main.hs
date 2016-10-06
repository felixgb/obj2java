module Main where

import Control.Applicative

import Text.Parsec
import Text.Parsec.Language

import qualified Text.Parsec.Token as Tok

type Vertex = (Double, Double, Double, Double)

type TextureCoord = (Double, Double, Double)

type VertexNormal = (Double, Double, Double)

type Vtn = (Double, Maybe Double, Maybe Double)

type Face = (Maybe Vtn, Maybe Vtn, Maybe Vtn)

data Obj = Obj
    { vertices = [Vertex]
    , textCoords = [TextureCoord]
    , vertexNormal = [VertexNormal]
    , textCoords = [Face]
    }

type ObjParsec a = Parsec String [Obj] a

lexer = Tok.makeTokenParser style
    where
        ops = ["/"]
        names = []
        style = emptyDef 
        { Tok.commentStart = "#"
        , Tok.reservedNames = names
        , Tok.reservedOpNames = ops
        }

reservedOp = Tok.reservedOp lexer

double = Tok.double lexer

whiteSpace = Tok.whiteSpace lexer

vertex :: ObjParser
vertex = do
    reserved "f"
    x <- double
    y <- double
    z <- double
    z <- optionMaybe :w

    return $ 

document p = od
    whiteSpace
    r <- p
    eof
    return r
