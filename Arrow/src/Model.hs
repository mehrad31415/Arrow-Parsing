module Model where

-- Each production rule a constructor. The terminals doe not appear the non-terminals are written.
-- Exercise 1
data Token = TokenArrow
           | TokenDot
           | TokenComma
           | TokenGo
           | TokenTake
           | TokenMark
           | TokenNothing
           | TokenTurn
           | TokenCase
           | TokenOf
           | TokenEnd
           | TokenLeft
           | TokenRight
           | TokenFront
           | TokenSemicolon
           | TokenEmpty
           | TokenLamda
           | TokenDebris
           | TokenAsteroid
           | TokenBoundary
           | TokenUnderscore
           | TokenIdent Ident
           deriving (Show, Eq)
          
-- just to make it easier I have defined Ident as a String also in Lexer.x
type Ident = String

-- However, because Ident is ONE or more occurences we could have used the below defined
-- data types as a better alternative (at least I think).
{-
data Ident = Ident' :| [Ident'] deriving (Show, Eq)

data Ident' = Letter Char
            | Digit Digit
            | Plus
            | Minus
            deriving (Show, Eq)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
            deriving (Show, Eq)

-}
-- Note that the parser parses reverse so the lists obtained need to be reversed again to receive the desired result.
-- Exercise 2
-- same as above all production rules are constructors and non-terminals are written as fields.
data Program = Program [Rule] deriving (Show, Eq)

data Rule = Rule Ident [Cmd]
          deriving (Show, Eq)

data Cmd = Go | Take | Mark | Nothing'
         | Turn Dir
         | Case Dir [Alt]
         | CmdIdent Ident
         deriving (Show, Eq)

data Dir = Left'
         | Right'
         | Front'
           deriving (Show, Eq)

data Alt = Alt Pat [Cmd] deriving (Show, Eq)

data Pat = Empty'
         | Lambda'
         | Debris'
         | Asteroid'
         | Boundary'
         | Underscore'
         deriving (Show, Eq)


