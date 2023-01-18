module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L
import qualified Data.Maybe as May

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Show)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents -- assocs gives -> [(Pos, Contents)]



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace space = show size ++ "\n" ++ concat (map f (L.assocs space))
  where
    pos  = L.keys space  -- Returns ONLY keys of the map in ascending order.
    size = last pos      -- the last element of the keys is the largest and defines the size.

    -- if the column number is at the max we need to print a new line.
    f ((_, cols), cont) = case (lookup cont contentsTable) of
      Just x  -> if (cols == snd size) then [x] ++ "\n" else [x]
      Nothing -> error "the defined content cannot be printed"



-- These three should be defined by you
-- type Ident = String (this has already been defined in the Model.hs file) we could equivalently write:
type Ident = Model.Ident
type Commands = [Model.Cmd] -- or just [Cmd]
data Heading = North | South | West | East 
  deriving (Show, Eq) -- the heading direction of the arrow. This is different from the command direction given that an arrow can move in four directions.

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment string  
  -- checking the program; if incorrect, then an empty environment is resulted.
  | checkProgram program = getEnvironment program
  | otherwise            = L.empty 
  where 
    -- extracting the tokens using the parser.y
    program = parser $ alexScanTokens string
    -- getting the identifier & command of all the rules of the program in key pair values. 
    getEnvironment (Program rules) = L.fromList $ map (\(Rule ident commands) -> (ident, reverse commands)) rules



-- | Exercise 9
step :: Environment -> ArrowState -> Step
-- if there is no command:
step _   (ArrowState space pos head []) = Done space pos head
-- if command is Go: only if the new position to move is empty goes to that place otherwise stays at the current position.
step env (ArrowState space pos head (Go:commands)) 
 | elem (L.lookup (newPosition Front' head pos) s) [Just Empty, Just Lambda, Just Debris] = Ok (ArrowState space (newPosition Front' head pos) head commands) 
 | otherwise = Ok (ArrowState space pos head commands)
-- if command is Take: this replaces the current position with empty.
step env (ArrowState space pos head (Take:commands)) = Ok (ArrowState (L.insert pos Empty space) pos head commands)
-- if command is Mark: just as above but replace with lambda
step env (ArrowState space pos head (Mark:commands)) = Ok (ArrowState (L.insert pos Lambda space) pos head commands)
-- if command is Nothing' then don't do anything :)
step env (ArrowState space pos head (Nothing':commands)) = Ok (ArrowState space pos head commands)
-- if command is Turn we change directions.
step env (ArrowState space pos head (Turn dir:commands)) = Ok (ArrowState space pos (newDir dir head) commands)
-- if command is Case:
-- step env (ArrowState space pos head (Case dir alts:commands)) = caseSolver couldn't get it working
-- finally if command is CmdIdent: adding to the command stack.
step env (ArrowState space pos head (CmdIdent ident :commands)) = addRule (L.lookup ident env) 
  where addRule Nothing = error "the defined rule does'nt exist" 
        addRule (Just commands2) = Ok (ArrowState space pos head (commands2++commands))


-- this function gives a new position based on the current position and the direction of movement. 
newPosition :: Dir -> Heading -> Pos -> Pos 
newPosition dir head pos = move pos (newDir dir heading)
  where
    move (x,y) North = (x,y+1)
    move (x,y) South = (x,y-1)
    move (x,y) East = (x+1,y)
    move (x,y) West = (x-1,y)

-- this function gives the new direction when changing it.
newDir :: Dir -> Heading -> Heading
newDir Left' South = East
newDir Left' West  = South
newDir Left' North = West
newDir Left' East  = North
newDir Right' South = West
newDir Right' West  = North
newDir Right' North = East
newDir Right' East  = South
newDir Front' x = x



