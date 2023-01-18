module Algebra where

import Data.List
import Model

-- Exercise 5 
type ProgramAlgebra pro r c a
    = (([r] -> pro),                     -- this is for program.
       (Ident -> [c] -> r),              -- this is for rule.
       (c, c, c, c,                      -- Go, Take, Mark, Nothing'
        Dir -> c,                        -- Turn
        Dir -> [a] -> c,                 -- Case
        Ident -> c),                     -- CmdIdent
        (Pat -> [c] -> a))               -- Alt
   

foldP :: ProgramAlgebra pro r c a -> Program -> pro
foldP ((program), 
        (rule), 
        (go, take, mark, nothing, turn, case', cmdident), 
        (alt)) = fold
    where
        fold   (Program rule')        = program (map foldR rule')
        foldR  (Rule ident commands)  = rule ident (map foldC commands)
        foldC   Go                    = go 
        foldC   Take                  = take
        foldC   Mark                  = mark
        foldC   Nothing'              = nothing
        foldC  (Turn x)               = turn x
        foldC  (Case x alts)          = case' x(map foldA alts)
        foldC  (CmdIdent ident)       = cmdident ident
        foldA  (Alt x commands)       = alt x (map foldC commands)

-- equivalently I could have defined programAlgebra as below with all the directions and pats but because they are
-- constants I have wroked with the above (I have treated Dir and Pat data types as like Ints, Strings ... where we don't need to
-- explicitly mention them. Don't know if it is still correct but saves alot of time in writing all the combinations for the
-- foldP function).
-- Below is the full programAlgebra and the foldP function for the corresponding type.
{-
type ProgramAlgebra pro r c d a pat
    = (([r] -> pro),                     -- this is for program.
       (Ident -> [c] -> r),              -- this is for rule.
       (c, c, c, c,                      -- Go, Take, Mark, Nothing'
        d -> c,                          -- Turn
        d -> [a] -> c,                   -- Case
        Ident -> c),                     -- CmdIdent
        (d, d, d),                       -- Left', Right', Front'
        (pat -> [c] -> a),               -- Alt
        (pat, pat, pat, pat, pat, pat))  -- Empty, Lambda, Debris, Asteroid, Boundary, Underscore       

foldP :: ProgramAlgebra pro r c d a pat -> Program -> pro
foldP ((program), 
        (rule), 
        (go, take, mark, nothing, turn, case', cmdident), 
        (left, right, front),
        (alt),
        (empty, lambda, debris, asteroid, boundary, underscore)) = fold
    where
        fold   (Program rule')        = program (map foldR rule')
        foldR  (Rule ident commands)  = rule ident (map foldC commands)
        foldC   Go                    = go 
        foldC   Take                  = take
        foldC   Mark                  = mark
        foldC   Nothing'              = nothing
        foldC  (Turn Left')           = turn left
        foldC  (Turn Right')          = turn right
        foldC  (Turn Front')          = turn front
        foldC  (Case Left' alts)      = case' left (map foldA alts)
        foldC  (Case Front' alts)     = case' front (map foldA alts)
        foldC  (Case Right' alts)     = case' right (map foldA alts)
        foldC  (CmdIdent ident)       = cmdident ident
        foldA  (Alt Empty commands)   = alt empty (map foldC commands)
        foldA  (Alt Lambda commands)  = alt lambda (map foldC commands)
        foldA  (Alt Debris commands)  = alt debris (map foldC commands)
        foldA  (Alt Asteroid commands)   = alt asteroid (map foldC commands)
        foldA  (Alt Boundary commands)   = alt boundary (map foldC commands)
        foldA  (Alt Underscore commands) = alt underscore (map foldC commands)
-}

-- Exercise 6


checkProgram :: Program -> Bool
checkProgram program = 
    undefinedRules program && existStart             program &&
    noRepition     program && noPatternMatchFailures program

undefinedRules :: Program -> Bool
undefinedRules = foldP undefinedRules'
  where
    undefinedRules' :: ProgramAlgebra Bool  (Ident, [Ident]) [Ident] [Ident]
    undefinedRules' =
        ((\pr -> let (ids, calls) = (unzip pr)
                in checkCallInIdent ids (concat calls)),    -- program
            (\ident cmds -> (ident, concat cmds)),          -- rule
            ([],                                            -- go
            [],                                             -- take
            [],                                             -- mark
            [],                                             -- nothing
            \_ -> [],                                       -- turn
            \_ alts -> concat alts,                         -- case
            \ident -> [ident]),                             -- ident
            (\_ cmds -> concat cmds))                       -- alt

    checkCallInIdent :: [Ident] -> [Ident] -> Bool
    checkCallInIdent ids = all (\ x -> x `elem` ids)

existStart  :: Program -> Bool
existStart  = foldP existStart'
  where
    existStart' :: ProgramAlgebra Bool Bool () ()
    existStart' =
        ((or),  
            (\ident _ -> ident == "start"),
            ((), (), (), (),                                
            \_ -> (),    
            \_ _ -> (),
            \_ -> ()),  
            (\_ _ -> ())) 

noRepition :: Program -> Bool
noRepition = foldP noRepition'
  where
    noRepition' :: ProgramAlgebra Bool Ident () ()
    noRepition' =
        ((\rs -> (length (nub rs)) == length rs),
            (\ident _ -> ident),           
            ((), (), (), (),        
            \_ -> (),     
            \_ _ -> (),             
            \_ -> ()),                             
            (\_ _ -> ()))                          

noPatternMatchFailures :: Program -> Bool
noPatternMatchFailures = foldP noPatternMatchFailures'
  where
    noPatternMatchFailures' :: ProgramAlgebra Bool Bool Bool Pat
    noPatternMatchFailures' =
        ((\rs -> all (==True) rs),
         (\_ cmds -> all (==True) cmds),
         (True, True, True, True,     
         \_ -> True,        
            \_ alts -> checkAllPats alts, 
            \_ -> True),    
            (\pat _ -> pat))  

    checkAllPats :: [Pat] -> Bool
    checkAllPats pats = Underscore' `elem` pats ||
                        (Empty'     `elem` pats && 
                         Lambda'    `elem` pats && 
                         Debris'    `elem` pats &&
                         Asteroid'  `elem` pats && 
                         Boundary'  `elem` pats)