{
module Parser where

import Model
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  '->'                                  { TokenArrow      }
  '.'                                   { TokenDot        }
  ','                                   { TokenComma      }
  go                                    { TokenGo         }
  take                                  { TokenTake       }
  mark                                  { TokenMark       }
  nothing                               { TokenNothing    }
  turn                                  { TokenTurn       }
  case                                  { TokenCase       }
  of                                    { TokenOf         }
  end                                   { TokenEnd        }
  left                                  { TokenLeft       }
  right                                 { TokenRight      }
  front                                 { TokenFront      }
  ';'                                   { TokenSemicolon  }
  Empty                                 { TokenEmpty      }
  Lambda                                { TokenLambda     }
  Debris                                { TokenDebris     }
  Asteroid                              { TokenAsteroid   }
  Boundary                              { TokenBoundary   }
  '_'                                   { TokenUnderscore }
  Ident                                 { TokenIdent $$   }


%%

program : Rules { Program $1 }

Rule : Ident '->' Cmds '.' { Rule $1 $3 }

Rules : {- empty -} { [] }
      | Rule { [$1] }
      | Rules Rule { $2 : $1}

Cmds : {- empty -} { [] }
     | Cmd        { [$1] }
     | Cmds ',' Cmd { $3 : $1 }

morecmds : { - empty - } { [] }
         | ',' Cmds Cmd morecmds { $2 : $3}

Cmd : go { Go }
    | take { Take }
    | nothing { Nothing}
    | turn Dir { Turn $2 }
    | case Dir of Alts end { Case $2 $4}
    | Ident { CmdIdent $1  }


Dir : left  { Left }
    | right { Right }
    | front { Front }

Alts : {- empty -}            { [] }
     | Alt                    { [$1] }
     | Alts ';' Alt           { $3 : $1 }

Alt : pat '->' Cmds { Alt $1 $3 }

pat : empty       { Empty }
    | lambda      { Lambda }
    | debris      { Debris }
    | asteroid    { Asteroid }
    | boundary    { Boundary }
    | underscore  { Underscore }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}


