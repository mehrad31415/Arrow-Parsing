{
module Lexer where

import Model

}

%wrapper "basic"

$digit = 0-9			  -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+                             ;
  \->                                 { \s -> TokenArrow }
  \.                                  { \s -> TokenDot }
  \,                                  { \s -> TokenComma }
  go                                  { \s -> TokenGo }
  take                                { \s -> TokenTake }
  mark                                { \s -> TokenMark }
  nothing                             { \s -> TokenNothing}
  turn                                { \s -> TokenTurn }
  case                                { \s -> TokenCase }
  of                                  { \s -> TokenOf }
  end                                 { \s -> TokenEnd }
  left                                { \s -> TokenLeft }
  right                               { \s -> TokenRight }
  front                               { \s -> TokenFront }
  \;                                  { \s -> TokenSemicolon }
  Empty                               { \s -> TokenEmpty }
  Lambda                              { \s -> TokenLambda }
  Debris                              { \s -> TokenDebris }
  Asteroid                            { \s -> TokenAsteroid }
  Boundary                            { \s -> TokenBoundary }
  _                                   { \s -> TokenUnderscore }
  $alpha [$alpha $digit \+ \-]+       { \s -> TokenIdent s }