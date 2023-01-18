
# Open questions

## Exercise 4
*Happy* is more efficient at parsing left-recursive rules. The main reason is that they result in a constant stack-space parser. In contrast, right-recursive rules are less efficient because right-recursive rules require stack space proportional to the length of the list (sequence) being parsed. The difference can be especially seen in long sequences where automatically generating output might lead to running out of stack space. However, in left-recursive grammar, parsing does not work (or gets stuck) because the parser combinator gets stuck in a loop. The reason is that in left-recursion it keeps calling itself without consuming anything and thus, gets stuck in an infinity loop.

## Exercise 10
If the recursive calls are in the middle of the command sequence, then the stack will become bigger compared to when the recursive calls are in the end of the command sequence. The reason is that when the recursive call is in the middle, commands have not been handled yet so we require more stack space. But when the call is in the end we need less stack space because the commands have been handled are thus removed from the sequence. 
