# ordinal
ordinal is an interpreter for a language that is based on [Forth](https://www.forth.com/forth/).
It's pretty small, having only 30 or so words from the Forth language, but it still pretty neat.

```
>>> 100 100 + .                    
200 
>>> : inc 1 + ; 66 inc inc inc .
69 
>>> : hello ." Hello, world!" ; hello
Hello, world!
>>> : is-odd 2 mod 1 = if ." Odd!" then ; 13 is-odd
Odd!
>>> 12 is-odd
>>> : is-even 2 mod 0 = if ." Even!" else ." Odd!" then ;
>>> 12 is-even 5 is-even
Even!Odd!
>>> : tens 0 do i 10 * . loop ;
>>> 10 tens
0 10 20 30 40 50 60 70 80 90 
>>> 4 tens
0 10 20 30 
>>> : is-even-range 0 do i . i 2 mod 0 = if ." Even! " else ." Odd! " then loop ;
>>> 5 is-even-range
0 Even! 1 Odd! 2 Even! 3 Odd! 4 Even! 
>>> 12 is-even-range
0 Even! 1 Odd! 2 Even! 3 Odd! 4 Even! 5 Odd! 6 Even! 7 Odd! 8 Even! 9 Odd! 10 Even! 11 Odd! 
>>> : print-keycode begin key dup . 32 = until ; ( Print the keycode of keys pressed until the spacebar is pressed ( Unix only ) )
>>> print-keycode
72 101 108 108 111 44 119 111 114 108 100 33 32 
>>> : print-upto-10 0 do i . i 10 = if break then loop ; ( break is not a word found in the Forth language iirc ) 
>>> 5 print-upto-10
0 1 2 3 4 
>>> 17 print-upto-10
0 1 2 3 4 5 6 7 8 9 10 
```
As described above, the `key` word uses the `unix` library under the hood, so this implementation
only supports *nix operating systems.