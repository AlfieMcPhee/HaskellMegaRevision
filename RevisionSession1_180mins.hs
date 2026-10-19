{-# LANGUAGE ExtendedDefaultRules #-}
-- (The pragma above makes ghc pick default types the way ghci does, so tests
--  such as  thin [1,2] == []  compile even before you've written thin.)

{-
================================================================================
  COMP105 HASKELL MEGA STUDY FILE      (Lectures 5 - 10)
================================================================================

  HOW TO USE IT  (Neovim + WSL2)
  ------------------------------
  1. Open a terminal split inside nvim:   :term ghci COMP105_Haskell_Mega.hs
     (or use a tmux pane). Edit here, :w, then type  :r  in ghci to reload.
  2. Every exercise below is a stub:   some_name = undefined
     Replace that WHOLE line with your own definition (keep the same name!).
  3. Auto-check your work:
        in ghci:    :main            -> runs every test
                    :main 7          -> only lecture 7   (5 6 7 8 9 10 extra)
        in a shell: runghc COMP105_Haskell_Mega.hs 7
     [ ok ] = pass, [FAIL] = wrong answer, [ERR ] = crashed or "not implemented
     yet", [TIME] = took more than 3 seconds (probably infinite recursion).
  4. ANSWER KEY is at the very bottom, after the big SPOILER banner. Try first!
     You can also check my key itself:   runghc COMP105_Haskell_Mega.hs key
     (I couldn't compile it where I wrote it, so if a key test ever fails,
     the key is wrong, not you - tell me which line.)
  5. No type signatures needed yet (that's Lecture 11). If you do add one,
     use Int / [Int] / String, or leave it off.

  THE 3 HOUR PLAN
  ---------------
    0:00 - 0:10   skim the cheat sheet below, get ghci running
    0:10 - 0:25   Lecture 5   tuples, lists, pattern matching
    0:25 - 0:40   Lecture 6   ranges + list comprehensions
    0:40 - 1:05   Lecture 7   recursion, base cases, guards       <- IMPORTANT
    1:05 - 1:35   Lecture 8   recursion on lists                  <- IMPORTANT
    1:35 - 2:10   Lecture 9   where, 2 lists, mutual/multiple rec, quicksort
    2:10 - 2:40   Lecture 10  Caesar cipher program
    2:40 - 2:55   Extra practice (mixed recursion)
    2:55 - 3:00   run  :main  and see what is still red
  Rule of thumb: spend 1/3 of the time reading, 2/3 typing. If you're stuck
  for more than 5 minutes on one exercise, peek at the key, then close it and
  re-write the solution from memory.

  NOTE: only Lectures 5-10 were in the uploads (Lecture 10 was uploaded three
  times). Lectures 1-4 (intro, functions, if/let, basic syntax) are only
  covered by the refresher below.

================================================================================
  CHEAT SHEET
================================================================================

  Basics (from lectures 1-4, as used in later slides)
    f x y = x + y                     define a function: no brackets, no commas
    f (x, y) = x + y                  takes ONE tuple argument (different thing!)
    if c then a else b                an expression; else is mandatory
    let a = 1; b = 2 in a + b         local names, let is an expression
    x `mod` 2    x `div` 2            backticks make a function infix
    ==  /=  <  <=  >  >=  &&  ||  not    comparison and logic ( /= is "not equal")
    f (-1)                            negative numbers need brackets as arguments

  Lists                                 Tuples
    [1,2,3]  "abc" == ['a','b','c']       (1, "two", 3.0)   fixed size, mixed types
    x : xs         add ONE element to the front
    xs ++ ys       join two lists
    xs !! 2        index (starts at 0, slow on long lists)
    head tail init last length reverse sum product take drop elem null and or

  Patterns                                Recursion recipe
    []          empty list                  1. base case(s): when do I stop?
    [x]         exactly one element         2. recursive rule(s): make the input
    (x:xs)      head and tail                  SMALLER each time
    (x:y:rest)  two heads                   3. pretend f already works on the
    (_:xs)      ignore the head                smaller input; what do I add?
    Cases are tried TOP TO BOTTOM. Catch-all goes LAST.

  Guards and where
    f x
      | x < 0     = ...
      | x == 0    = ...
      | otherwise = ...                  otherwise = catch-all (it just means True)
      where helper = ...                 where: one per function, shared by ALL guards

  Recursion "shapes" you will meet over and over (list functions)
    reduce    sum' (x:xs) = x + sum' xs               -> a single value
    map       square_list (x:xs) = x*x : square_list xs  -> a list, same length
    filter    keep x : rest   or just   rest          -> a shorter list
    build     down_from x = x : down_from (x-1)       -> list from a number
    two lists add_lists (x:xs) (y:ys) = ... xs ys     -> consume both in step
    pair out  (a, b) = recursive call in a where      -> return two lists at once
    helper    carry extra info (fast_fib_help)        -> avoid repeated work

  Most common mistakes (check these FIRST when something breaks)
    * missing base case              -> hangs or stack overflow
    * catch-all case above a specific case  -> specific one never runs
    * recursive call doesn't get smaller    -> hangs
    * x : xs  needs an element on the left; xs ++ ys needs lists on both sides
    * [x] is a list with one element, x is the element
    * forgetting backticks:  x mod 2  should be  x `mod` 2
    * forgetting brackets:   f x-1 means (f x) - 1 ;  write f (x-1)
    * indentation: guards and where must be indented more than the function name
================================================================================
-}

module Main where

import Control.Exception
import Data.Char
import System.Environment (getArgs)
import System.Timeout (timeout)

-- ============================================================================
--  TEST MACHINERY  (you don't need to read this part)
-- ============================================================================

type Test = IO Bool

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

describeErr :: SomeException -> String
describeErr e =
  let l = firstLine (show e)
  in if l == "Prelude.undefined" then "not implemented yet" else l

safeShow :: Show a => a -> IO String
safeShow x = do
  r <- try (evaluate (let s = show x in length s `seq` s))
  return (either (\e -> "<error while printing: " ++ describeErr e ++ ">") id r)

check :: (Eq a, Show a) => String -> a -> a -> Test
check name actual expected = do
  r <- try (timeout 3000000 (evaluate (actual == expected)))
  case r of
    Right (Just True) -> do
      putStrLn ("  [ ok ] " ++ name)
      return True
    Right (Just False) -> do
      g <- safeShow actual
      putStrLn ("  [FAIL] " ++ name)
      putStrLn ("           got:      " ++ g)
      putStrLn ("           expected: " ++ show expected)
      return False
    Right Nothing -> do
      putStrLn ("  [TIME] " ++ name ++ "  (over 3 seconds - infinite recursion?)")
      return False
    Left e -> do
      putStrLn ("  [ERR ] " ++ name ++ "  -> " ++ describeErr e)
      return False

runOne :: (String, String, [Test]) -> IO (Int, Int)
runOne (_, title, tests) = do
  putStrLn ("\n=== " ++ title ++ " ===")
  rs <- sequence tests
  let p = length (filter id rs)
  putStrLn ("  -> " ++ show p ++ "/" ++ show (length rs) ++ " passed")
  return (p, length rs)

runSuites :: [(String, String, [Test])] -> IO ()
runSuites [] = putStrLn "No such section. Use: 5 6 7 8 9 10 extra   (or: key)"
runSuites ss = do
  rs <- mapM runOne ss
  let p = sum (map fst rs)
      t = sum (map snd rs)
  putStrLn ("\nTOTAL: " ++ show p ++ "/" ++ show t ++ " tests passing")
  putStrLn (if p == t
              then "All green. Nice."
              else "Look for the [FAIL] / [ERR ] / [TIME] lines above.")

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["key"] -> runSuites suites_key
    []      -> runSuites suites
    names   -> runSuites [s | s@(n, _, _) <- suites, n `elem` names]

{-
================================================================================
  LECTURE 5  -  TUPLES, LISTS AND PATTERN MATCHING
================================================================================

  TUPLES  (1, "two", 3.0)
    * fixed size (2 or more), can mix types
    * use them to return several things from one function
    * area_and_perimeter r = (pi*r*r, 2*pi*r)
    * a function can take a tuple:  rect_area (w, h) = w * h
      (but  f a b = ...  is the normal style and what we prefer)

  LISTS  [1,2,3]
    * variable length (even empty []), ALL elements the same type
    * [1, "two"] is a type error. [(1,2),(3,4)] is fine (all pairs of numbers)
    * a String IS a list of Char:  "abc" == ['a','b','c'],  so every list
      function also works on strings
    * Lists are linked lists: cheap at the FRONT, so we process from the front
      head [1,2,3] = 1          tail [1,2,3] = [2,3]
      last [1,2,3] = 3          init [1,2,3] = [1,2]
      1 : [2,3]    = [1,2,3]    [1,2] ++ [3,4] = [1,2,3,4]
      [10,20,30] !! 1 = 20      (index starts at 0)
    * [1,2,3] is secretly 1 : (2 : (3 : []))

  PATTERN MATCHING in function arguments
    triple_head (x:xs) = 3 * x     x gets the head, xs gets the tail
    mult_first_two (x:y:xs) = x*y
    double_second (_:y:_) = 2*y    _ means "I don't care"
    If the argument doesn't match you get a runtime error:
        triple_head []   ->  Non-exhaustive patterns

  USEFUL FUNCTIONS
    length, reverse, sum, product, take 3 xs, drop 3 xs,
    elem 4 xs  (or  4 `elem` xs),  null xs  (is it empty?), and [..], or [..]

  GOTCHAS
    [1] : [2,3] is a TYPE ERROR (left side must be an element)
    [1,2] ++ 3  is a TYPE ERROR (right side must be a list)
    head [] crashes.   xs !! 10 on a short list crashes.
    null xs is better than length xs == 0 (length walks the whole list).
-}

-- Sandbox: play with these in ghci  (e.g.  triple_head [3,2,3] )
area_and_perimeter r = (pi * r * r, 2 * pi * r)
rect_area (width, height) = width * height
double_head list = 2 * head list
triple_head (x:_) = 3 * x
mult_first_two (x:y:_) = x * y
double_second (_:y:_) = 2 * y

{-
  PREDICT THE RESULT FIRST (then check in ghci). Answers in the key.
    P5.1   "Hello" !! 1
    P5.2   ([5,4,3] ++ [2,1]) !! (2 + 1)
    P5.3   [(1,2),(3,4),(5,6)] !! 0
    P5.4   head (tail [1,2,3,4])
    P5.5   last (init [1,2,3,4])
    P5.6   take 2 (drop 1 [5,6,7,8])
    P5.7   'a' : "bc"
    P5.8   null (tail [1])
    P5.9   Which of these is a type error?   (a) [1,2] ++ [3]   (b) 1 : [2]
           (c) [1] : [2,3]   (d) "ab" ++ "cd"
-}

-- EXERCISE 5.1   x_and_double x
--   Return the tuple (x, two times x).        x_and_double 4 == (4,8)
x_and_double = undefined

-- EXERCISE 5.2   middle t
--   t is a tuple of length 3; return the middle element.   middle (1,2,3) == 2
middle = undefined

-- EXERCISE 5.3   join l1 l2
--   Append l2 to the end of l1.      join [1,2] [3,4] == [1,2,3,4]
join = undefined

-- EXERCISE 5.4   happy s
--   The string s with " :-)" on the end.      happy "hi" == "hi :-)"
happy = undefined

-- EXERCISE 5.5   fourth list
--   The fourth element of the list.      fourth "abcde" == 'd'
fourth = undefined

-- EXERCISE 5.6   double_last list
--   Two times the final element.      double_last [1,2,3] == 6
double_last = undefined

-- EXERCISE 5.7   four_and_five list
--   True if the list contains BOTH 4 and 5.      four_and_five [5,1,4] == True
four_and_five = undefined

-- EXERCISE 5.8   double_second_last list
--   Two times the second-to-last element.      double_second_last [1,2,3,4] == 6
double_second_last = undefined

-- EXERCISE 5.9 (*)   thin list
--   Remove both the first and last elements.      thin [1,2,3,4] == [2,3]
thin = undefined

-- EXERCISE 5.10 (*)   split_head_tail list
--   Return the pair (head, tail).      split_head_tail [1,2,3] == (1,[2,3])
split_head_tail = undefined

-- EXERCISE 5.11 (*)   split_at x list
--   Return (first x elements, the rest).
--   split_at 2 [1,2,3,4,5] == ([1,2],[3,4,5])
split_at = undefined

{-
================================================================================
  LECTURE 6  -  RANGES AND LIST COMPREHENSIONS
================================================================================

  RANGES
    [1..20]        [1,2,...,20]            ['a'..'z']  is the alphabet
    [2,4..20]      step by 2               [20,19..15] counting down
    [1..]          INFINITE list - fine because Haskell is lazy:  take 5 [1..]
    repeat 5       5,5,5,5,... forever     cycle "abc"  abcabcabc... forever
    Careful: [1..0] is just [] (the range only counts upward by default).

  LIST COMPREHENSIONS     [ expression | generator, generator, predicate ... ]
    [x*x | x <- [1..10]]                         squares
    [x*x | x <- [1..10], x*x > 40]               only keep some (predicates)
    [x*y | x <- [2,5,10], y <- [8,10,11]]        two generators = nested loops
                                                  (the LAST generator moves fastest)
    [if x < 10 then "Yes" else "No" | x <- xs]   any expression on the left
    [1 | _ <- xs]                                 _ when you ignore the value
    [[x | x <- xs, even x] | xs <- xxs]           nested comprehension
    A function body can simply BE a comprehension:
        evens_less_than y = [x | x <- [0..(y-1)], x `mod` 2 == 0]

  WHEN TO USE WHAT
    comprehension : map + filter on a list, one-liners
    recursion     : anything that needs to "remember" or combine results
                    (next lectures)
  Python equivalent:  [x**2 for x in range(10)]   (the idea came from Haskell)

  GOTCHAS
    * [x | x <- [1..], cond]  is infinite - only use it with take, or know the
      answer is finite.
    * Predicates are tested left to right; any number of them is allowed.
    * All sublists of a list of lists must hold the same type.
-}

-- Sandbox
evens_less_than y = [x | x <- [0 .. (y - 1)], x `mod` 2 == 0]
lt10 xs = [if x < 10 then "Yes" else "No" | x <- xs]
join_words xs ys = [x ++ " " ++ y | x <- xs, y <- ys]
removeLowercase st = [c | c <- st, c `elem` ['A' .. 'Z']]
length_lc xs = sum [1 | _ <- xs]
factors n = [x | x <- [1 .. n], n `mod` x == 0]
primes_upto n = [x | x <- [1 .. n], length (factors x) == 2]
evens_in_lists xxs = [[x | x <- xs, even x] | xs <- xxs]

{-
  PREDICT THE RESULT FIRST. Answers in the key.
    P6.1   take 5 [1,3..]
    P6.2   [10 * x | x <- [1..10]]
    P6.3   take 4 [x | x <- [1..], x `mod` 3 == 0 && x `mod` 5 == 0]
    P6.4   [x*y | x <- [1,2], y <- [10,20]]
    P6.5   [c | c <- "Hello World", c `elem` ['a'..'z']]
    P6.6   [[x | x <- xs, odd x] | xs <- [[1,2,3],[4,5],[7]]]
    P6.7   length [1,3..99]
    P6.8   [x | x <- [1..20], 20 `mod` x == 0]
-}

-- EXERCISE 6.1   upto x
--   All numbers from 1 to x.      upto 5 == [1,2,3,4,5]
upto = undefined

-- EXERCISE 6.2   between x y
--   All numbers from x to y (inclusive).      between 3 7 == [3,4,5,6,7]
between = undefined

-- EXERCISE 6.3   strictly_between x y
--   All numbers between x and y, NOT including x and y.
--   strictly_between 3 7 == [4,5,6]
strictly_between = undefined

-- EXERCISE 6.4   ten_of x
--   A list with 10 copies of x. Hint: use repeat and take.
--   ten_of 'x' == "xxxxxxxxxx"
ten_of = undefined

-- EXERCISE 6.5   tens n
--   [10, 20, ..] up to 10 * n.      tens 4 == [10,20,30,40]
tens = undefined

-- EXERCISE 6.6   cubes n
--   The cubes 1^3, 2^3, ... n^3  (I read the slide as "all cubes up to n cubed",
--   i.e. n of them).      cubes 4 == [1,8,27,64]
cubes = undefined

-- EXERCISE 6.7   removeUppercase st
--   Like removeLowercase but removes the UPPERCASE letters.
--   removeUppercase "The Big Dog" == "he ig og"
removeUppercase = undefined

-- EXERCISE 6.8   no_twos xs
--   Remove every 2 from a list.      no_twos [1,2,3,2,4] == [1,3,4]
no_twos = undefined

-- EXERCISE 6.9 (*)   primes
--   NO arguments: an infinite list of all the primes.   take 5 primes == [2,3,5,7,11]
--   (hint: you have factors and primes_upto above)
primes = undefined

-- EXERCISE 6.10 (*)   total_length xxs
--   xxs is a list of lists; return the sum of the lengths. Use sum.
--   total_length [[1,2,3],[4],[5,6]] == 6
total_length = undefined

{-
================================================================================
  LECTURE 7  -  RECURSION
================================================================================

  A recursive function calls itself. It needs
    1. BASE CASE(S)       the stopping condition (usually the simplest input)
    2. RECURSIVE RULE(S)  each one moves the input CLOSER to a base case

      factorial 1 = 1                        <- base case
      factorial n = n * factorial (n - 1)    <- recursive rule

      factorial 4 -> 4 * factorial 3 -> 4 * 3 * factorial 2
                  -> 4 * 3 * 2 * factorial 1 -> 4 * 3 * 2 * 1 -> 24

  PATTERN MATCHING IN FUNCTIONS: cases are tried top to bottom; the first that
  matches wins. The catch-all (a plain variable) must come LAST.
      sayMe 1 = "One!"
      sayMe x = "Not between 1 and 5"      <- if this came first, 1 would never match

  GUARDS (nice when you test conditions rather than shapes)
      even_sum x
        | x == 0           = 0
        | x `mod` 2 == 0   = x + even_sum (x - 1)
        | otherwise        = even_sum (x - 1)
    Pattern matching: best for things with a shape (tuples, lists, literals).
    Guards: best for yes/no tests.

  HOW TO DESIGN A RECURSIVE FUNCTION
    1. When do I stop?                          -> base cases
    2. How do I make the problem smaller?       -> the recursive call
    3. PRETEND the function already works on the smaller input. What do I do
       with that answer to get the answer for the bigger input?
          factorial: I have factorial (n-1), multiply by n.
          even_sum:  I have even_sum (x-1); if x is even add x, else add nothing.

  Loops vs recursion: Haskell has no while/for. Base case = loop stop
  condition, recursive rule = loop body + "go again with a smaller thing".

  MULTIPLE BASE CASES / MULTIPLE RULES are fine (fibonacci has 2 base cases,
  even_sum has 2 rules) - but make sure EVERY input has some rule that applies.

  EUCLID'S ALGORITHM  gcd' 400 60 == 20
      gcd' a b | a == b = a | a > b = gcd' (a-b) b | otherwise = gcd' a (b-a)

  THE TWO WAYS RECURSION GOES WRONG
    * no base case reached (hangs)   factorial n = n * factorial (n-1)  goes 2,1,0,-1,...
    * no progress                    factorial n = n * factorial n
-}

-- Sandbox
factorial 1 = 1
factorial n = n * factorial (n - 1)

sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

pow16 0 = 1
pow16 x = 16 * pow16 (x - 1)

multiply x 1 = x
multiply x y = x + multiply x (y - 1)

is_even 0 = True
is_even 1 = False
is_even n = is_even (n - 2)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

even_sum 0 = 0
even_sum x = if x `mod` 2 == 0 then x + even_sum (x - 1) else even_sum (x - 1)

even_sum_g x
  | x == 0 = 0
  | x `mod` 2 == 0 = x + even_sum_g (x - 1)
  | otherwise = even_sum_g (x - 1)

gcd' a b
  | a == b = a
  | a > b = gcd' (a - b) b
  | otherwise = gcd' a (b - a)

-- The lecture's "what do these do?" functions. Try small inputs in ghci.
m7a 1 = 1
m7a x = x + m7a (x - 1)

m7b (-1) = -1
m7b x = x * m7b (x + 1)

-- WARNING: m7c never finishes (Ctrl-C to stop it). Work out why BEFORE running.
m7c n = n * m7c (n - 1)
m7c 1 = 1

{-
  QUESTIONS (answers in the key)
    Q7.1   What does m7a compute? (try m7a 4)
    Q7.2   What does m7b compute? (try m7b (-4)). Why must the argument be negative?
    Q7.3   Why does m7c loop forever even though it has a base case?
    Q7.4   Trace gcd' 400 60 by hand, step by step.
    Q7.5   Without running it, what is multiply 3 4 ? What goes wrong with multiply 3 0 ?
-}

-- EXERCISE 7.1   convertMe s       (use PATTERN MATCHING)
--   1 if s is "one", 2 if "two", 3 if "three", 0 for any other string.
--   convertMe "two" == 2
convertMe = undefined

-- EXERCISE 7.2   max_guard a b     (use GUARDS)
--   The larger of a and b.      max_guard 3 9 == 9
max_guard = undefined

-- EXERCISE 7.3   factorial_square x
--   factorial_square x = x^2 * factorial_square (x-1)   if x > 1
--   factorial_square 1 = 1
--   (The slide's superscript was lost in my copy; I read it as x squared.)
--   factorial_square 3 == 36
factorial_square = undefined

-- EXERCISE 7.4   pow x y
--   x to the power y, WITHOUT ^ or **. Hint: look at pow16.
--   pow 2 10 == 1024
pow = undefined

-- EXERCISE 7.5   odd_sum x
--   The sum of all odd numbers <= x.      odd_sum 10 == 25   (1+3+5+7+9)
odd_sum = undefined

-- EXERCISE 7.6   div3 x
--   True if x is divisible by 3. Recursive, NO mod/div. Assume x >= 0.
--   div3 9 == True,  div3 10 == False
div3 = undefined

-- EXERCISE 7.7   fib_sum x
--   The sum of the first x terms of the Fibonacci sequence 0,1,1,2,3,5,8,...
--   You may reuse fibonacci. (First term = fibonacci 0.)
--   fib_sum 5 == 7  (0+1+1+2+3)
fib_sum = undefined

-- EXERCISE 7.8 (*)   prime x
--   True if x is prime (x > 1 and no y strictly between 1 and x divides x).
--   Write a helper  divisions x y  that tests the divisors.
--   prime 7 == True,  prime 9 == False,  prime 1 == False
prime = undefined

{-
================================================================================
  LECTURE 8  -  RECURSION ON LISTS
================================================================================

  Template (memorise it):
      f []     = <base case: the answer for the empty list>
      f (x:xs) = <combine x with  f xs>

  Walk through sum' [1,2,3]:
      sum' (1:[2,3]) = 1 + sum' [2,3] = 1 + 2 + sum' [3] = 1 + 2 + 3 + sum' []
                     = 1 + 2 + 3 + 0 = 6

  SHAPES OF LIST RECURSION (every exercise is one of these, or a mix)
    reduce       sum' [] = 0;  sum' (x:xs) = x + sum' xs
                 length' (_:xs) = 1 + length' xs   (head ignored with _)
    transform    square_list (x:xs) = x*x : square_list xs
    build        down_from x = x : down_from (x-1)       (input is a number)
    filter       m8a: if keep then x : rest else rest
    search       elem' e (x:xs) | e == x = True | otherwise = elem' e xs
    several base cases
                 take' 0 _ = []            (taken enough)
                 take' _ [] = []           (ran out of list)
    drop         same as take but the base case returns the rest of the list
    exactly one element  maximum' [x] = x   (needed as the "start" of max)
    eat TWO at a time    add_adjacent (x:y:xs) = x+y : add_adjacent xs
                         (also needs a case for an odd leftover [x])
    LOOK at the next one without eating it
                         add_next (x:y:xs) = x+y : add_next (y:xs)   <- y:xs put back
    break into chunks    group n list = take n list : group n (drop n list)
    reverse              reverse' (x:xs) = reverse' xs ++ [x]    (slow, O(n^2))

  GOTCHAS
    * empty list base case vs the one-element case [x]: maximum' of [] is an
      error, so the real base case is [x].
    * x*x : square_list xs  parses as (x*x) : (square_list xs).
      Function application is tightest, then arithmetic, then : last.
    * Calling error "message" is fine for impossible inputs.
-}

-- Sandbox
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' [] = 0
length' (_:xs) = 1 + length' xs

down_from 0 = []
down_from x = x : down_from (x - 1)

square_list [] = []
square_list (x:xs) = x * x : square_list xs

take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

drop' 0 list = list
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

maximum' [] = error "Called with empty list"
maximum' [x] = x
maximum' (x:xs) =
  let max_tail = maximum' xs
  in if x > max_tail then x else max_tail

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

add_adjacent [] = []
add_adjacent [_] = error "Odd number of elements"
add_adjacent (x:y:xs) = x + y : add_adjacent xs

add_next [] = error "Not enough elements"
add_next [_] = error "Not enough elements"
add_next [x, y] = [x + y]
add_next (x:y:xs) = x + y : add_next (y : xs)

group _ [] = []
group n list =
  let first = take n list
      rest = drop n list
  in first : group n rest

-- The lecture's "what do these do?" functions
m8a [] = []
m8a (x:xs)
  | x `mod` 2 == 0 = x : m8a xs
  | otherwise = m8a xs

m8b [] = []
m8b (x:xs) = x : x : m8b xs

{-
  QUESTIONS (answers in the key)
    Q8.1   What does m8a do? What does m8b do? Try [1..6] on both.
    Q8.2   Trace  down_from 3  by hand (use the lecture's arrow style).
    Q8.3   Trace  add_next [1,2,3,4]  by hand.
    Q8.4   Why does  maximum' []  crash, and why is [x] the right base case?
    Q8.5   take' 0 _ = []  comes BEFORE  take' _ [] = [].  Does the order matter
           for  take' 0 []?   (What is the result either way?)
-}

-- (The slide numbering skips 7 - I've kept the slide numbers.)

-- EXERCISE 8.1   product' list
--   Multiply all the numbers together.      product' [1,2,3,4] == 24
product' = undefined

-- EXERCISE 8.2   count x list
--   How many times x appears in list.      count 'a' "banana" == 3
count = undefined

-- EXERCISE 8.3   cube_list list
--   Cube every element.      cube_list [1,2,3] == [1,8,27]
cube_list = undefined

-- EXERCISE 8.4   mult_pairs list
--   Multiply adjacent pairs.      mult_pairs [1,2,3,4] == [2,12]
--   (an odd number of elements may raise an error)
mult_pairs = undefined

-- EXERCISE 8.5   list_min list
--   The smallest element.      list_min [5,3,8,1,9] == 1
list_min = undefined

-- EXERCISE 8.6 (*)   palindrome list
--   True if the list reads the same forwards and backwards. Recursive.
--   palindrome [1,2,3,2,1] == True,  palindrome [1,2,2,1] == True
palindrome = undefined

-- EXERCISE 8.8 (*)   separate list
--   A pair: (all the even numbers, all the odd numbers).
--   separate [1..6] == ([2,4,6],[1,3,5])     (hint: where + tuple, like gt_10)
separate = undefined

-- EXERCISE 8.9 (**)   second_min list
--   The second smallest element. You may need a helper function.
--   second_min [5,3,8,1,9] == 3
--   (With duplicates, count each copy: second_min [4,4,7] is 4 in my key.)
second_min = undefined

{-
================================================================================
  LECTURE 9  -  MORE COMPLEX RECURSION
================================================================================

  WHERE   (one per function, at the END, scoped over every guard)
      remove_twos (x:xs)
        | x == 2    = rest
        | otherwise = x : rest
        where rest = remove_twos xs
    * binds as many names as you like; they are NOT variables, just names
    * you can pattern match:   where (f:_) = fname
    * let is an EXPRESSION (can sit inside other expressions); where is SYNTAX
      attached to a function definition, great together with guards.

  RECURSION OVER TWO LISTS
      add_lists _ [] = []
      add_lists [] _ = []
      add_lists (x:xs) (y:ys) = x + y : add_lists xs ys
    base cases fire when EITHER list is empty; the recursive rule eats one
    element from each. (list_equal is how == on lists really works.)

  RETURNING TWO LISTS (a tuple) FROM RECURSION  - the trick:
      gt_10 [] = ([], [])
      gt_10 (x:xs)
        | x > 10    = (x : gt, lt)
        | otherwise = (gt, x : lt)
        where (gt, lt) = gt_10 xs
    The recursive call returns a pair; pattern match it in `where`, then put x
    onto the right half. Base case builds the empty pair.

  ZIP  turns two lists into a list of pairs: zip' [1,2,3] "abc" == [(1,'a'),...]
    Very common in functional programming. e.g. zip list [0..] numbers a list.

  MUTUAL RECURSION: two functions call each other
      even' 0 = True;  even' n = odd' (n-1)
      odd'  0 = False; odd'  n = even' (n-1)
      evens (x:xs) = x : odds xs      (keep this one)
      odds  (_:xs) = evens xs         (skip this one)
    Make sure you always hit a base case and always get smaller.

  MULTIPLE RECURSION: more than one recursive call in a rule  (fib)
      fib n = fib (n-1) + fib (n-2)  is EXPONENTIALLY slow (fib 40 takes ages)
    Fix: a helper that returns a LIST of previous values so each is computed once:
      fast_fib_help n = x + y : (x:y:xs) where (x:y:xs) = fast_fib_help (n-1)
      (only works for n >= 1: there is no base case for 0.)

  QUICKSORT: pick the head as the pivot; split the rest into lower (< pivot) and
  upper (>= pivot); sort both; glue:  qs lower ++ [pivot] ++ qs upper
  Two versions below: one with split (tuple recursion), one with comprehensions.
-}

-- Sandbox
remove_twos [] = []
remove_twos (x:xs)
  | x == 2 = rest
  | otherwise = x : rest
  where rest = remove_twos xs

initials fname lname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = fname
        (l:_) = lname

add_lists _ [] = []
add_lists [] _ = []
add_lists (x:xs) (y:ys) = x + y : add_lists xs ys

list_equal [] [] = True
list_equal _ [] = False
list_equal [] _ = False
list_equal (x:xs) (y:ys)
  | x == y = list_equal xs ys
  | otherwise = False

gt_10 [] = ([], [])
gt_10 (x:xs)
  | x > 10 = (x : gt, lt)
  | otherwise = (gt, x : lt)
  where (gt, lt) = gt_10 xs

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

even' 0 = True
even' n = odd' (n - 1)
odd' 0 = False
odd' n = even' (n - 1)

evens [] = []
evens (x:xs) = x : odds xs
odds [] = []
odds (_:xs) = evens xs

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fast_fib_help 1 = [1, 0]
fast_fib_help n = x + y : (x : y : xs)
  where (x:y:xs) = fast_fib_help (n - 1)

fast_fib n = head (fast_fib_help n)

split _ [] = ([], [])
split pivot (x:xs)
  | x < pivot = (x : lower, upper)
  | otherwise = (lower, x : upper)
  where (lower, upper) = split pivot xs

qs [] = []
qs (x:xs) = qs lower ++ [x] ++ qs upper
  where (lower, upper) = split x xs

qs' [] = []
qs' (x:xs) = qs' lower ++ [x] ++ qs' upper
  where lower = [e | e <- xs, e < x]
        upper = [e | e <- xs, e >= x]

-- The lecture's "what do these do?" functions
m9a [] = ([], [])
m9a [_] = error "Error"
m9a (x:y:xs) = (x : first, y : second)
  where (first, second) = m9a xs

m9b [] [] = True
m9b [] _ = False
m9b _ [] = False
m9b _ [_] = False
m9b (x:xs) (y1:y2:ys)
  | x == y1 && x == y2 = m9b xs ys
  | otherwise = False

{-
  QUESTIONS (answers in the key)
    Q9.1   What does m9a do? Try m9a [1..6]. What happens with 5 elements?
    Q9.2   What does m9b do? Try m9b [1,2] [1,1,2,2] and m9b [1,2] [1,1,2,3].
           How is it related to m8b?
    Q9.3   Trace  gt_10 [8,12,9]  by hand. What does each recursive call return?
    Q9.4   Why is  fib 30  slow but  fast_fib 30  instant?
    Q9.5   Evaluate  qs [3,1,2]  by hand.
    Q9.6   Why does  fast_fib 0  never finish?
-}

-- EXERCISE 9.1   twos_threes list      (use WHERE)
--   Keep only the elements equal to 2 or 3.
--   twos_threes [1,2,3,4,2,5,3] == [2,3,2,3]
twos_threes = undefined

-- EXERCISE 9.2   product_lists list1 list2
--   Position i of the result is list1 !! i * list2 !! i.
--   (stop when the shorter list runs out)
--   product_lists [1,2,3] [4,5,6] == [4,10,18]
product_lists = undefined

-- EXERCISE 9.3   list_gt list1 list2
--   True if list1 !! i > list2 !! i for ALL i (stop at the shorter list).
--   list_gt [3,4,5] [1,2,3] == True,   list_gt [3,1,5] [1,2,3] == False
list_gt = undefined

-- EXERCISE 9.4   caps_and_smalls string
--   A pair: (all the small letters, all the CAPITAL letters). Ignore others.
--   caps_and_smalls "Hello World" == ("elloorld","HW")
caps_and_smalls = undefined

-- EXERCISE 9.5   thirds list      (use MUTUAL recursion)
--   The elements whose indices are divisible by 3 (0, 3, 6, ...).
--   thirds "abcdefg" == "adg"
thirds = undefined

-- EXERCISE 9.6 (*)   mult_by_pos list
--   Multiply each element by its position (starting at 0).
--   Hint: zip and a helper.     mult_by_pos [10,20,30] == [0,20,60]
mult_by_pos = undefined

-- EXERCISE 9.7 (*)   merge list1 list2
--   Both lists are sorted low to high; return ONE sorted list with everything.
--   merge [1,3,5] [2,4,6] == [1,2,3,4,5,6]
merge = undefined

-- EXERCISE 9.8 (*)   merge_sort list
--   If 0 or 1 elements: return it. Otherwise split into two halves (sizes may
--   differ by 1), merge_sort each, then merge.
--   merge_sort [5,2,9,1,5,6] == [1,2,5,5,6,9]
--   (you can use YOUR merge from 9.7)
merge_sort = undefined

{-
================================================================================
  LECTURE 10  -  CRACKING THE CAESAR CIPHER   (a whole program)
================================================================================

  Caesar cipher: shift every letter forward by k, wrapping z -> a.
      "abcde" shifted by 3 -> "defgh"        "vwxyz" -> "yzabc"
      ROT13 = shift by 13

  PLAN: three functions
      caesar_enc string offset     encode
      caesar_dec string offset     decode  (= encode with -offset)
      caesar_crack string          find the offset automatically

  BUILDING BLOCKS
      ord 'c' = 99, chr 98 = 'b'         (Data.Char)
      char2int c = ord c - ord 'a'       'a'->0 ... 'z'->25
      int2char i = chr (i + ord 'a')
      shift: convert, add offset, `mod` 26 (wrap-around), convert back.
             Non-lowercase characters are left alone.
      Haskell's mod always returns a non-negative result for a positive
      divisor:  (-2) `mod` 26 == 24, which is why a NEGATIVE offset decodes.

  CRACKING = FREQUENCY ANALYSIS
      1. try all 26 offsets
      2. for each decoded text, count how often each letter appears
         (freq c list = count / length)
      3. compare with typical English letter frequencies (eng_freqs, 'e' is ~12%)
         using the chi-squared score:   sum over letters of (freq - eng)^2 / eng
         LOWER score = closer to English
      4. pick the decoding with the smallest score

  THE PIPELINE (read bottom-up in the code):
      caesar_crack
        -> chi_squared_list   (decode with 0..25, score each)
             -> caesar_dec -> shift -> char2int / int2char
             -> chi_squared_string -> get_freqs -> freq -> count_c
                                   -> chi_squared  (vs eng_freqs)
        -> get_best           (recursion that keeps the pair with the lowest score)

  NOTES
    * The slide for caesar_dec skips the base case; it must have
      caesar_dec [] _ = []  (it's in the code below).
    * I added type signatures here so Haskell doesn't have to guess number
      types. The slides print 0.33333334 (Float); Double prints 0.3333333333333333.
    * count in the lecture is called count_c here because YOUR exercise 8.2
      is called count.
-}

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char i = chr (i + ord 'a')

shift :: Char -> Int -> Char
shift c offset =
  let converted = char2int c
      is_lower = converted >= 0 && converted < 26
  in if is_lower
       then int2char ((converted + offset) `mod` 26)
       else c

caesar_enc :: String -> Int -> String
caesar_enc [] _ = []
caesar_enc (x:xs) offset = shift x offset : caesar_enc xs offset

caesar_dec :: String -> Int -> String
caesar_dec [] _ = []
caesar_dec (x:xs) offset = shift x (-offset) : caesar_dec xs offset

count_c :: Char -> String -> Int
count_c _ [] = 0
count_c c (x:xs)
  | c == x = 1 + rest
  | otherwise = rest
  where rest = count_c c xs

freq :: Char -> String -> Double
freq c list = fromIntegral (count_c c list) / fromIntegral (length list)

get_freqs :: String -> Int -> [Double]
get_freqs _ 26 = []
get_freqs string c = freq (int2char c) string : get_freqs string (c + 1)

chi_squared :: [Double] -> [Double] -> Double
chi_squared [] [] = 0
chi_squared (x:xs) (y:ys) = (x - y) ** 2 / y + chi_squared xs ys
chi_squared _ _ = error "chi_squared: lists have different lengths"

eng_freqs :: [Double]
eng_freqs =
  [ 0.0855, 0.0160, 0.0316, 0.0387, 0.1210
  , 0.0218, 0.0209, 0.0496, 0.0733, 0.0022
  , 0.0081, 0.0421, 0.0253, 0.0717, 0.0747
  , 0.0207, 0.0010, 0.0633, 0.0673, 0.0894
  , 0.0268, 0.0106, 0.0183, 0.0019, 0.0172
  , 0.0011 ]

chi_squared_string :: String -> Double
chi_squared_string string =
  let string_freqs = get_freqs string 0
  in chi_squared string_freqs eng_freqs

chi_squared_list :: String -> Int -> [(Double, String)]
chi_squared_list _ 26 = []
chi_squared_list string i =
  let decoded = caesar_dec string i
      score = chi_squared_string decoded
  in (score, decoded) : chi_squared_list string (i + 1)

get_best :: [(Double, String)] -> (Double, String)
get_best [(score, string)] = (score, string)
get_best ((score, string) : xs) =
  let (tail_score, tail_string) = get_best xs
  in if score < tail_score
       then (score, string)
       else (tail_score, tail_string)
get_best [] = error "get_best: empty list"

caesar_crack :: String -> String
caesar_crack string =
  let scores = chi_squared_list string 0
      (_, best) = get_best scores
  in best

{-
  TRY IN GHCI
    caesar_enc "hello there" 3
    caesar_dec "khoor wkhuh" 3
    caesar_crack "lbh jvyy arire qrpbqr guvf"
    chi_squared_list "ifmmp" 0         (look at the scores!)
    take 3 (chi_squared_list "ukq sehh jaran xa wxha pk zaykza pdeo iaoowca" 0)

  QUESTIONS (answers in the key)
    Q10.1  What is  shift 'z' 1 ?  shift 'a' (-1) ?  shift '!' 5 ?
    Q10.2  Why must  get_freqs  return exactly 26 numbers?
    Q10.3  Why is the chi-squared score LOWER for a better match?
    Q10.4  What goes wrong if the ciphertext is very short (e.g. "ab")?
    Q10.5  What does  caesar_enc "Hello" 3  give, and why?
-}

-- EXERCISE 10.2   shift2, caesar_enc2, caesar_dec2
--   Make the encoder/decoder ALSO handle CAPITAL letters (keep their case).
--   shift2 c offset: shift2 'Z' 3 == 'C',  shift2 'a' 3 == 'd',  shift2 '!' 3 == '!'
--   caesar_enc2 "Hello, World!" 3 == "Khoor, Zruog!"
shift2 = undefined
caesar_enc2 = undefined
caesar_dec2 = undefined

-- EXERCISE 10.3   caesar_enc3 string off1 off2,   caesar_dec3 string off1 off2
--   Even positions (0, 2, 4, ...) use off1, odd positions (1, 3, ...) use off2.
--   Positions count EVERY character including spaces. Lowercase only is fine
--   (you can use the lecture's shift).
--   caesar_enc3 "hello" 1 2 == "ignmp"       caesar_enc3 "a b" 1 2 == "b c"
caesar_enc3 = undefined
caesar_dec3 = undefined

-- EXERCISE 10.4   crack_offset string
--   Like caesar_crack, but return the OFFSET (0-25) that decodes the string.
--   crack_offset "ifmmp" == 1       crack_offset "lbh jvyy arire qrpbqr guvf" == 13
crack_offset = undefined

-- EXERCISE 10.5 (*)   crack_mixed string
--   Crack strings that mix small and CAPITAL letters (keep the case in the
--   output).   crack_mixed "Lbh jvyy arire qrpbqr guvf" == "You will never decode this"
crack_mixed = undefined

-- EXERCISE 10.6 (*)   crack_two string        (NOT auto-tested)
--   Crack text that was encoded with caesar_enc3 (two offsets). Return the
--   decoded string. Idea: the even-position letters form their own Caesar
--   cipher and so do the odd ones (hint: evens and odds from lecture 9!).
--   Self-check: take a long English paragraph, caesar_enc3 it with two offsets
--   of your choice, and see whether crack_two gives the paragraph back.
--   (Needs plenty of text: frequency analysis is unreliable on short strings.)
crack_two = undefined

{-
================================================================================
  EXTRA PRACTICE  -  mixed recursion (not from the slides)
================================================================================
  These combine the lecture 7-9 skills. They're the sort of thing a test might
  ask. Same rules: replace the stub, run  :main extra
-}

-- EXTRA 1   count_evens list
--   How many even numbers.      count_evens [1,2,3,4,6] == 3
count_evens = undefined

-- EXTRA 2   filter_gt n list
--   Keep the elements greater than n.      filter_gt 3 [1,5,2,7,3,9] == [5,7,9]
filter_gt = undefined

-- EXTRA 3   interleave xs ys
--   Alternate elements; when one list runs out, append the rest of the other.
--   interleave [1,2,3] [10,20] == [1,10,2,20,3]       interleave "abc" "xyz" == "axbycz"
interleave = undefined

-- EXTRA 4   rotate_left n list
--   Move the first n elements to the back.      rotate_left 2 [1,2,3,4,5] == [3,4,5,1,2]
rotate_left = undefined

-- EXTRA 5   dedupe list
--   Collapse runs of equal ADJACENT elements into one.
--   dedupe [1,1,2,2,2,3,1] == [1,2,3,1]       dedupe "aabbbc" == "abc"
dedupe = undefined

-- EXTRA 6   is_sorted list
--   True if the list is in non-decreasing order.
--   is_sorted [1,2,2,5] == True,  is_sorted [3,1,2] == False,  is_sorted [] == True
is_sorted = undefined

-- EXTRA 7 (*)   rle string       (run-length encoding)
--   rle "aaabccdddd" == [('a',3),('b',1),('c',2),('d',4)]
--   Hint: a helper that returns (how many repeats, the rest) - the gt_10 trick.
rle = undefined

-- EXTRA 8 (*)   un_rle pairs     (the inverse of rle)
--   un_rle [('a',3),('b',1),('c',2)] == "aaabcc"      (hint: replicate)
un_rle = undefined

-- EXTRA 9 (*)   collatz_steps n
--   Steps to reach 1: if n is even go to n/2 (use div), otherwise 3n+1.
--   collatz_steps 1 == 0,  collatz_steps 6 == 8,  collatz_steps 7 == 16
collatz_steps = undefined

{-
================================================================================
  TESTS    (stay out of this part)
================================================================================
-}

tests_L5 :: [Test]
tests_L5 =
  [ check "x_and_double 4" (x_and_double 4) (4, 8)
  , check "x_and_double 0" (x_and_double 0) (0, 0)
  , check "middle (1,2,3)" (middle (1, 2, 3)) 2
  , check "middle (\"a\",'b',3)" (middle ("a", 'b', 3)) 'b'
  , check "join [1,2] [3,4]" (join [1, 2] [3, 4]) [1, 2, 3, 4]
  , check "join \"ab\" \"\"" (join "ab" "") "ab"
  , check "happy \"hi\"" (happy "hi") "hi :-)"
  , check "happy \"\"" (happy "") " :-)"
  , check "fourth \"abcde\"" (fourth "abcde") 'd'
  , check "fourth [10,20,30,40]" (fourth [10, 20, 30, 40]) 40
  , check "double_last [1,2,3]" (double_last [1, 2, 3]) 6
  , check "double_last [5]" (double_last [5]) 10
  , check "four_and_five [5,1,4]" (four_and_five [5, 1, 4]) True
  , check "four_and_five [4,6]" (four_and_five [4, 6]) False
  , check "four_and_five []" (four_and_five []) False
  , check "double_second_last [1,2,3,4]" (double_second_last [1, 2, 3, 4]) 6
  , check "double_second_last [7,8]" (double_second_last [7, 8]) 14
  , check "thin [1,2,3,4]" (thin [1, 2, 3, 4]) [2, 3]
  , check "thin \"abc\"" (thin "abc") "b"
  , check "thin [1,2]" (thin [1, 2]) []
  , check "split_head_tail [1,2,3]" (split_head_tail [1, 2, 3]) (1, [2, 3])
  , check "split_head_tail \"x\"" (split_head_tail "x") ('x', "")
  , check "split_at 2 [1..5]" (split_at 2 [1, 2, 3, 4, 5]) ([1, 2], [3, 4, 5])
  , check "split_at 0 [1,2]" (split_at 0 [1, 2]) ([], [1, 2])
  , check "split_at 3 \"abc\"" (split_at 3 "abc") ("abc", "")
  ]

tests_L6 :: [Test]
tests_L6 =
  [ check "upto 5" (upto 5) [1, 2, 3, 4, 5]
  , check "upto 0" (upto 0) []
  , check "between 3 7" (between 3 7) [3, 4, 5, 6, 7]
  , check "between 4 4" (between 4 4) [4]
  , check "strictly_between 3 7" (strictly_between 3 7) [4, 5, 6]
  , check "strictly_between 3 4" (strictly_between 3 4) []
  , check "ten_of 'x'" (ten_of 'x') "xxxxxxxxxx"
  , check "ten_of 7" (ten_of 7) [7, 7, 7, 7, 7, 7, 7, 7, 7, 7]
  , check "tens 4" (tens 4) [10, 20, 30, 40]
  , check "tens 0" (tens 0) []
  , check "cubes 4" (cubes 4) [1, 8, 27, 64]
  , check "cubes 1" (cubes 1) [1]
  , check "removeUppercase \"The Big Dog\"" (removeUppercase "The Big Dog") "he ig og"
  , check "removeUppercase \"ABC\"" (removeUppercase "ABC") ""
  , check "no_twos [1,2,3,2,4]" (no_twos [1, 2, 3, 2, 4]) [1, 3, 4]
  , check "no_twos [2,2]" (no_twos [2, 2]) []
  , check "take 10 primes" (take 10 primes) [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  , check "total_length [[1,2,3],[4],[5,6]]" (total_length [[1, 2, 3], [4], [5, 6]]) 6
  , check "total_length []" (total_length []) 0
  ]

tests_L7 :: [Test]
tests_L7 =
  [ check "convertMe \"one\"" (convertMe "one") 1
  , check "convertMe \"two\"" (convertMe "two") 2
  , check "convertMe \"three\"" (convertMe "three") 3
  , check "convertMe \"banana\"" (convertMe "banana") 0
  , check "max_guard 3 9" (max_guard 3 9) 9
  , check "max_guard 9 3" (max_guard 9 3) 9
  , check "max_guard 4 4" (max_guard 4 4) 4
  , check "factorial_square 1" (factorial_square 1) 1
  , check "factorial_square 2" (factorial_square 2) 4
  , check "factorial_square 3" (factorial_square 3) 36
  , check "factorial_square 4" (factorial_square 4) 576
  , check "pow 2 10" (pow 2 10) 1024
  , check "pow 5 0" (pow 5 0) 1
  , check "pow 3 3" (pow 3 3) 27
  , check "odd_sum 0" (odd_sum 0) 0
  , check "odd_sum 1" (odd_sum 1) 1
  , check "odd_sum 5" (odd_sum 5) 9
  , check "odd_sum 10" (odd_sum 10) 25
  , check "div3 0" (div3 0) True
  , check "div3 9" (div3 9) True
  , check "div3 12" (div3 12) True
  , check "div3 2" (div3 2) False
  , check "div3 10" (div3 10) False
  , check "div3 14" (div3 14) False
  , check "fib_sum 0" (fib_sum 0) 0
  , check "fib_sum 1" (fib_sum 1) 0
  , check "fib_sum 2" (fib_sum 2) 1
  , check "fib_sum 5" (fib_sum 5) 7
  , check "fib_sum 7" (fib_sum 7) 20
  , check "prime 2" (prime 2) True
  , check "prime 7" (prime 7) True
  , check "prime 13" (prime 13) True
  , check "prime 97" (prime 97) True
  , check "prime 1" (prime 1) False
  , check "prime 4" (prime 4) False
  , check "prime 9" (prime 9) False
  , check "prime 100" (prime 100) False
  ]

tests_L8 :: [Test]
tests_L8 =
  [ check "product' [1,2,3,4]" (product' [1, 2, 3, 4]) 24
  , check "product' []" (product' []) 1
  , check "count 'a' \"banana\"" (count 'a' "banana") 3
  , check "count 3 [1,3,3,2,3]" (count 3 [1, 3, 3, 2, 3]) 3
  , check "count 9 []" (count 9 []) 0
  , check "cube_list [1,2,3]" (cube_list [1, 2, 3]) [1, 8, 27]
  , check "cube_list []" (cube_list []) []
  , check "mult_pairs [1,2,3,4]" (mult_pairs [1, 2, 3, 4]) [2, 12]
  , check "mult_pairs [2,3]" (mult_pairs [2, 3]) [6]
  , check "mult_pairs []" (mult_pairs []) []
  , check "list_min [5,3,8,1,9]" (list_min [5, 3, 8, 1, 9]) 1
  , check "list_min [7]" (list_min [7]) 7
  , check "list_min \"hello\"" (list_min "hello") 'e'
  , check "palindrome [1,2,3,2,1]" (palindrome [1, 2, 3, 2, 1]) True
  , check "palindrome [1,2,2,1]" (palindrome [1, 2, 2, 1]) True
  , check "palindrome [1,2,3]" (palindrome [1, 2, 3]) False
  , check "palindrome []" (palindrome []) True
  , check "palindrome \"racecar\"" (palindrome "racecar") True
  , check "palindrome \"ab\"" (palindrome "ab") False
  , check "separate [1..6]" (separate [1, 2, 3, 4, 5, 6]) ([2, 4, 6], [1, 3, 5])
  , check "separate []" (separate []) ([], [])
  , check "separate [3,5]" (separate [3, 5]) ([], [3, 5])
  , check "second_min [5,3,8,1,9]" (second_min [5, 3, 8, 1, 9]) 3
  , check "second_min [2,1]" (second_min [2, 1]) 2
  , check "second_min [10,20,30]" (second_min [10, 20, 30]) 20
  ]

tests_L9 :: [Test]
tests_L9 =
  [ check "twos_threes [1,2,3,4,2,5,3]" (twos_threes [1, 2, 3, 4, 2, 5, 3]) [2, 3, 2, 3]
  , check "twos_threes []" (twos_threes []) []
  , check "twos_threes [4,5]" (twos_threes [4, 5]) []
  , check "product_lists [1,2,3] [4,5,6]" (product_lists [1, 2, 3] [4, 5, 6]) [4, 10, 18]
  , check "product_lists [1,2,3] [2,2]" (product_lists [1, 2, 3] [2, 2]) [2, 4]
  , check "product_lists [] [1]" (product_lists [] [1]) []
  , check "list_gt [3,4,5] [1,2,3]" (list_gt [3, 4, 5] [1, 2, 3]) True
  , check "list_gt [3,1,5] [1,2,3]" (list_gt [3, 1, 5] [1, 2, 3]) False
  , check "list_gt [] []" (list_gt [] []) True
  , check "caps_and_smalls \"Hello World\"" (caps_and_smalls "Hello World") ("elloorld", "HW")
  , check "caps_and_smalls \"abc\"" (caps_and_smalls "abc") ("abc", "")
  , check "caps_and_smalls \"ABC\"" (caps_and_smalls "ABC") ("", "ABC")
  , check "caps_and_smalls \"a1B!\"" (caps_and_smalls "a1B!") ("a", "B")
  , check "thirds [0..9]" (thirds [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) [0, 3, 6, 9]
  , check "thirds \"abcdefg\"" (thirds "abcdefg") "adg"
  , check "thirds []" (thirds []) []
  , check "thirds [1,2]" (thirds [1, 2]) [1]
  , check "mult_by_pos [5,5,5,5]" (mult_by_pos [5, 5, 5, 5]) [0, 5, 10, 15]
  , check "mult_by_pos [10,20,30]" (mult_by_pos [10, 20, 30]) [0, 20, 60]
  , check "mult_by_pos []" (mult_by_pos []) []
  , check "merge [1,3,5] [2,4,6]" (merge [1, 3, 5] [2, 4, 6]) [1, 2, 3, 4, 5, 6]
  , check "merge [] [1,2]" (merge [] [1, 2]) [1, 2]
  , check "merge [1,2] []" (merge [1, 2] []) [1, 2]
  , check "merge [1,1,2] [1,3]" (merge [1, 1, 2] [1, 3]) [1, 1, 1, 2, 3]
  , check "merge_sort [5,2,9,1,5,6]" (merge_sort [5, 2, 9, 1, 5, 6]) [1, 2, 5, 5, 6, 9]
  , check "merge_sort []" (merge_sort []) []
  , check "merge_sort [1]" (merge_sort [1]) [1]
  , check "merge_sort [3,2,1]" (merge_sort [3, 2, 1]) [1, 2, 3]
  , check "merge_sort \"haskell\"" (merge_sort "haskell") "aehklls"
  ]

tests_L10 :: [Test]
tests_L10 =
  [ check "(lecture code) caesar_enc \"hello there\" 3" (caesar_enc "hello there" 3) "khoor wkhuh"
  , check "(lecture code) caesar_dec \"khoor wkhuh\" 3" (caesar_dec "khoor wkhuh" 3) "hello there"
  , check "(lecture code) caesar_enc \"vwxyz\" 3" (caesar_enc "vwxyz" 3) "yzabc"
  , check "(lecture code) caesar_enc \"abcde\" 13" (caesar_enc "abcde" 13) "nopqr"
  , check "(lecture code) caesar_crack" (caesar_crack "lbh jvyy arire qrpbqr guvf") "you will never decode this"
  , check "shift2 'Z' 3" (shift2 'Z' 3) 'C'
  , check "shift2 'A' 3" (shift2 'A' 3) 'D'
  , check "shift2 'a' 3" (shift2 'a' 3) 'd'
  , check "shift2 'x' 3" (shift2 'x' 3) 'a'
  , check "shift2 '!' 3" (shift2 '!' 3) '!'
  , check "caesar_enc2 \"Hello, World!\" 3" (caesar_enc2 "Hello, World!" 3) "Khoor, Zruog!"
  , check "caesar_dec2 \"Khoor, Zruog!\" 3" (caesar_dec2 "Khoor, Zruog!" 3) "Hello, World!"
  , check "caesar_enc3 \"aaaa\" 1 2" (caesar_enc3 "aaaa" 1 2) "bcbc"
  , check "caesar_enc3 \"hello\" 1 2" (caesar_enc3 "hello" 1 2) "ignmp"
  , check "caesar_enc3 \"a b\" 1 2" (caesar_enc3 "a b" 1 2) "b c"
  , check "caesar_dec3 \"ignmp\" 1 2" (caesar_dec3 "ignmp" 1 2) "hello"
  , check "crack_offset \"ifmmp\"" (crack_offset "ifmmp") 1
  , check "crack_offset \"lbh jvyy arire qrpbqr guvf\"" (crack_offset "lbh jvyy arire qrpbqr guvf") 13
  , check "crack_mixed \"Lbh jvyy arire qrpbqr guvf\"" (crack_mixed "Lbh jvyy arire qrpbqr guvf") "You will never decode this"
  ]

tests_extra :: [Test]
tests_extra =
  [ check "count_evens [1,2,3,4,6]" (count_evens [1, 2, 3, 4, 6]) 3
  , check "count_evens []" (count_evens []) 0
  , check "filter_gt 3 [1,5,2,7,3,9]" (filter_gt 3 [1, 5, 2, 7, 3, 9]) [5, 7, 9]
  , check "filter_gt 10 [1,2]" (filter_gt 10 [1, 2]) []
  , check "interleave [1,2,3] [10,20]" (interleave [1, 2, 3] [10, 20]) [1, 10, 2, 20, 3]
  , check "interleave \"abc\" \"xyz\"" (interleave "abc" "xyz") "axbycz"
  , check "interleave [] [1,2]" (interleave [] [1, 2]) [1, 2]
  , check "rotate_left 2 [1..5]" (rotate_left 2 [1, 2, 3, 4, 5]) [3, 4, 5, 1, 2]
  , check "rotate_left 0 [1,2,3]" (rotate_left 0 [1, 2, 3]) [1, 2, 3]
  , check "rotate_left 5 [1..5]" (rotate_left 5 [1, 2, 3, 4, 5]) [1, 2, 3, 4, 5]
  , check "dedupe [1,1,2,2,2,3,1]" (dedupe [1, 1, 2, 2, 2, 3, 1]) [1, 2, 3, 1]
  , check "dedupe \"aabbbc\"" (dedupe "aabbbc") "abc"
  , check "dedupe []" (dedupe []) []
  , check "is_sorted [1,2,2,5]" (is_sorted [1, 2, 2, 5]) True
  , check "is_sorted [3,1,2]" (is_sorted [3, 1, 2]) False
  , check "is_sorted []" (is_sorted []) True
  , check "is_sorted [7]" (is_sorted [7]) True
  , check "rle \"aaabccdddd\"" (rle "aaabccdddd") [('a', 3), ('b', 1), ('c', 2), ('d', 4)]
  , check "rle \"\"" (rle "") []
  , check "rle \"abc\"" (rle "abc") [('a', 1), ('b', 1), ('c', 1)]
  , check "un_rle [('a',3),('b',1),('c',2)]" (un_rle [('a', 3), ('b', 1), ('c', 2)]) "aaabcc"
  , check "un_rle []" (un_rle []) ""
  , check "collatz_steps 1" (collatz_steps 1) 0
  , check "collatz_steps 6" (collatz_steps 6) 8
  , check "collatz_steps 7" (collatz_steps 7) 16
  ]

suites :: [(String, String, [Test])]
suites =
  [ ("5", "Lecture 5: tuples, lists, pattern matching", tests_L5)
  , ("6", "Lecture 6: ranges and list comprehensions", tests_L6)
  , ("7", "Lecture 7: recursion, guards", tests_L7)
  , ("8", "Lecture 8: recursion on lists", tests_L8)
  , ("9", "Lecture 9: where, two lists, mutual recursion, sorting", tests_L9)
  , ("10", "Lecture 10: Caesar cipher", tests_L10)
  , ("extra", "Extra practice", tests_extra)
  ]

tests_L5_key :: [Test]
tests_L5_key =
  [ check "x_and_double 4" (sol_x_and_double 4) (4, 8)
  , check "x_and_double 0" (sol_x_and_double 0) (0, 0)
  , check "middle (1,2,3)" (sol_middle (1, 2, 3)) 2
  , check "middle (\"a\",'b',3)" (sol_middle ("a", 'b', 3)) 'b'
  , check "join [1,2] [3,4]" (sol_join [1, 2] [3, 4]) [1, 2, 3, 4]
  , check "join \"ab\" \"\"" (sol_join "ab" "") "ab"
  , check "happy \"hi\"" (sol_happy "hi") "hi :-)"
  , check "happy \"\"" (sol_happy "") " :-)"
  , check "fourth \"abcde\"" (sol_fourth "abcde") 'd'
  , check "fourth [10,20,30,40]" (sol_fourth [10, 20, 30, 40]) 40
  , check "double_last [1,2,3]" (sol_double_last [1, 2, 3]) 6
  , check "double_last [5]" (sol_double_last [5]) 10
  , check "four_and_five [5,1,4]" (sol_four_and_five [5, 1, 4]) True
  , check "four_and_five [4,6]" (sol_four_and_five [4, 6]) False
  , check "four_and_five []" (sol_four_and_five []) False
  , check "double_second_last [1,2,3,4]" (sol_double_second_last [1, 2, 3, 4]) 6
  , check "double_second_last [7,8]" (sol_double_second_last [7, 8]) 14
  , check "thin [1,2,3,4]" (sol_thin [1, 2, 3, 4]) [2, 3]
  , check "thin \"abc\"" (sol_thin "abc") "b"
  , check "thin [1,2]" (sol_thin [1, 2]) []
  , check "split_head_tail [1,2,3]" (sol_split_head_tail [1, 2, 3]) (1, [2, 3])
  , check "split_head_tail \"x\"" (sol_split_head_tail "x") ('x', "")
  , check "split_at 2 [1..5]" (sol_split_at 2 [1, 2, 3, 4, 5]) ([1, 2], [3, 4, 5])
  , check "split_at 0 [1,2]" (sol_split_at 0 [1, 2]) ([], [1, 2])
  , check "split_at 3 \"abc\"" (sol_split_at 3 "abc") ("abc", "")
  ]

tests_L6_key :: [Test]
tests_L6_key =
  [ check "upto 5" (sol_upto 5) [1, 2, 3, 4, 5]
  , check "upto 0" (sol_upto 0) []
  , check "between 3 7" (sol_between 3 7) [3, 4, 5, 6, 7]
  , check "between 4 4" (sol_between 4 4) [4]
  , check "strictly_between 3 7" (sol_strictly_between 3 7) [4, 5, 6]
  , check "strictly_between 3 4" (sol_strictly_between 3 4) []
  , check "ten_of 'x'" (sol_ten_of 'x') "xxxxxxxxxx"
  , check "ten_of 7" (sol_ten_of 7) [7, 7, 7, 7, 7, 7, 7, 7, 7, 7]
  , check "tens 4" (sol_tens 4) [10, 20, 30, 40]
  , check "tens 0" (sol_tens 0) []
  , check "cubes 4" (sol_cubes 4) [1, 8, 27, 64]
  , check "cubes 1" (sol_cubes 1) [1]
  , check "removeUppercase \"The Big Dog\"" (sol_removeUppercase "The Big Dog") "he ig og"
  , check "removeUppercase \"ABC\"" (sol_removeUppercase "ABC") ""
  , check "no_twos [1,2,3,2,4]" (sol_no_twos [1, 2, 3, 2, 4]) [1, 3, 4]
  , check "no_twos [2,2]" (sol_no_twos [2, 2]) []
  , check "take 10 primes" (take 10 sol_primes) [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  , check "total_length [[1,2,3],[4],[5,6]]" (sol_total_length [[1, 2, 3], [4], [5, 6]]) 6
  , check "total_length []" (sol_total_length []) 0
  ]

tests_L7_key :: [Test]
tests_L7_key =
  [ check "convertMe \"one\"" (sol_convertMe "one") 1
  , check "convertMe \"two\"" (sol_convertMe "two") 2
  , check "convertMe \"three\"" (sol_convertMe "three") 3
  , check "convertMe \"banana\"" (sol_convertMe "banana") 0
  , check "max_guard 3 9" (sol_max_guard 3 9) 9
  , check "max_guard 9 3" (sol_max_guard 9 3) 9
  , check "max_guard 4 4" (sol_max_guard 4 4) 4
  , check "factorial_square 1" (sol_factorial_square 1) 1
  , check "factorial_square 2" (sol_factorial_square 2) 4
  , check "factorial_square 3" (sol_factorial_square 3) 36
  , check "factorial_square 4" (sol_factorial_square 4) 576
  , check "pow 2 10" (sol_pow 2 10) 1024
  , check "pow 5 0" (sol_pow 5 0) 1
  , check "pow 3 3" (sol_pow 3 3) 27
  , check "odd_sum 0" (sol_odd_sum 0) 0
  , check "odd_sum 1" (sol_odd_sum 1) 1
  , check "odd_sum 5" (sol_odd_sum 5) 9
  , check "odd_sum 10" (sol_odd_sum 10) 25
  , check "div3 0" (sol_div3 0) True
  , check "div3 9" (sol_div3 9) True
  , check "div3 12" (sol_div3 12) True
  , check "div3 2" (sol_div3 2) False
  , check "div3 10" (sol_div3 10) False
  , check "div3 14" (sol_div3 14) False
  , check "fib_sum 0" (sol_fib_sum 0) 0
  , check "fib_sum 1" (sol_fib_sum 1) 0
  , check "fib_sum 2" (sol_fib_sum 2) 1
  , check "fib_sum 5" (sol_fib_sum 5) 7
  , check "fib_sum 7" (sol_fib_sum 7) 20
  , check "prime 2" (sol_prime 2) True
  , check "prime 7" (sol_prime 7) True
  , check "prime 13" (sol_prime 13) True
  , check "prime 97" (sol_prime 97) True
  , check "prime 1" (sol_prime 1) False
  , check "prime 4" (sol_prime 4) False
  , check "prime 9" (sol_prime 9) False
  , check "prime 100" (sol_prime 100) False
  ]

tests_L8_key :: [Test]
tests_L8_key =
  [ check "product' [1,2,3,4]" (sol_product' [1, 2, 3, 4]) 24
  , check "product' []" (sol_product' []) 1
  , check "count 'a' \"banana\"" (sol_count 'a' "banana") 3
  , check "count 3 [1,3,3,2,3]" (sol_count 3 [1, 3, 3, 2, 3]) 3
  , check "count 9 []" (sol_count 9 []) 0
  , check "cube_list [1,2,3]" (sol_cube_list [1, 2, 3]) [1, 8, 27]
  , check "cube_list []" (sol_cube_list []) []
  , check "mult_pairs [1,2,3,4]" (sol_mult_pairs [1, 2, 3, 4]) [2, 12]
  , check "mult_pairs [2,3]" (sol_mult_pairs [2, 3]) [6]
  , check "mult_pairs []" (sol_mult_pairs []) []
  , check "list_min [5,3,8,1,9]" (sol_list_min [5, 3, 8, 1, 9]) 1
  , check "list_min [7]" (sol_list_min [7]) 7
  , check "list_min \"hello\"" (sol_list_min "hello") 'e'
  , check "palindrome [1,2,3,2,1]" (sol_palindrome [1, 2, 3, 2, 1]) True
  , check "palindrome [1,2,2,1]" (sol_palindrome [1, 2, 2, 1]) True
  , check "palindrome [1,2,3]" (sol_palindrome [1, 2, 3]) False
  , check "palindrome []" (sol_palindrome []) True
  , check "palindrome \"racecar\"" (sol_palindrome "racecar") True
  , check "palindrome \"ab\"" (sol_palindrome "ab") False
  , check "separate [1..6]" (sol_separate [1, 2, 3, 4, 5, 6]) ([2, 4, 6], [1, 3, 5])
  , check "separate []" (sol_separate []) ([], [])
  , check "separate [3,5]" (sol_separate [3, 5]) ([], [3, 5])
  , check "second_min [5,3,8,1,9]" (sol_second_min [5, 3, 8, 1, 9]) 3
  , check "second_min [2,1]" (sol_second_min [2, 1]) 2
  , check "second_min [10,20,30]" (sol_second_min [10, 20, 30]) 20
  ]

tests_L9_key :: [Test]
tests_L9_key =
  [ check "twos_threes [1,2,3,4,2,5,3]" (sol_twos_threes [1, 2, 3, 4, 2, 5, 3]) [2, 3, 2, 3]
  , check "twos_threes []" (sol_twos_threes []) []
  , check "twos_threes [4,5]" (sol_twos_threes [4, 5]) []
  , check "product_lists [1,2,3] [4,5,6]" (sol_product_lists [1, 2, 3] [4, 5, 6]) [4, 10, 18]
  , check "product_lists [1,2,3] [2,2]" (sol_product_lists [1, 2, 3] [2, 2]) [2, 4]
  , check "product_lists [] [1]" (sol_product_lists [] [1]) []
  , check "list_gt [3,4,5] [1,2,3]" (sol_list_gt [3, 4, 5] [1, 2, 3]) True
  , check "list_gt [3,1,5] [1,2,3]" (sol_list_gt [3, 1, 5] [1, 2, 3]) False
  , check "list_gt [] []" (sol_list_gt [] []) True
  , check "caps_and_smalls \"Hello World\"" (sol_caps_and_smalls "Hello World") ("elloorld", "HW")
  , check "caps_and_smalls \"abc\"" (sol_caps_and_smalls "abc") ("abc", "")
  , check "caps_and_smalls \"ABC\"" (sol_caps_and_smalls "ABC") ("", "ABC")
  , check "caps_and_smalls \"a1B!\"" (sol_caps_and_smalls "a1B!") ("a", "B")
  , check "thirds [0..9]" (sol_thirds [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) [0, 3, 6, 9]
  , check "thirds \"abcdefg\"" (sol_thirds "abcdefg") "adg"
  , check "thirds []" (sol_thirds []) []
  , check "thirds [1,2]" (sol_thirds [1, 2]) [1]
  , check "mult_by_pos [5,5,5,5]" (sol_mult_by_pos [5, 5, 5, 5]) [0, 5, 10, 15]
  , check "mult_by_pos [10,20,30]" (sol_mult_by_pos [10, 20, 30]) [0, 20, 60]
  , check "mult_by_pos []" (sol_mult_by_pos []) []
  , check "merge [1,3,5] [2,4,6]" (sol_merge [1, 3, 5] [2, 4, 6]) [1, 2, 3, 4, 5, 6]
  , check "merge [] [1,2]" (sol_merge [] [1, 2]) [1, 2]
  , check "merge [1,2] []" (sol_merge [1, 2] []) [1, 2]
  , check "merge [1,1,2] [1,3]" (sol_merge [1, 1, 2] [1, 3]) [1, 1, 1, 2, 3]
  , check "merge_sort [5,2,9,1,5,6]" (sol_merge_sort [5, 2, 9, 1, 5, 6]) [1, 2, 5, 5, 6, 9]
  , check "merge_sort []" (sol_merge_sort []) []
  , check "merge_sort [1]" (sol_merge_sort [1]) [1]
  , check "merge_sort [3,2,1]" (sol_merge_sort [3, 2, 1]) [1, 2, 3]
  , check "merge_sort \"haskell\"" (sol_merge_sort "haskell") "aehklls"
  ]

tests_L10_key :: [Test]
tests_L10_key =
  [ check "(lecture code) caesar_enc \"hello there\" 3" (caesar_enc "hello there" 3) "khoor wkhuh"
  , check "(lecture code) caesar_dec \"khoor wkhuh\" 3" (caesar_dec "khoor wkhuh" 3) "hello there"
  , check "(lecture code) caesar_enc \"vwxyz\" 3" (caesar_enc "vwxyz" 3) "yzabc"
  , check "(lecture code) caesar_enc \"abcde\" 13" (caesar_enc "abcde" 13) "nopqr"
  , check "(lecture code) caesar_crack" (caesar_crack "lbh jvyy arire qrpbqr guvf") "you will never decode this"
  , check "shift2 'Z' 3" (sol_shift2 'Z' 3) 'C'
  , check "shift2 'A' 3" (sol_shift2 'A' 3) 'D'
  , check "shift2 'a' 3" (sol_shift2 'a' 3) 'd'
  , check "shift2 'x' 3" (sol_shift2 'x' 3) 'a'
  , check "shift2 '!' 3" (sol_shift2 '!' 3) '!'
  , check "caesar_enc2 \"Hello, World!\" 3" (sol_caesar_enc2 "Hello, World!" 3) "Khoor, Zruog!"
  , check "caesar_dec2 \"Khoor, Zruog!\" 3" (sol_caesar_dec2 "Khoor, Zruog!" 3) "Hello, World!"
  , check "caesar_enc3 \"aaaa\" 1 2" (sol_caesar_enc3 "aaaa" 1 2) "bcbc"
  , check "caesar_enc3 \"hello\" 1 2" (sol_caesar_enc3 "hello" 1 2) "ignmp"
  , check "caesar_enc3 \"a b\" 1 2" (sol_caesar_enc3 "a b" 1 2) "b c"
  , check "caesar_dec3 \"ignmp\" 1 2" (sol_caesar_dec3 "ignmp" 1 2) "hello"
  , check "crack_offset \"ifmmp\"" (sol_crack_offset "ifmmp") 1
  , check "crack_offset \"lbh jvyy arire qrpbqr guvf\"" (sol_crack_offset "lbh jvyy arire qrpbqr guvf") 13
  , check "crack_mixed \"Lbh jvyy arire qrpbqr guvf\"" (sol_crack_mixed "Lbh jvyy arire qrpbqr guvf") "You will never decode this"
  ]

tests_extra_key :: [Test]
tests_extra_key =
  [ check "count_evens [1,2,3,4,6]" (sol_count_evens [1, 2, 3, 4, 6]) 3
  , check "count_evens []" (sol_count_evens []) 0
  , check "filter_gt 3 [1,5,2,7,3,9]" (sol_filter_gt 3 [1, 5, 2, 7, 3, 9]) [5, 7, 9]
  , check "filter_gt 10 [1,2]" (sol_filter_gt 10 [1, 2]) []
  , check "interleave [1,2,3] [10,20]" (sol_interleave [1, 2, 3] [10, 20]) [1, 10, 2, 20, 3]
  , check "interleave \"abc\" \"xyz\"" (sol_interleave "abc" "xyz") "axbycz"
  , check "interleave [] [1,2]" (sol_interleave [] [1, 2]) [1, 2]
  , check "rotate_left 2 [1..5]" (sol_rotate_left 2 [1, 2, 3, 4, 5]) [3, 4, 5, 1, 2]
  , check "rotate_left 0 [1,2,3]" (sol_rotate_left 0 [1, 2, 3]) [1, 2, 3]
  , check "rotate_left 5 [1..5]" (sol_rotate_left 5 [1, 2, 3, 4, 5]) [1, 2, 3, 4, 5]
  , check "dedupe [1,1,2,2,2,3,1]" (sol_dedupe [1, 1, 2, 2, 2, 3, 1]) [1, 2, 3, 1]
  , check "dedupe \"aabbbc\"" (sol_dedupe "aabbbc") "abc"
  , check "dedupe []" (sol_dedupe []) []
  , check "is_sorted [1,2,2,5]" (sol_is_sorted [1, 2, 2, 5]) True
  , check "is_sorted [3,1,2]" (sol_is_sorted [3, 1, 2]) False
  , check "is_sorted []" (sol_is_sorted []) True
  , check "is_sorted [7]" (sol_is_sorted [7]) True
  , check "rle \"aaabccdddd\"" (sol_rle "aaabccdddd") [('a', 3), ('b', 1), ('c', 2), ('d', 4)]
  , check "rle \"\"" (sol_rle "") []
  , check "rle \"abc\"" (sol_rle "abc") [('a', 1), ('b', 1), ('c', 1)]
  , check "un_rle [('a',3),('b',1),('c',2)]" (sol_un_rle [('a', 3), ('b', 1), ('c', 2)]) "aaabcc"
  , check "un_rle []" (sol_un_rle []) ""
  , check "collatz_steps 1" (sol_collatz_steps 1) 0
  , check "collatz_steps 6" (sol_collatz_steps 6) 8
  , check "collatz_steps 7" (sol_collatz_steps 7) 16
  ]

suites_key :: [(String, String, [Test])]
suites_key =
  [ ("5", "Lecture 5: tuples, lists, pattern matching", tests_L5_key)
  , ("6", "Lecture 6: ranges and list comprehensions", tests_L6_key)
  , ("7", "Lecture 7: recursion, guards", tests_L7_key)
  , ("8", "Lecture 8: recursion on lists", tests_L8_key)
  , ("9", "Lecture 9: where, two lists, mutual recursion, sorting", tests_L9_key)
  , ("10", "Lecture 10: Caesar cipher", tests_L10_key)
  , ("extra", "Extra practice", tests_extra_key)
  ]


{-
################################################################################
################################################################################
##                                                                            ##
##                              S P O I L E R S                               ##
##                                                                            ##
##        ANSWER KEY BELOW.  Seriously, try the exercises first.              ##
##                                                                            ##
################################################################################
################################################################################
-}





































{-
================================================================================
  ANSWERS TO THE PREDICT / QUESTION SECTIONS
================================================================================
  P5.1  'e'                      P5.2  2  ([5,4,3,2,1] !! 3)
  P5.3  (1,2)                    P5.4  2
  P5.5  3                        P5.6  [6,7]
  P5.7  "abc"                    P5.8  True
  P5.9  (c): [1] : [2,3] is a type error, the left side of : must be ONE element.

  P6.1  [1,3,5,7,9]
  P6.2  [10,20,30,40,50,60,70,80,90,100]
  P6.3  [15,30,45,60]
  P6.4  [10,20,20,40]
  P6.5  "elloorld"
  P6.6  [[1,3],[5],[7]]
  P6.7  50      (1,3,...,99)
  P6.8  [1,2,4,5,10,20]

  Q7.1  m7a x = 1 + 2 + ... + x, the sum 1..x.  m7a 4 == 10.
  Q7.2  m7b x multiplies x * (x+1) * ... * (-1): it counts UP to the base case
        -1, so it only terminates for negative x. m7b (-4) = (-4)(-3)(-2)(-1) = 24.
        (For x >= 0 it counts away from -1 forever.)
  Q7.3  The catch-all  m7c n  is above  m7c 1  so it always matches first and
        the base case is never reached. Fix: put m7c 1 = 1 first.
  Q7.4  gcd' 400 60 -> gcd' 340 60 -> 280 60 -> 220 60 -> 160 60 -> 100 60
        -> 40 60 -> (b > a) gcd' 40 20 -> (a > b) gcd' 20 20 -> 20.
  Q7.5  multiply 3 4 = 3 + multiply 3 3 = 3+3+multiply 3 2 = 3+3+3+multiply 3 1
        = 3+3+3+3 = 12. multiply 3 0 never hits the base case (y == 1) and
        counts down 0, -1, -2, ... forever.

  Q8.1  m8a keeps only the even numbers (filter).   m8a [1..6] == [2,4,6]
        m8b duplicates every element.               m8b [1..6] == [1,1,2,2,...,6,6]
  Q8.2  down_from 3
        -> 3 : down_from 2
        -> 3 : (2 : down_from 1)
        -> 3 : (2 : (1 : down_from 0))
        -> 3 : (2 : (1 : []))  ==  [3,2,1]
  Q8.3  add_next [1,2,3,4]
        -> 1+2 : add_next [2,3,4]
        -> 3 : (2+3 : add_next [3,4])
        -> 3 : (5 : [3+4])      (add_next [x,y] = [x+y])
        -> [3,5,7]
  Q8.4  maximum' [] has no answer (what is the max of nothing?) so it errors.
        The recursion needs a starting value: [x] = x. If the base case were [],
        you'd have to invent a value (like minus infinity).
  Q8.5  No difference here: take' 0 [] = [] by the first clause (0 matches),
        and would also be [] by the second. The order matters only when both
        could match and give different answers (e.g. take' 0 [1,2] must be [],
        and it is, because clause 1 wins).

  Q9.1  m9a splits a list into its even-indexed and odd-indexed elements:
        m9a [1..6] == ([1,3,5],[2,4,6]). With 5 elements the last step hits
        m9a [_] and errors.
  Q9.2  m9b xs ys is True exactly when ys is xs with every element doubled:
        m9b [1,2] [1,1,2,2] == True, m9b [1,2] [1,1,2,3] == False.
        It checks the output of m8b.
  Q9.3  gt_10 [8,12,9]
        gt_10 [8,12,9] -> 8 not > 10 -> (gt, 8:lt) where (gt,lt) = gt_10 [12,9]
        gt_10 [12,9]   -> 12 > 10   -> (12:gt, lt) where (gt,lt) = gt_10 [9]
        gt_10 [9]      -> (gt, 9:lt) where (gt,lt) = gt_10 []   = ([], [9])
        gt_10 [12,9]   = ([12], [9])
        gt_10 [8,12,9] = ([12], [8,9])
  Q9.4  fib calls itself twice, recomputing the same values over and over
        (exponentially many calls). fast_fib_help computes each Fibonacci
        number once, keeping the previous ones in the returned list.
  Q9.5  qs [3,1,2] = qs [1,2] ++ [3] ++ qs []  (pivot 3: lower [1,2], upper [])
        qs [1,2] = qs [] ++ [1] ++ qs [2] = [1] ++ [2] = [1,2]
        so qs [3,1,2] = [1,2,3]
  Q9.6  fast_fib_help only has a base case for 1. fast_fib_help 0 calls
        fast_fib_help (-1), (-2), ... forever.

  Q10.1 shift 'z' 1 == 'a' (wraps),  shift 'a' (-1) == 'z' (mod of a negative
        is non-negative),  shift '!' 5 == '!' (non-lowercase unchanged).
  Q10.2 chi_squared pairs each of the 26 frequencies with the 26 English
        frequencies. It has no case for lists of different lengths.
  Q10.3 Each term (freq - english)^2 / english measures how far a letter is
        from its expected frequency. Close match -> small terms -> low sum.
  Q10.4 With very few letters the frequencies are lumpy and random, so the
        best-scoring offset is often wrong. Frequency analysis needs a
        decent amount of text.
  Q10.5 "Hkoor". Capital H is not 'a'..'z', so shift leaves it alone, and
        "ello" shifts to "hoor". (That's why exercise 10.2 exists.)

================================================================================
  SOLUTIONS
================================================================================
  All solution names start with  sol_  so they don't clash with your own.
  (They're one possible answer each; yours may differ and still be correct.)
-}

-- ----------------------------- Lecture 5 -----------------------------------
sol_x_and_double x = (x, 2 * x)

sol_middle (_, b, _) = b

sol_join l1 l2 = l1 ++ l2

sol_happy s = s ++ " :-)"

sol_fourth list = list !! 3

sol_double_last list = 2 * last list

sol_four_and_five list = elem 4 list && elem 5 list

sol_double_second_last list = 2 * last (init list)

sol_thin list = tail (init list)

sol_split_head_tail list = (head list, tail list)

sol_split_at x list = (take x list, drop x list)

-- ----------------------------- Lecture 6 -----------------------------------
sol_upto x = [1 .. x]

sol_between x y = [x .. y]

sol_strictly_between x y = [x + 1 .. y - 1]

sol_ten_of x = take 10 (repeat x)

sol_tens n = [10 * x | x <- [1 .. n]]

sol_cubes n = [x * x * x | x <- [1 .. n]]

sol_removeUppercase st = [c | c <- st, not (c `elem` ['A' .. 'Z'])]

sol_no_twos xs = [x | x <- xs, x /= 2]

sol_primes :: [Integer]
sol_primes = [x | x <- [2 ..], length (factors x) == 2]

sol_total_length xxs = sum [length xs | xs <- xxs]

-- ----------------------------- Lecture 7 -----------------------------------
sol_convertMe "one" = 1
sol_convertMe "two" = 2
sol_convertMe "three" = 3
sol_convertMe _ = 0

sol_max_guard a b
  | a > b = a
  | otherwise = b

sol_factorial_square 1 = 1
sol_factorial_square x = x * x * sol_factorial_square (x - 1)

sol_pow _ 0 = 1
sol_pow x y = x * sol_pow x (y - 1)

sol_odd_sum 0 = 0
sol_odd_sum x
  | x `mod` 2 == 1 = x + sol_odd_sum (x - 1)
  | otherwise = sol_odd_sum (x - 1)

sol_div3 0 = True
sol_div3 1 = False
sol_div3 2 = False
sol_div3 x = sol_div3 (x - 3)

sol_fib_sum 0 = 0
sol_fib_sum x = fibonacci (x - 1) + sol_fib_sum (x - 1)

sol_prime x = x > 1 && sol_divisions x (x - 1)

sol_divisions _ 1 = True
sol_divisions x y
  | x `mod` y == 0 = False
  | otherwise = sol_divisions x (y - 1)

-- ----------------------------- Lecture 8 -----------------------------------
sol_product' [] = 1
sol_product' (x:xs) = x * sol_product' xs

sol_count _ [] = 0
sol_count x (y:ys)
  | x == y = 1 + rest
  | otherwise = rest
  where rest = sol_count x ys

sol_cube_list [] = []
sol_cube_list (x:xs) = x * x * x : sol_cube_list xs

sol_mult_pairs [] = []
sol_mult_pairs [_] = error "mult_pairs: odd number of elements"
sol_mult_pairs (x:y:rest) = x * y : sol_mult_pairs rest

sol_list_min [] = error "list_min: empty list"
sol_list_min [x] = x
sol_list_min (x:xs)
  | x < m = x
  | otherwise = m
  where m = sol_list_min xs

sol_palindrome [] = True
sol_palindrome [_] = True
sol_palindrome list = head list == last list && sol_palindrome (tail (init list))

sol_separate [] = ([], [])
sol_separate (x:xs)
  | even x = (x : es, os)
  | otherwise = (es, x : os)
  where (es, os) = sol_separate xs

sol_remove_first _ [] = []
sol_remove_first e (x:xs)
  | e == x = xs
  | otherwise = x : sol_remove_first e xs

sol_second_min list = sol_list_min (sol_remove_first (sol_list_min list) list)

-- ----------------------------- Lecture 9 -----------------------------------
sol_twos_threes [] = []
sol_twos_threes (x:xs)
  | x == 2 || x == 3 = x : rest
  | otherwise = rest
  where rest = sol_twos_threes xs

sol_product_lists _ [] = []
sol_product_lists [] _ = []
sol_product_lists (x:xs) (y:ys) = x * y : sol_product_lists xs ys

sol_list_gt _ [] = True
sol_list_gt [] _ = True
sol_list_gt (x:xs) (y:ys) = x > y && sol_list_gt xs ys

sol_caps_and_smalls [] = ([], [])
sol_caps_and_smalls (x:xs)
  | x `elem` ['a' .. 'z'] = (x : smalls, caps)
  | x `elem` ['A' .. 'Z'] = (smalls, x : caps)
  | otherwise = (smalls, caps)
  where (smalls, caps) = sol_caps_and_smalls xs

-- thirds keeps index 0, skips 1 and 2, keeps 3, ...  (mutual recursion x3)
sol_thirds [] = []
sol_thirds (x:xs) = x : sol_thirds_skip1 xs

sol_thirds_skip1 [] = []
sol_thirds_skip1 (_:xs) = sol_thirds_skip2 xs

sol_thirds_skip2 [] = []
sol_thirds_skip2 (_:xs) = sol_thirds xs

sol_mult_by_pos list = sol_mult_helper (zip list [0 ..])

sol_mult_helper [] = []
sol_mult_helper ((x, i) : rest) = x * i : sol_mult_helper rest

sol_merge [] ys = ys
sol_merge xs [] = xs
sol_merge (x:xs) (y:ys)
  | x <= y = x : sol_merge xs (y : ys)
  | otherwise = y : sol_merge (x : xs) ys

sol_merge_sort list
  | length list <= 1 = list
  | otherwise = sol_merge (sol_merge_sort first_half) (sol_merge_sort second_half)
  where half = length list `div` 2
        first_half = take half list
        second_half = drop half list

-- ----------------------------- Lecture 10 ----------------------------------
sol_shift2 :: Char -> Int -> Char
sol_shift2 c offset
  | isLower c = rotate 'a'
  | isUpper c = rotate 'A'
  | otherwise = c
  where rotate base = chr (ord base + (ord c - ord base + offset) `mod` 26)

sol_caesar_enc2 :: String -> Int -> String
sol_caesar_enc2 [] _ = []
sol_caesar_enc2 (x:xs) offset = sol_shift2 x offset : sol_caesar_enc2 xs offset

sol_caesar_dec2 :: String -> Int -> String
sol_caesar_dec2 string offset = sol_caesar_enc2 string (negate offset)

-- swap the two offsets at every step: even positions use o1, odd use o2
sol_caesar_enc3 :: String -> Int -> Int -> String
sol_caesar_enc3 [] _ _ = []
sol_caesar_enc3 (x:xs) o1 o2 = shift x o1 : sol_caesar_enc3 xs o2 o1

sol_caesar_dec3 :: String -> Int -> Int -> String
sol_caesar_dec3 string o1 o2 = sol_caesar_enc3 string (negate o1) (negate o2)

sol_crack_offset :: String -> Int
sol_crack_offset string =
  snd (minimum [(chi_squared_string (caesar_dec string i), i) | i <- [0 .. 25]])

sol_crack_mixed :: String -> String
sol_crack_mixed string = sol_caesar_dec2 string (sol_crack_offset (map toLower string))

sol_crack_two :: String -> String
sol_crack_two string = sol_caesar_dec3 string o1 o2
  where o1 = sol_crack_offset (evens string)
        o2 = sol_crack_offset (odds string)

-- ------------------------------- Extras ------------------------------------
sol_count_evens [] = 0
sol_count_evens (x:xs)
  | even x = 1 + rest
  | otherwise = rest
  where rest = sol_count_evens xs

sol_filter_gt _ [] = []
sol_filter_gt n (x:xs)
  | x > n = x : rest
  | otherwise = rest
  where rest = sol_filter_gt n xs

sol_interleave [] ys = ys
sol_interleave xs [] = xs
sol_interleave (x:xs) (y:ys) = x : y : sol_interleave xs ys

sol_rotate_left n list = drop n list ++ take n list

sol_dedupe [] = []
sol_dedupe [x] = [x]
sol_dedupe (x:y:rest)
  | x == y = sol_dedupe (y : rest)
  | otherwise = x : sol_dedupe (y : rest)

sol_is_sorted [] = True
sol_is_sorted [_] = True
sol_is_sorted (x:y:rest) = x <= y && sol_is_sorted (y : rest)

-- sol_run_len c xs: how many leading elements equal c, and what is left over
sol_run_len _ [] = (0, [])
sol_run_len c (y:ys)
  | c == y = (n + 1, rest)
  | otherwise = (0, y : ys)
  where (n, rest) = sol_run_len c ys

sol_rle [] = []
sol_rle (x:xs) = (x, n + 1) : sol_rle rest
  where (n, rest) = sol_run_len x xs

sol_un_rle [] = []
sol_un_rle ((c, n) : rest) = replicate n c ++ sol_un_rle rest

sol_collatz_steps 1 = 0
sol_collatz_steps n
  | even n = 1 + sol_collatz_steps (n `div` 2)
  | otherwise = 1 + sol_collatz_steps (3 * n + 1)
