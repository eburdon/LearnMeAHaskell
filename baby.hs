-- random beginning stuff

addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first (x, _ , _) = x
second (_, y, _ ) = y
third (_, _, z) = z

head' [] = error "You can't call head on an empty list, dummy!"
head' (x:_) = x

length' [] = 0
length' (_:xs) = 1 + length' xs

sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell bmi
    | bmi <= 18.5   = "You're underweight, you emo, you!"
    | bmi <= 25.0   = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0   = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"

-- recursion

maximum' [] = error "No max in an empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail   = x
    | otherwise     = maxTail
    where maxTail   = maximum' xs

replicate' n x
    | n <= 0= []
    | otherwise = x : replicate' (n-1) x

quicksort [] = []
quicksort (x:xs) =
    let smallerSorted   = quicksort [ a | a <- xs, a <= x ]
        biggersorted    = quicksort [ a | a <- xs, a > x ]
    in smallerSorted ++ [x] ++ biggersorted

-- Higher order functions: currying, etc...

-- Haskell functions can take functions as parameters and return functions as return values. A function that does either of those is called a higher order function.
-- [haskell] ... define computations by defining what stuff is

-- Higher order function:
-- * Takes other functions as arguments
-- * returns function as a result
-- Major use: abstract common functions

----- CURRYING ----

-- instead of...
doubleList [] = []
doubleList (x : xs) = 2*x: doubleList xs

-- .. and ...
tripleList [] = []
tripleList (x : xs) = 3*x : tripleList xs

-- ... we can parameterize OUT the difference

multList n [] = []
multList n (x : xs) = n*x : multList n xs

-- ... and define a less error prone version like ...
tripleList_ = multList 3
doubleList_ = multList 2

--  C, , Java, Pascal, Ada, and so on, are all imperative languages. They are "imperative" in the sense that they consist of a sequence of commands, which are executed strictly one after the other.
-- Haskell is a functional language. A functional program is a single expression, which is executed by evaluating the expression
-- The focus is on what is to be computed, not how it should be computed

-- This focus on the high-level "what" rather than the low-level "how" is a distinguishing characteristic of functional programming languages.

-- Another well-known nearly-functional language is the standard database query language SQL. An SQL query is an expression involving projections, selections, joins and so forth. The query says what relation should be computed, without saying how it should be computed. Indeed, the query can be evaluated in any convenient order. SQL implementations often perform extensive query optimization which (among other things) figures out the best order in which to evaluate the expression.

-- The first thing to know about Haskell's syntax is that parentheses are used for grouping, and not for function application.

-- The application of a function f to an argument x is written f x, not necessarily f(x). It can be written as (f x) to separate it from its surroundings.


