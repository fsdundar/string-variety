-- === LICENSE ===
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

-- Copyright 2021, Furkan Semih DÜNDAR
-- Email: f.semih.dundar@yandex.com


-- In this code, we try to implement Barbour & Smolin's maximal
-- variety function. The one suggested by David Deutsch.
-- Barbour refers to arXiv:9203041

import System.Environment
import System.IO
import Data.List
import Text.EditDistance

-- ===============================================
-- PART #1: CODE TO IMPLEMENT THE VARIETY FUNCTION
-- ===============================================
    
-- cyclic indices
index string i = string !! (mod i (length string))

-- Returns the mirror image of a string
mirror string = reverse string

-- cycles the string by increment i
stringCycle string i = [index string j | j <- [i..(length string)+i-1]]

-- Barbour p .13 Definition due to Deutsch
nim string i m = [index string (i+j) | j <- [(-m)..m]]

-- We use this function to calculate (see the next function) kij in Barbour p. 14
listKij string i j = [(nim string i m, nim string j m) | m <- [1..mStar]]
    where
        p = length string
        mStar = if mod p 2 == 0 then div p 2 - 1; else div (p-1) 2

-- Barbour p. 14
kij string i j =
  if try == []
    then 0
    else fst $ head $ try
    where try = filter snd $ zip [1..] $ (map (\(a,b) -> (a /= b) && (a /= mirror b)) $ listKij string i j)


-- Barbour p. 14
ri string i = foldr1 max [kij string i j | j <- [0..(length string - 1)], j /= i]

-- Barbour p. 14 eqn. (5)
varietyP string = 
  if (length.nub) string <= 1
    then 0
    else sum [recip (toRational (ri string i)) | i <- [0..(length string - 1)]]

-- The function returns True if the configuration is Leibnizian in the sense of Barbour p. 17 and returns False otherwise
leibnizianQ string = and [ (kij string i j) > 0 | i <- [0..(p-1)], j <- [0..(p-1)], i < j]
    where
        p = length string
        mStar = if mod p 2 == 0 then div p 2 - 1; else div (p-1) 2

-- =============================================================
-- PART #2: CODE TO IMPLEMENT COMPARISON AND SELECTION FUNCTIONS
-- =============================================================

--Just to reformat Frac % as /
replacePercentage string = map (\x -> if x == '%' then '/'; else x) string

-- creates [x,x,..,x] where there are n x's
multiplyList 0 x = []
multiplyList n x = x : (multiplyList (n-1) x)

-- creates all the possible matchings in [x,x,..,x]
space x n = map concat $ sequence $ multiplyList n x

-- selects the strings with maximal variety in a given list of strings which are Leibnizian
maximalVarietyStrings xs = [x | x <- vars, fst x == m]
    where
        vars = filter (leibnizianQ.snd) $ map (\x -> (varietyP x, x)) xs
        m = foldl1 max $ map fst vars

-- Returns the maximal variety that can be found on a string of length n letters.
-- currently we consider two distinct letters. you may modify the chars variable if needed.
maximalVariety n = foldl1 max $ map varietyP $ space chars n
    where
        chars = ["X","-"]

-- Returns the maximal variety that can be found on a string of length n letters where candidate strings are Leibnizian.
-- currently we consider two distinct letters. you may modify the chars variable if needed.
maximalVarietyLeibnizian n = foldl1 max $ map varietyP $ filter leibnizianQ $ space chars n
    where
        chars = ["X","-"]

-- Returns a list of maximal variety Leibnizian strings of length n.
maximalVarietyLeibnizianStrings n = filter (\x -> varietyP x == m) $ filter leibnizianQ $ space chars n
    where
        m = maximalVarietyLeibnizian n
        chars = ["X","-"]

-- match strings. returns the number of letters which occur at the same place
match xs ys = sum $ map (\(a,b) -> if a == b then 1; else 0) $ zip xs ys

allCyclicConfigurations xs = [stringCycle xs i | i <- [0..(length xs - 1)]]

-- best-match strings in the sense of Barbour
-- xs is a string that is known, and yss are candidates to match againts
-- the function returns a list of ys that are best matches
bestMatchStringsBarbour xs yss = [(i, ys) | (i, ys) <- matches, i == m]
    where
        matches = [(match xc yc, ys) | ys <- yss, yc <- allCyclicConfigurations ys, xc <- allCyclicConfigurations xs]
        m = foldl1 max $ map fst matches

--best-match strings using the Levenshtein edit distance
bestMatchStrings xs yss = [(i, ys) | (i, ys) <- matches, i == m]
    where
        matches = [(levenshteinDistance defaultEditCosts xc yc, ys) | ys <- yss, yc <- allCyclicConfigurations ys, xc <- allCyclicConfigurations xs]
        m = foldl1 min $ map fst matches


nextMaximalVarietyStrings xs = map snd $ bestMatchStrings xs $ map snd $ maximalVarietyStrings $ space chars (len + 1)
    where
        len = length xs
        chars = ["X","-"]

nextLeibnizianMaximalVarietyStrings xs = filter leibnizianQ $ map snd $ bestMatchStrings xs $ map snd $ maximalVarietyStrings $ space chars (len + 1)
    where
        len = length xs
        chars = ["X","-"]


maximalVarietyChainStep xss = [ y:xs | xs <- xss, y <- (nextLeibnizianMaximalVarietyStrings.head) xs]

maximalVarietyChain xss 1 = maximalVarietyChainStep xss
maximalVarietyChain xss n = maximalVarietyChainStep $ maximalVarietyChain xss (n-1)

-- Returns True if s1 is isomorphic to s2, False otherwise. This includes cyclic and mirror symmetries
stringIsomorphQ s1 s2 = or $ [s1 == stringCycle s2 i | i <- [1..(length s2)]] ++ [s1 == stringCycle (mirror s2) i | i <- [1..(length s2)]]

-- Returns true if two histories are the same modulo symmetries (cyclic, mirror)
isomorphHistoriesQ xs ys = and $ map (\(x,y) -> stringIsomorphQ x y) $ zip xs ys


-- =========================================
-- PART #3: CODE TO IMPLEMENT STRING ALGEBRA
-- =========================================

-- Injects the string ys in to xs after the nth element of xs
injectStrAt xs ys n = (take n xs) ++ ys ++ (drop n xs)

-- Injects the string ys in to xs at all possible places
injectStr xs ys = [injectStrAt xs ys n | n <- [0..(length xs - 1)]]

-- Symmetric injection of strings
injectStrSym xs ys = injectStr xs ys ++ injectStr ys xs

-- Find a substring xs of ys
matchSubStr xs ys = [ (xs, drop len zs) | zs <- allCyclicConfigurations ys, matchHead xs zs ]
    where
        len = length xs

-- Returns True if xs matches the head of zs, and False otherwise.
matchHead xs zs = if xs == take len zs then
                    True
                  else
                    False
    where len = length xs

-- Removes a substring xs from ys (of length n) of maximal variety Leibnizian strings
removeStr xs n = [ drop len zs | ys <- ysCandidate, zs <- allCyclicConfigurations ys, matchHead xs zs ]
    where
        len = length xs
        ysCandidate = maximalVarietyLeibnizianStrings n

-- Removes all Leibnizian maximal variety strings of length m from those that of lenght n.
removeStrMN m n = [ (xs, ys, varietyP xs, varietyP ys) | xs <- xsCandidate, ys <- removeStr xs n , leibnizianQ ys]
    where
        xsCandidate = maximalVarietyLeibnizianStrings m

-- Adds two maximal variety strings of lengths m and n and returns the maximum of variety that is reached.
addStrMN m n = [ (x,y,z, varietyP x, varietyP y, varietyP z) | (x,y,z) <- zsCandidate, varietyP z == mVar ]
    where
        xsCandidate = maximalVarietyLeibnizianStrings m
        ysCandidate = maximalVarietyLeibnizianStrings n
        -- Leibnizian additions
        zsCandidate = [ (xs, ys, xs ++ ys) | xs <- xsCandidate, ys <- ysCandidate, leibnizianQ (xs ++ ys)]
        mVar = foldl1 max $ map (\(_,_,x) -> varietyP x) zsCandidate

main = do
    -- putStrLn "============================="
    -- putStrLn "=== BEST MATCHING HISTORY ==="
    -- putStrLn "============================="
    
    -- mapM (putStrLn.show.reverse) $ nubBy isomorphHistoriesQ $ maximalVarietyChain [["XX-X---"]] 3
    
    
    -- mapM (putStrLn.show.reverse) $ nubBy isomorphHistoriesQ $ maximalVarietyChain [["XXX-X---"]] 3

    --mapM (putStrLn.show) $ maximalVarietyStrings $ space ["X","-"] 7

    -- Prints out the maximum varities of strins of lengths n = 6..20 
    --putStrLn "======================"
    --putStrLn "MAXIMAL VARIETY VALUES"
    --putStrLn "======================"
    --mapM (putStrLn.show) $ map (\n -> (n, maximalVarietyLeibnizian n)) [6..20]

    -- putStrLn "=== Length 3 ==="
    -- mapM (putStrLn.show) $ filter (\(x,_,_) -> x) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 3
    -- putStrLn "=== Length 4 ==="
    -- mapM (putStrLn.show) $ filter (\(x,_,_) -> x) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 4
    -- putStrLn "=== Length 5 ==="
    -- mapM (putStrLn.show) $ filter (\(x,_,_) -> x) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 5
    -- putStrLn "=== Length 6 ==="
    -- mapM (putStrLn.show) $ filter (\(x,_,_) -> x) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 6
    -- putStrLn "=== Length 7 ==="
    --mapM (putStrLn.show) $ map (\(_,x,y) -> (x,y)) $ filter (\(x,_,_) -> x) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 7
    
    -- mapM (putStrLn.show) $ map (\x -> (x, varietyP x)) $ space ["X","-"] 7
    
    -- Prints out maximal varieties of string length n which are Leibnizian
    --mapM (putStrLn.replacePercentage.show) $ map (\n -> (n, maximalVarietyLeibnizian n)) [6..15]

    -- Print variety of Leibnizian configuration of length n
    --putStrLn "String, Variety"
    --mapM (putStrLn) $ map (\(_,x,y) -> x ++ ", " ++ show (fromRational y)) $ filter (\(x,_,_) -> x) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 15

    -- Print the vairety of all stings of length n
    --putStrLn "String, Variety"
    --mapM (putStrLn) $ map (\(_,x,y) -> x ++ ", " ++ show (fromRational y)) $ map (\x -> (leibnizianQ x, x, varietyP x)) $ space ["X","-"] 15

    -- Print the Levenshtein distance between two texts
    --putStrLn $ show $ levenshteinDistance defaultEditCosts "X-" "-X"
    --putStrLn $ show $ restrictedDamerauLevenshteinDistance defaultEditCosts "X-" "-X"



    -- ===========================
    -- ======= MV CHAIN ==========
    -- ===========================
    -- Take the input from command line (str) and find out and print the next maximal variety strings of length (1 + len str)
    --args <- getArgs
    --let str = args !! 0
    --mapM (putStrLn.show.reverse) $ nubBy isomorphHistoriesQ $ maximalVarietyChain [[str]] 1
    --mapM (putStrLn.show.reverse) $ nubBy isomorphHistoriesQ $ maximalVarietyChain [["XX-X---"]] 3

    -- Takes two command line arguments and prints out the Levenshtein edit distance between them
    --args <- getArgs
    --let str1 = args !! 0
    --let str2 = args !! 1
    --putStrLn $ show $ levenshteinDistance defaultEditCosts str1 str2
    

    -- ===========================
    -- ===== STRING ALGEBRA ======
    -- ===========================
    --args <- getArgs
    --let str1 = args !! 0
    --let str2 = args !! 1
    
    -- Take two command line arguments and inject symmetrically one string into another at all possible places
    -- and print out the configurations with maximum variety.
    --let leibnizianInjections = map (\x -> (varietyP x, x)) $ nub $ filter leibnizianQ $ injectStrSym str1 str2
    --let m = foldl1 max $ map fst leibnizianInjections
    --mapM (putStrLn.show) $ [ (length x, x, n) | (n, x) <- leibnizianInjections, n == m]

    -- Take two command line arguments and returns matches if xs is a substring of ys.
    --mapM (putStrLn.show) $ nub $ matchSubStr str1 str2

    -- Take two command line arguments and subtracts xs from maximal variety Leibnizian strings of length n
    -- if there is a match and calculates the variety of the remaining string.
    --let m = read str1 :: Int
    --let n = read str2 :: Int
    --mapM (putStrLn.show) $ nub $ removeStrMN m n
    --mapM (putStrLn.show) $ nub $ addStrMN m n
    --mapM (putStrLn.show) $ map (\x -> (length x, x, varietyP x))

    -- ============================
    -- ===== SAMPLE PROGRAM =======
    -- ============================
    -- Takes one command line argument and returns if the string is Leibnizian or not and its variety.
    args <- getArgs
    let str = args !! 0
    putStrLn $ "String: " ++ str
    if leibnizianQ str then
        putStrLn "Leibnizian: True"
    else
        putStrLn "Leibnizian: False"
    
    putStrLn $ "Variety: " ++ show (varietyP str) 