import Auto
import System.IO
import System.Environment
import Text.Read
import Numeric.Natural
import Data.Char
import Data.Maybe
import Data.List

-- type MN = Maybe [Natural]
-- type MS = Maybe (Natural, String, [Natural])
-- type MT = Maybe (Natural, Char, [Natural])

main :: IO ()
main = do
  [filePath] <- getArgs
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  -- let errMsg = "BAD INPUT"
  --     ((st, is, ia, tl), word)= parseInput contents
      -- aut = fromLists st is ia tl
  -- print aut
  hClose handle

parseTransitionLine :: String -> Maybe [(Natural, Char, [Natural])]
parseTransitionLine l | length ws < 3 = Nothing
                      | isNothing q' = Nothing
                      | not $ all isUpper cs = Nothing
                      | any isNothing qs' = Nothing
                      | otherwise = Just $ nub [ (fromJust q', c, map fromJust qs') | c <- cs ]
  where ws = words l
        q:cs:qs = ws
        q' = readMaybe q :: Maybe Natural
        qs' = map (\n -> readMaybe n :: Maybe Natural) qs

parseInput :: String -> Maybe ((Natural, [Natural], [Natural], [(Natural, Char, [Natural])]), String)
parseInput input | length strings < 4 = Nothing
                 | isNothing numStates = Nothing
                 | isNothing startStates = Nothing
                 | isNothing acceptStates = Nothing
                 | any isNothing transitionList = Nothing
                 | not $ all isUpper word = Nothing
                 | otherwise =
                   Just ((fromJust numStates, fromJust startStates,
                   fromJust acceptStates, concatMap fromJust transitionList), word)
  where isEmptyLine l = null l || all isSpace l
        strings = filter (not . isEmptyLine) $ lines input
        numStatesStr:startStatesStr:acceptStatesStr:restStrList = strings
        numStates = readMaybe numStatesStr :: Maybe Natural
        startStates = readMaybe startStatesStr :: Maybe [Natural]
        acceptStates = readMaybe acceptStatesStr :: Maybe [Natural]
        transitionList = map parseTransitionLine $ init restStrList
        word = last restStrList -- TODO strip

-- numStates <- readMaybe numStatesStr :: Maybe Natural

-- parseInput :: String -> Maybe ((Natural, [Natural], [Natural], [(Natural, Char, [Natural])]), String)
-- parseInput input =
--   let isEmptyLine l = null l || all isSpace l
--       strings = filter (not . isEmptyLine) $ lines input
--   in if length strings < 4 then Nothing
--      else
--        let numStatesStr:startStatesStr:acceptStatesStr:restStrList = strings
--            word = last strings -- TODO strip
--            numStates = readMaybe numStatesStr :: Maybe Natural
--            startStates = readMaybe startStatesStr :: [Natural]
--            acceptStates = readMaybe acceptStatesStr :: [Natural]
--            parseLine l = let ws = words l in
--              if length ws < 3 then Nothing
--              else let q:cs:qs = ws
--                       q' = readMaybe q :: Natural
--                       cs' = if all isUpper cs then cs else Nothing
--                       qs' = map (\n -> readMaybe n :: Natural) qs
--                   in if isNothing q' || isNothing cs' || any isNothing qs' then Nothing else
--                     Just [ (fromJust q', c, map fromJust qs') | c <- fromJust cs' ]
--            transitionMaybeList = map parseLine $ init strings
--            transitionList = if any isNothing transitionMaybeList then Nothing else map fromJust ransitionMaybeList

-- parseInput :: String -> Maybe ((Natural, [Natural], [Natural], [(Natural, Char, [Natural])]), String)
-- parseInput input =
--   let isEmptyLine l = null l || all isSpace l
--       strings = filter (not . isEmptyLine) $ lines input
--   in if length strings < 4 then Nothing
--      else
--        let numStatesStr:startStatesStr:acceptStatesStr:restStrList = strings
--            word = last strings -- TODO strip
--            numStates = readMaybe numStatesStr :: Maybe Natural
--            startStates = readMaybe startStatesStr :: [Natural]
--            acceptStates = readMaybe acceptStatesStr :: [Natural]
--            parseLine l = let ws = words l in
--              if length ws < 3 then Nothing
--              else let q:cs:qs = ws
--                       q' = readMaybe q :: Natural
--                       cs' = if all isUpper cs then cs else Nothing
--                       qs' = map (\n -> readMaybe n :: Natural) qs
--                   in if isNothing q' || isNothing cs' || any isNothing qs' then Nothing else
--                     Just [ (fromJust q', c, map fromJust qs') | c <- fromJust cs' ]
--            transitionMaybeList = map parseLine $ init strings
--            transitionList = if any isNothing transitionMaybeList then Nothing else map fromJust ransitionMaybeList

-- Wywalić maybe bez propagacji
-- parseInput :: String -> ((Maybe Natural, MN, MN, [MT]), Maybe String)
-- parseInput input = ((numStates, startStates, acceptStates, transitionList), word)
--   where
--     empty l = null l || all isSpace l
--     numStatesStr:startStatesStr:acceptStatesStr:restStrList = filter (not . empty) $ lines input
--     -- numStatesStr:startStatesStr:acceptStatesStr:restStrList = filter (not . null) $ lines input
--     word = if null restStrList then Nothing else Just $ last restStrList
--     numStates = readMaybe numStatesStr :: Maybe Natural
--     startStates = readMaybe startStatesStr :: MN
--     acceptStates = readMaybe acceptStatesStr :: MN
--     transitionList = if null restStrList then Nothing else
    -- transitionList = if null restStrList then [Nothing] else
    --   let transitionCompressedList = map (\l -> readMaybe l :: MS) (init restStrList)
    --       uncompress (q, cs, qs) = [ Just (q, c, qs) | c <- cs ]
    --   in concatMap (maybe [Nothing] uncompress) transitionCompressedList

-- createAuto :: ((Maybe Natural, MN, MN, [MT]), Maybe String) -> Maybe (Auto Char Natural)
-- createAuto ((numStates, startStates, acceptStates, transitionList), word) = do
--
-- startStates ++ acceptStates ++ map (\(q, c, qs)) transitionList
