import Auto
import System.IO
import System.Environment
import Text.Read
import Numeric.Natural
import Data.Char
import Data.Maybe
import Data.List

main :: IO ()
main = do
  let errMsg = "BAD INPUT"
  filePath:args <- getArgs
  if not $ null args then print errMsg
  else do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let parsed = parseInput contents
    if isNothing parsed then print errMsg
    else
      let (t@(n, is, ia, tr), word) = fromJust parsed
          st = generateStateList t
      in if length st > n then print errMsg
        else
          let aut = fromLists st is ia tr
          in print $ accepts aut word
    hClose handle

-- printError :: String -> IO ()
-- printError e = let errMsg = "BAD INPUT: " in print $ errMsg ++ e

-- mergować przejścia postaci q, ABC -> [1,2,3]; q, ABC -> [3,4,5]

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

parseTransitionLine :: String -> Maybe [(Natural, Char, [Natural])]
parseTransitionLine l | length ws < 3        = Nothing
                      | isNothing q'         = Nothing
                      | not $ all isUpper cs = Nothing
                      | any isNothing qs'    = Nothing
                      | otherwise = Just $ nub [ (fromJust q', c, map fromJust qs') | c <- cs ]
  where ws = words l
        q:cs:qs = ws
        q' = readMaybe q :: Maybe Natural
        qs' = map (\n -> readMaybe n :: Maybe Natural) qs

mergeTransitions :: [(Natural, Char, [Natural])] -> [(Natural, Char, [Natural])]
mergeTransitions l = unionMap $ groupBy transitionEq $ sort l
  where
    transitionEq :: (Natural, Char, [Natural]) -> (Natural, Char, [Natural]) -> Bool
    transitionEq (q1, c1, _) (q2, c2, _) = q1 == q2 && c1 == c2
    unionMap :: [[(Natural, Char, [Natural])]] -> [(Natural, Char, [Natural])]
    unionMap = map (\trl -> let ([q], [c], qsl) = unzip3 trl
                            in (q, c, foldl union [] qsl))

parseInput :: String -> Maybe ((Int, [Natural], [Natural], [(Natural, Char, [Natural])]), String)
parseInput input | length strings < 4           = Nothing
                 | isNothing numStates          = Nothing
                 | isNothing startStates        = Nothing
                 | isNothing acceptStates       = Nothing
                 | any isNothing transitionList = Nothing
                 | not $ all isUpper word       = Nothing
                 | otherwise =
                   Just ((fromJust numStates, fromJust startStates,fromJust acceptStates,
                   mergeTransitions $ concatMap fromJust transitionList), word)
  where isEmptyLine l = null l || all isSpace l
        strings = filter (not . isEmptyLine) $ lines input
        numStatesStr:startStatesStr:acceptStatesStr:restStrList = strings
        numStates = readMaybe numStatesStr :: Maybe Int
        startStates = readMaybe startStatesStr :: Maybe [Natural]
        acceptStates = readMaybe acceptStatesStr :: Maybe [Natural]
        transitionList = map parseTransitionLine $ init restStrList
        word = trim $ last restStrList

generateStateList :: (Int, [Natural], [Natural], [(Natural, Char, [Natural])]) -> [Natural]
generateStateList (_, is, ia, tr) =
  nub $ is ++ ia ++ map (\(q, _, _) -> q) tr ++ concatMap (\(_, _, qs) -> qs) tr
