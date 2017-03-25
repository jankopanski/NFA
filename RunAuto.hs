import Auto
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
    contents <- readFile filePath
    let parsed = parseInput contents
    if isNothing parsed then print errMsg
    else
      let (t@(n, is, ia, tr), word) = fromJust parsed
          st = generateStateList t
      in if length st > n then print errMsg
        else
          let aut = fromLists st is ia tr
          in print $ accepts aut word

newtype Alpha = Alpha Char deriving (Eq, Ord)
instance Bounded Alpha where
    minBound = Alpha 'A'
    maxBound = Alpha 'Z'
instance Enum Alpha where
  toEnum n = Alpha $ chr $ n + 65
  fromEnum (Alpha a) = ord a - 65
instance Show Alpha where show (Alpha a) = show a

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

parseTransitionLine :: String -> Maybe [(Natural, Alpha, [Natural])]
parseTransitionLine l | length ws < 3        = Nothing
                      | isNothing q'         = Nothing
                      | not $ all isUpper cs = Nothing
                      | any isNothing qs'    = Nothing
                      | otherwise =
                        Just $ nub [ (fromJust q', Alpha c, map fromJust qs') | c <- cs ]
  where ws = words l
        q:cs:qs = ws
        q' = readMaybe q :: Maybe Natural
        qs' = map (\n -> readMaybe n :: Maybe Natural) qs

mergeTransitions :: [(Natural, Alpha, [Natural])] -> [(Natural, Alpha, [Natural])]
mergeTransitions l = unionMap $ groupBy transitionEq $ sort l
  where
    transitionEq :: (Natural, Alpha, [Natural]) -> (Natural, Alpha, [Natural]) -> Bool
    transitionEq (q1, c1, _) (q2, c2, _) = q1 == q2 && c1 == c2
    unionMap :: [[(Natural, Alpha, [Natural])]] -> [(Natural, Alpha, [Natural])]
    unionMap = map (\trl -> let (ql, cl, qsl) = unzip3 trl
                            in (head ql, head cl, foldl union [] qsl))

parseInput :: String -> Maybe ((Int, [Natural], [Natural], [(Natural, Alpha, [Natural])]), [Alpha])
parseInput input | length strings < 4           = Nothing
                 | isNothing numStates          = Nothing
                 | isNothing startStates        = Nothing
                 | isNothing acceptStates       = Nothing
                 | any isNothing transitionList = Nothing
                 | not $ all isUpper word       = Nothing
                 | otherwise =
                   Just ((fromJust numStates, nub $ fromJust startStates, nub $ fromJust acceptStates,
                   mergeTransitions $ concatMap fromJust transitionList), map Alpha word)
  where isEmptyLine l = null l || all isSpace l
        strings = filter (not . isEmptyLine) $ lines input
        numStatesStr:startStatesStr:acceptStatesStr:restStrList = strings
        numStates = readMaybe numStatesStr :: Maybe Int
        startStates = readMaybe startStatesStr :: Maybe [Natural]
        acceptStates = readMaybe acceptStatesStr :: Maybe [Natural]
        transitionList = map parseTransitionLine $ init restStrList
        word = trim $ last restStrList

generateStateList :: (Int, [Natural], [Natural], [(Natural, Alpha, [Natural])]) -> [Natural]
generateStateList (_, is, ia, tr) =
  nub $ is ++ ia ++ map (\(q, _, _) -> q) tr ++ concatMap (\(_, _, qs) -> qs) tr
