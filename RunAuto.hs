import Auto
import System.IO
import System.Environment
import Text.Read
import Numeric.Natural

type MN = Maybe [Natural]
type MS = Maybe (Natural, String, [Natural])
type MT = Maybe (Natural, Char, [Natural])

main :: IO ()
main = do
  [filePath] <- getArgs
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let errMsg = "BAD INPUT"
      ((st, is, ia, tl), word)= parseInput contents
      -- aut = fromLists st is ia tl
  -- print aut
  hClose handle

-- Wywalić maybe bez propagacji
parseInput :: String -> ((Maybe Natural, MN, MN, [MT]), Maybe String)
parseInput input = ((numStates, startStates, acceptStates, transitionList), word)
  where
    numStatesStr:startStatesStr:acceptStatesStr:rest = filter (not . null) $ lines input
    numStates = readMaybe numStatesStr :: Maybe Natural
    startStates = readMaybe startStatesStr :: MN
    acceptStates = readMaybe acceptStatesStr :: MN
    word = if null rest then Nothing else Just $ last rest
    transitionList = if null rest then [Nothing] else
      let transitionCompressedList = map (\l -> readMaybe l :: MS) (init rest)
          uncompress (q, cs, qs) = [ Just (q, c, qs) | c <- cs ]
      in concatMap (maybe [Nothing] uncompress) transitionCompressedList

-- createAuto :: ((Maybe Natural, MN, MN, [MT]), Maybe String) -> Maybe (Auto Char Natural)
-- createAuto ((numStates, startStates, acceptStates, transitionList), word) = do
--
-- startStates ++ acceptStates ++ map (\(q, c, qs)) transitionList
