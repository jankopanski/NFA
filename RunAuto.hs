import Auto ()
import System.IO
import System.Environment
-- import Text.Read
import Numeric.Natural

main :: IO ()
main = do
  [filePath] <- getArgs
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  let x = parseInput contents
  print x
  hClose handle


parseInput :: String -> (([Natural], [Natural], [Natural], [(Natural, Char, [Natural])]), String)
parseInput input = ((st', is', ia', tl), w)
  where
    st:is:ia:xs = filter (not . null) $ lines input
    st' = read st :: [Natural]
    is' = read is :: [Natural]
    ia' = read ia :: [Natural]
    w = last xs
    tl = map (\l -> read l :: (Natural, Char, [Natural])) (init xs)
