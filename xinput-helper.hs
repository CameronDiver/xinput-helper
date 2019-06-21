import           Data.List
import           Data.Char
import           System.Environment
import           System.Process

main = do
  args <- getArgs
  setMouse args

setMouse :: [String] -> IO ()
setMouse (name : prop : value : []) = do
  line <- findLineFromCommand "xinput" ["list"] name
  let deviceId = extractFromLine line "id=" (not . isSpace)
  propLine <- findLineFromCommand "xinput" ["list-props", deviceId] prop
  let propId = extractFromLine propLine "(" isDigit
  callProcess "xinput" ["set-prop", deviceId, propId, value]
setMouse _ = error "Could not parse arguments"

extractFromLine :: String -> String -> (Char -> Bool) -> String
extractFromLine line needle pred = if take len line == needle
  then takeWhile pred (drop len line)
  else extractFromLine (tail line) needle pred
  where len = length needle

findLineFromCommand :: String -> [String] -> String -> IO String
findLineFromCommand command args needle = do
  output <- readProcess command args []
  let (Just value) = find (isInfixOf needle) $ lines output
  return value
