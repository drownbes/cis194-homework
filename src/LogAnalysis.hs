module LogAnalysis where
import Log
import Data.List


safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

safeLast :: [String] -> String
safeLast [] = ""
safeLast xs = last xs

splitOnSpace :: String -> [String]
splitOnSpace text = go text []
  where go (x:xs) res = if x == ' '
                         then go xs (res ++ [""])
                         else go xs (safeInit res ++ [safeLast res ++ [x]])
        go [] res = filter (not . null) res

joinWithSpaces :: [String] -> String
joinWithSpaces = intercalate " "


parseMessage :: String -> LogMessage
parseMessage message = case (words message) of "I":t:m -> LogMessage Info (read t::Int) (unwords m)
                                               "W":t:m -> LogMessage Warning (read t::Int) (unwords m)
                                               "E":e:t:m -> LogMessage (Error (read e::Int)) (read t::Int) (unwords m)
                                               _ -> Unknown message
getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ timestamp _) = timestamp
getTimestamp (Unknown _) = 0

parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert newMsg (Node left msg right)
  | newT <= oldT = Node (LogAnalysis.insert newMsg left) msg right
  | otherwise = Node left msg (LogAnalysis.insert newMsg right)
  where newT = getTimestamp newMsg
        oldT = getTimestamp msg

build :: [LogMessage] -> MessageTree
build = foldr LogAnalysis.insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isErrorMsg :: LogMessage -> Bool
isErrorMsg (LogMessage (Error _) _ _) = True
isErrorMsg _ = False

showLogMessage :: LogMessage -> String
showLogMessage (Unknown msg) = msg
showLogMessage (LogMessage _ _ m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map showLogMessage) . inOrder . build . filter isErrorMsg
