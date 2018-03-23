{-# LANGUAGE BangPatterns #-}

import System.Console.ANSI (clearScreen)
import System.IO (IOMode(..), openFile, hClose, hGetContents,
                hSetEncoding, utf8, hSetBuffering, stdin, BufferMode(..))


data State = State { player :: Player
                 , girl :: Girl
                 , chara :: [Chara]
                 , enemy :: [Enemy]
                 , event :: Int
                 , file :: Int
                 } deriving (Show)

data Player = Player { pl_name :: String
                     , pl_life :: Int} deriving (Show)

data Girl = Girl { gl_name :: String
                 , friendship :: Int } deriving (Show)

data Chara = Chara { ch_name :: String
                   , ch_event :: Int } deriving (Show)
                   
data Enemy = Enemy { en_name :: String
                   , en_life :: Int } deriving (Show)

data Mode = Normal | Name | Choice | Jump deriving(Eq,Show)

files :: Int -> FilePath
files 0 = "main_events.txt"

initiate :: IO State 
initiate = do
    putStr "主人公の名前(空白なら てる とします): "
    name_p <- getLine
    putStr "女の子の名前(空白なら のこ とします): "
    name_g <- getLine
    let p = Player { pl_name = (if name_p=="" then "てる" else name_p)
                   , pl_life = 10}
    let g = Girl { gl_name = (if name_g=="" then "のこ" else name_g)
                 , friendship = 1}
    let state = State {player = p, girl = g, chara = [], enemy = [], event = 0, file = 0} 
    putStrLn ( (pl_name p)++" と "++(gl_name g)++" は 仲良しです" )
    return state

ending :: State -> IO ()
ending state = do
    print state 

routine :: State -> IO State 
routine state = do
    clearScreen
    el <- readEvent (files (file state)) (event state)
    exeEvent state Normal "" el
    let p = (player state) { pl_life = (pl_life $ player state)-1 }
    let newState = state {player = p, event=(event state)+1}
    if el==[]
      then return newState 
      else routine newState

readLine :: State -> Mode -> String -> String -> ( Mode, String, String )
readLine state m c [] = ( m, [], [] )
readLine state m c (x:xs)
    | (m==Normal) && (x=='#') && (xs/=[]) = 
                          case (head xs) of 'n' -> (readLine state Name "" (tail xs))
                                            _   -> atrd [x] (readLine state m c xs)
    | m==Name && x=='#' = cfst Normal ( atrd (showName state (read c :: Int)) (readLine state Normal c xs) )
    | m/=Normal = asnd [x] (readLine state m (c++[x]) xs)
    | otherwise = atrd [x] (readLine state m c xs)

exeEvent :: State -> Mode -> String -> [String] -> IO ()
exeEvent state m c [] = return ()
exeEvent state m c (x:xs)
    | m==Choice = do
          return ()
    | otherwise = do
          let tp = readLine state m c x
          print tp
          putStrLn $ gtrd tp
          getLine
          exeEvent state (gfst tp) (gsnd tp) xs

gfst :: (a, b, c) -> a
gfst (a, _, _) = a

gsnd :: (a, b, c) -> b
gsnd (_, b, _) = b

gtrd :: (a, b, c) -> c
gtrd (_, _, c) = c

cfst :: Mode -> (Mode, b, c) -> (Mode, b, c)
cfst x (_, b, c) = (x, b, c)

asnd :: String -> (a, String, c) -> (a, String, c)
asnd x (a, b, c) = (a, x++b, c)

atrd :: String -> (a, b, String) -> (a, b, String)
atrd x (a, b, c) = (a, b, x++c)

readEvent :: FilePath -> Int -> IO [String] 
readEvent fn e = do
    h <- openFile fn ReadMode 
    hSetEncoding h utf8
    c <- hGetContents h
    let !result = getEventLines e False $ lines c
    hClose h
    return result 

getEventLines :: Int -> Bool -> [String] -> [String]
getEventLines _ _ [] = []
getEventLines e b (x:xs)
  | x==(show (e+1)) || x=="#end#" = []
  | x==(show e) = getEventLines e True xs
  | b==True = [x]++(getEventLines e True xs)
  | otherwise = getEventLines e False xs

showName :: State -> Int -> String
showName state 0 = pl_name $ player state
showName state 1 = gl_name $ girl state
showName state 12 = "ごりら"

main :: IO ()
main = do
    state <- initiate
    finalState <- routine state
    ending finalState
               

