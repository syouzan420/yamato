{-# LANGUAGE BangPatterns #-}

import System.Console.ANSI (clearScreen)
import System.IO (IOMode(..), openFile, hClose, hGetContents,
                hSetEncoding, utf8, hSetBuffering, stdin, BufferMode(..))


data State = State { player :: Player
                 , girl :: Girl
                 , chara :: [Chara]
                 , enemy :: [Enemy]
                 , mode :: Mode
                 , eList :: [Int]
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
    let state = State {player = p, girl = g, chara = [], enemy = [],
                       mode = Normal, eList = [], event = 0, file = 0} 
    putStrLn ( (pl_name p)++" と "++(gl_name g)++" は 仲良しです" )
    return state

ending :: State -> IO ()
ending state = do
    print state 

routine :: State -> IO State 
routine state = do
    clearScreen
    el <- readEvent (files (file state)) (event state)
    state' <- exeEvent state Normal "" el
    let newState = state' {mode = Normal, eList = []}
    if el==[]
      then return newState 
      else routine newState

readLine :: State -> Mode -> String -> String -> ( Mode, String, String )
readLine state m c [] = ( m, [], [] )
readLine state m c (x:xs)
    | (m==Normal) && (x=='#') && (xs/=[]) = 
                          case (head xs) of 'n' -> (readLine state Name "" (tail xs))
                                            'j' -> (readLine state Jump "" (tail xs))
                                            'c' -> (readLine state Choice "" [])
                                            _   -> atrd [x] (readLine state m c xs)
    | m==Name && x=='#' = cfst Normal ( atrd (showName state (read c :: Int)) (readLine state Normal c xs) )
    | m==Jump && x=='#' = readLine state Jump c xs
    | m==Choice && x=='#' = cfst Normal (readLine state Normal c xs)
    | m/=Normal = asnd [x] (readLine state m (c++[x]) xs)
    | otherwise = atrd [x] (readLine state m c xs)

exeEvent :: State -> Mode -> String -> [String] -> IO State 
exeEvent state m c [] 
    | (mode state)==Jump = return state
    | (mode state)==Choice = return state
    | otherwise = return nextEvent
    where nextEvent = state {event=(event state)+1}
exeEvent state m c (x:xs)
    | m==Choice = do
          let tp = readLine state m c x
          let cState = if((gfst tp)==Normal)
                          then state 
                          else state {eList=(eList state)++[(read (last $ words $ gsnd tp))::Int]}
          if((gfst tp)==Normal)
              then do 
                  n <- makeChoice (eList cState) 
                  let newState = cState {mode = Choice, event = n}
                  exeEvent newState (gfst tp) (gsnd tp) xs
              else do
                  putStrLn ((show $ length (eList cState))++": "++(head $ words $ gsnd tp))
                  exeEvent cState (gfst tp) (gsnd tp) xs
    | otherwise = do
          let tp = readLine state m c x
          if((gfst tp)==Normal)
            then do 
                putStrLn $ gtrd tp
                getLine
                exeEvent state (gfst tp) (gsnd tp) xs
            else if((gfst tp)==Jump)
                    then do
                      let je = read (gsnd tp) :: Int
                      let jState = state {mode = Jump, event = je}
                      exeEvent jState (gfst tp) (gsnd tp) xs
                    else do
                      exeEvent state (gfst tp) (gsnd tp) xs

makeChoice :: [Int] -> IO Int 
makeChoice li = do
    s <- getLine
    if (s>="1" && s<=(show $ length li))
        then return (li !! ((read s :: Int)-1))
        else do
            putStrLn "先頭の番号を入力してください"
            makeChoice li

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

main :: IO ()
main = do
    state <- initiate
    finalState <- routine state
    ending finalState
               

