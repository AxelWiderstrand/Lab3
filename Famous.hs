data QA = Question String QA QA | Action String 
    deriving (Show, Read)

--read :: String -> QA
--read s = 


isfrEurope = Question "Is she from Europe?" isScientist isActress
isActress = Question "Is she an actress" marilyn hillary
isScientist = Question "Is she a scientist" marie queen 
marilyn = Question "Is it Marilyn Monroe?" yes no
hillary = Question "Is it Hillary Clinton?" yes no
marie = Question "Is it Marie Curie?" yes no
queen = Question "Is it Queen Elisabeth 2" yes no
yes = Action "Yes!" 
no = NewQ "Who was it?" q1
q1 = NewQ "Give me a question for the famous person" q2
q2 = 

s <- getline 


play :: QA -> IO ()
play (Question q ifYes ifNo) = do putStrLn q
                                  answer <- getLine
                                  case answer of
                                    'y':_ -> play ifYes
                                    'n':_ -> play ifNo
                                    _     -> play (Question q ifYes ifNo)
play (Action a)            = do 
    foo <- yesNoQuestion ....
    if foo
    then return (Action a)
    else do
        q <- ......
        ans <- .......
        return (Question q (Action ans) (Action a))
  
                                  --play d
--play Done                    = return ()


tryIOError :: IO a -> IO (Either IOError a)
tryIOError i = undefined


question :: String -> IO String
question s = undefined
    
    
{-    
    do putStrLn "q"
       q <- readFile "Questions.hs"
       s <- getLine
       return s
-}

yesNoQuestion :: String -> IO Bool
yesNoQuestion s = undefined



--main :: IO ()
