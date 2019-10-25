import System.IO.Error
import System.IO

--Data type for a question-answer tree
data QA = Question String QA QA | Action String 
    deriving (Show, Read)


--This is a default tree    
defaultQA   = isfrEurope
isfrEurope  = Question "Is she from Europe?" isScientist isActress
isActress   = Question "Is she an actress" marilyn hillary
isScientist = Question "Is she a scientist" marie queen 
marilyn     = Action "Marilyn Monroe"
hillary     = Action "Hillary Clinton"
marie       = Action "Marie Curie"
queen       = Action "Queen Elisabeth 2"


--Plays a QA tree
play :: QA -> IO QA
play (Question q ifYes ifNo) = do putStrLn q
                                  answer <- getLine
                                  case answer of
                                    'y':_ -> do newYes <- play ifYes
                                                return (Question q newYes ifNo)
                                    'n':_ -> do newNo <- play ifNo
                                                return (Question q ifYes newNo)
                                    _     -> play (Question q ifYes ifNo)
play (Action a)            = do 
    foo <- yesNoQuestion  ("My guess: Is it " ++ a ++ "?")
    if foo
    then do putStrLn "Hurray I won!"
            return (Action a)
    else do
        putStrLn "OK - you won this time"
        ans <- question "Just curious: Who was your famous person?"
        q2  <- question ("Give me a question for which the answer for " 
                          ++ ans ++ " is yes and the answer for " ++ a ++ " is no")
        return (Question q2 (Action ans) (Action a))


--Function used when asking questions
question :: String -> IO String
question s = do putStrLn s
                answer <- getLine
                return answer
    

--Function used when asking yes/no questions
yesNoQuestion :: String -> IO Bool
yesNoQuestion s = do
                      answer <- question s
                      case answer of
                       'y':_ -> return True
                       'n':_ -> return False
                       _     -> yesNoQuestion s
                        

--Used for running the game
main :: IO ()
main = do
         s <- tryIOError (readFile "Questions")
         let tree = case s of
                         Left e   -> defaultQA
                         Right [] -> defaultQA
                         Right c  -> read c
         newTree  <- play tree
         writeFile "Questions" (show newTree)
         again    <- yesNoQuestion "Do you want to play again?"   
         if again
         then do putStrLn "Great!"
                 main
         else do putStrLn "Bye!"
