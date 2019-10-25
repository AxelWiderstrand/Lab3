import System.IO.Error
import System.IO
import Test.QuickCheck

data QA = Question String QA QA | Action String 
    deriving (Show, Read)


isfrEurope = Question "Is she from Europe?" isScientist isActress
isActress = Question "Is she an actress" marilyn hillary
isScientist = Question "Is she a scientist" marie queen 
marilyn = Question "Is it Marilyn Monroe?" yes no
hillary = Question "Is it Hillary Clinton?" yes no
marie = Question "Is it Marie Curie?" yes no
queen = Question "Is it Queen Elisabeth 2" yes no
yes = Action "Yes!" 
no = Action "No" 


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
    foo <- yesNoQuestion  ("Is it " ++ a ++ "?")
    if foo
    then do putStrLn "I won!"
            return (Action a)
    else do
        ans <- question "Just curious: Who was your famous person?"
        q2 <- question ("Give me a question for which the answer for " ++ ans ++ " is yes and the answer for " ++ a ++ "is no")
        return (Question q2 (Action ans) (Action a))

question :: String -> IO String
question s = do putStrLn s
                answer <- getLine
                return answer
    

yesNoQuestion :: String -> IO Bool
yesNoQuestion s = do
                      answer <- question s
                      case answer of
                       'y':_ -> return True
                       'n':_ -> return False
                       _     -> yesNoQuestion s
                        

main :: IO ()
main = undefined
