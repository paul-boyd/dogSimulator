import Data.List
import System.IO
import Data.Char
import Text.Show.Functions

type Happiness = Integer
type Energy = Integer
type Attributes = (Happiness, Energy)



main = do
    mainLoop [(50,50)]

mainLoop :: [Attributes] -> IO ()
mainLoop xs = do
    putStrLn ("Pepper's current Happiness: " ++ show (currentHappiness xs) ++ "% | Energy: " ++ show (currentEnergy xs) ++ "%")
    putStrLn "[S]peak or [I]nteract with Pepper, or [L]eave!"
    c <- getChar
    putChar '\n'
    if toUpper c == 'L' then do
        putStrLn "Thanks for playing!"
        return ()
    else do
        xs' <- pepperReaction xs (toUpper c)
        mainLoop xs'


pepperReaction :: [Attributes] -> Char -> IO [Attributes]
pepperReaction xs 'S' = do
    getLine -- JUST TO DEAL WITH THE MYSTERY EXTRA INPUT I AM GETTING!!
    humanSpeak <- getLine
    return (speakReaction humanSpeak : xs)
pepperReaction xs 'I' = do
    putStrLn "Choose Action!"
    putStrLn "1) Pet"
    putStrLn "2) Give Treat"
    putStrLn "3) Snoot Boop"
    putStrLn "4) Throw Ball"
    getChar -- JUST TO DEAL WITH THE MYSTERY EXTRA INPUT I AM GETTING!!
    c <- getChar
    getChar -- JUST TO DEAL WITH THE MYSTERY EXTRA INPUT I AM GETTING!!
    case c of   '1' -> return ((1,1) : xs)
                '2' -> return((2,2) : xs)
                '3' -> return ((3,3) : xs)
                '4' -> return ((4,4) : xs)
                x -> do
                    putStrLn "Not an option, please choose from the list"
                    return xs
pepperReaction xs _ = do
    putStrLn "Command not recognized - Choose from [S]peak, [I]nteract, or [L]eave"
    return xs




speakReaction :: String -> Attributes
speakReaction xs = sumAttributes (map pepperWords (words (filter (\x -> x/='!' && x/='?') (map toLower xs))))

sumAttributes :: [Attributes] -> Attributes
sumAttributes xs = (sum (map fst xs), sum (map snd xs))

pepperWords :: String -> Attributes
pepperWords "hi" = (5,5)
pepperWords "puppy" = (5,15)
pepperWords "bad" = (-5,-5)
pepperWords xs = (0,0)


pepperAttributes :: [Attributes]
pepperAttributes = [(50,50)]

currentHappiness :: [Attributes] -> Happiness
currentHappiness xs = sum (map fst xs)

currentEnergy :: [Attributes] -> Energy
currentEnergy xs = sum (map snd xs)



-- COULDN'T GET THIS TO WORK IN DO BLOCK - GO BACK, CREATE STANDARD STATUS READOUT, CLEAN UP DO BLOCK
currentAttributes :: [Attributes] -> String
currentAttributes xs = "Pepper Happiness: " ++ show currentHappiness ++ " | Pepper Energy: " ++ show currentEnergy


