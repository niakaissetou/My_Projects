{--
-- EPITECH PROJECT, 2024
-- wolfram_test
-- File description:
-- funct_prog
--}
import System.Environment (getArgs)
import Data.Maybe ()
import System.Exit (ExitCode(..))
import System.Exit (exitWith, ExitCode(ExitFailure))


data Conf = Conf {
    option1::Maybe String,
    option2::Maybe Int,
    option3::Maybe Int,
    option4::Maybe Int,
    option5::Maybe Int
} deriving (Show)

defaultConf:: Conf
defaultConf = Conf {
    option1 = Nothing,
    option2 = Just (0),
    option3 = Just (100000),
    option4 = Just (80),
    option5 = Just (20)
}

parseOption1::[String] -> Maybe String
parseOption1 [] = Nothing
parseOption1 (arg:_) = Just arg

parseOption2::[String] -> Maybe Int
parseOption2 [] = Nothing
parseOption2 (arg:_) = Just (read arg :: Int)

parseOption3::[String] -> Maybe Int
parseOption3 [] = Nothing
parseOption3 (arg:_) = Just (read arg :: Int)

parseOption4::[String] -> Maybe Int
parseOption4 [] = Nothing
parseOption4 (arg:_) = Just (read arg :: Int)

parseOption5::[String] -> Maybe Int
parseOption5 [] = Nothing
parseOption5 (arg:_) = Just (read arg :: Int)

getOpts::Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf (opt:args) =
    case opt of
        "--rule" -> getOpts (conf { option1 = parseOption1 args}) (drop 1 args)
        "--start" -> getOpts (conf {option2 = parseOption3 args}) (drop 1 args)
        "--lines" -> getOpts (conf {option3 = parseOption2 args}) (drop 1 args)
        "--window" -> getOpts (conf{option4 = parseOption4 args}) (drop 1 args)
        "--move" -> getOpts (conf { option5 = parseOption5 args}) (drop 1 args)
        _ -> Nothing

rule30::Char -> Char -> Char -> Char
rule30 ' ' ' ' ' ' = ' '
rule30 ' ' ' ' '*' = '*'
rule30 ' ' '*' ' ' = '*'
rule30 ' ' '*' '*' = '*'
rule30 '*' ' ' ' ' = '*'
rule30 '*' ' ' '*' = ' '
rule30 '*' '*' ' ' = ' '
rule30 '*' '*' '*' = ' '
rule30 _ _ _ = ' '

rule90::Char -> Char -> Char -> Char
rule90 ' ' ' ' ' ' = ' '
rule90 ' ' ' ' '*' = '*'
rule90 ' ' '*' ' ' = ' '
rule90 ' ' '*' '*' = '*'
rule90 '*' ' ' ' ' = '*'
rule90 '*' ' ' '*' = ' '
rule90 '*' '*' ' ' = '*'
rule90 '*' '*' '*' = ' '
rule90 _ _ _ = ' '

rule110::Char -> Char -> Char -> Char
rule110 ' ' ' ' ' ' = ' '
rule110 ' ' ' ' '*' = '*'
rule110 ' ' '*' ' ' = '*'
rule110 ' ' '*' '*' = '*'
rule110 '*' ' ' ' ' = ' '
rule110 '*' ' ' '*' = '*'
rule110 '*' '*' ' ' = '*'
rule110 '*' '*' '*' = ' '
rule110 _ _ _ = ' '

genNextRow2:: String -> String -> String
genNextRow2 "30" (x:y:z:xs) = rule30 x y z : genNextRow2 "30" (y:z:xs)
genNextRow2 "90" (x:y:z:xs) = rule90 x y z : genNextRow2 "90" (y:z:xs)
genNextRow2 "110" (x:y:z:xs) = rule110 x y z : genNextRow2 "110" (y:z:xs)
genNextRow2 _ _ = []

genNextRow:: String -> String -> String
genNextRow rule row = genNextRow2 rule (' ':row ++ " ")

genAll:: String -> String -> Int -> Int -> [String]
genAll rule f_row l s
    | l <= 0 = []
    | s > 0 = genAll rule (genNextRow rule f_row) (l - 1) (s - 1)
    | otherwise = f_row : genAll rule (genNextRow rule f_row) (l - 1) (s - 1)

formatState:: String -> Int -> Int -> String
formatState state move window = replicate ((window - length state) `div` 2) ' ' ++ state ++ replicate ((window - length state) `div` 2) ' '

generateAutomaton::Maybe String -> Maybe Int -> Maybe Int  -> Maybe Int -> Maybe Int -> IO ()
generateAutomaton r l m w s =
    let Just lines = l
        Just rule = r
        Just move = m
        Just wind = w
        space = (wind * 100) `div` 2
        Just start = s
        f_line = replicate ((space)) ' ' ++ "*" ++ replicate ((space) - 1) ' '
        sequence = genAll rule f_line lines start 
        x seq = putStrLn $ formatState seq move wind
    in mapM_ putStrLn $ map (take wind . drop((space - wind `div` 2))) sequence

processArgs::Maybe String -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
processArgs r s l w m =
    case r of
        Just "30" -> generateAutomaton r l m w s
        Just "90" -> generateAutomaton r l m w s
        Just "110" -> generateAutomaton r l m w s
        _ -> putStrLn "--rule 'value' should be : 30, 90 or 110."
             >> exitWith (ExitFailure 84)

main::IO ()
main = do
    args <- getArgs
    case getOpts defaultConf args of
        Just (Conf Nothing _ _ _ _) ->
            putStrLn "Please use: '--rule 30, 90, or 110'."
            >> exitWith (ExitFailure 84)
        Just (Conf r s l w m) -> processArgs r s l w m
        Nothing -> putStrLn "Use: --rule, --lines, --start, --window, or move."
                    >> exitWith (ExitFailure 84)
