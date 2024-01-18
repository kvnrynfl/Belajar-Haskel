import Text.Read (readMaybe)

-- Function to print information about a number
printNumberInfo :: Int -> IO ()
printNumberInfo x = do
    putStrLn $ "Answer: " ++ show x ++ " [" ++ numberType ++ "] [" ++ positivity ++ "]"
  where
    numberType = if odd x then "Ganjil" else "Genap"
    positivity = if x > 0 then "Positive" else "Negative or zero"

-- Main function (entry point) of the program
main :: IO ()
main = do
    putStrLn "Enter a Number: "
    input <- getLine

    -- Using readMaybe for safe input reading
    case readMaybe input of
        Just number -> printNumberInfo number
        Nothing -> putStrLn "Invalid input. Please enter an integer."
