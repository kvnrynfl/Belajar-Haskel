import System.IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

-- Function to read and process data from a file
processDataFile :: FilePath -> IO ()
processDataFile filePath = do
    fileContents <- readFile filePath
    let inputTuples = read fileContents :: [(Int, Int, Int, Int)]
    
    let outputDir = "output"  -- Output folder name
        resultFilePaths = ["filtered_data_phase1.txt", "filtered_data_phase2.txt", "filtered_data_phase3.txt", "filtered_data_phase4.txt"]
    
    -- Phase 1: Remove tuples with elements containing 0
    let phase1Result = filterTuple1 inputTuples
    writeResultToFileWrapper (outputDir </> head resultFilePaths) (Right phase1Result)
    
    -- Phase 2: Combine c and d into e
    let phase2Result = filterTuple2 phase1Result
    writeResultToFileWrapper (outputDir </> resultFilePaths !! 1) (Left phase2Result)
    
    -- Phase 3: Remove tuples with e > 120
    let phase3Result = filterTuple3 phase2Result
    writeResultToFileWrapper (outputDir </> resultFilePaths !! 2) (Left phase3Result)
    
    -- Phase 4: Remove tuples with odd sum of a + b
    let phase4Result = filterTuple4 phase3Result
    writeResultToFileWrapper (outputDir </> resultFilePaths !! 3) (Left phase4Result)
    
    putStrLn "Process completed. Results saved in the 'output' folder."

-- Function to write results to a file with the appropriate path
writeResultToFile :: FilePath -> [(Int, Int, Int)] -> IO ()
writeResultToFile filePath tuples = do
    createDirectoryIfMissing True (takeDirectory filePath)  -- Create directory if it doesn't exist
    withFile filePath WriteMode $ \handle ->
        mapM_ (\(a, b, c) -> hPutStrLn handle $ show a ++ "," ++ show b ++ "," ++ show c) tuples

-- Overloaded version for tuples with four elements
writeResultToFile' :: FilePath -> [(Int, Int, Int, Int)] -> IO ()
writeResultToFile' filePath tuples = do
    createDirectoryIfMissing True (takeDirectory filePath)  -- Create directory if it doesn't exist
    withFile filePath WriteMode $ \handle ->
        mapM_ (\(a, b, c, d) -> hPutStrLn handle $ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d) tuples

-- Wrapper function to choose the function based on the tuple type
writeResultToFileWrapper :: FilePath -> Either [(Int, Int, Int)] [(Int, Int, Int, Int)] -> IO ()
writeResultToFileWrapper filePath (Left tuples) = writeResultToFile filePath tuples
writeResultToFileWrapper filePath (Right tuples) = writeResultToFile' filePath tuples

-- Function for phase 1: Remove tuples with elements containing 0
filterTuple1 :: [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
filterTuple1 = filter (\(a, b, c, d) -> a /= 0 && b /= 0 && c /= 0 && d /= 0)

-- Function for phase 2: Combine c and d into e
filterTuple2 :: [(Int, Int, Int, Int)] -> [(Int, Int, Int)]
filterTuple2 = map (\(a, b, c, d) -> (a, b, c + d))

-- Function for phase 3: Remove tuples with e > 120
filterTuple3 :: [(Int, Int, Int)] -> [(Int, Int, Int)]
filterTuple3 = filter (\(_, _, e) -> e <= 120)

-- Function for phase 4: Remove tuples with odd sum of a + b
filterTuple4 :: [(Int, Int, Int)] -> [(Int, Int, Int)]
filterTuple4 = filter (\(a, b, e) -> even (a + b))

main :: IO ()
main = do
    putStrLn "Processing data..."
    let inputFilePath = "data.txt"  -- Replace with the appropriate path
    processDataFile inputFilePath
