guessthenumber :: Int -> IO ()
guessthenumber number = do
    putStrLn "What number am I thinking?"
    putStr "Enter a number: "
    input <- getLine
    let guess = read input :: Int
    let (response, continue) = check guess number
    putStrLn response
    if continue then
        (guessthenumber number)
    else
        putStrLn "End"



check :: Int -> Int -> (String, Bool)
check guess number
    | number == guess = ("You Win!", False)
    | guess < number  = ("It's bigger!", True)
    | otherwise       = ("It's smaller!", True)
