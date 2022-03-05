
{-# LANGUAGE TypeApplications #-}


-- calculate superdigit given string of digits (n) and k number of times to repeat n
superdigit :: String -> Int -> String
superdigit n k = (calcSuperDigit . show) $ read @Int (calcOnceSuperDigit n) * k


-- takes a string of digits and sums them together
calcOnceSuperDigit :: String -> String
calcOnceSuperDigit = show . sum . map ((read :: String -> Int) . (: []))

-- recursive calc super digit
calcSuperDigit :: String -> String
calcSuperDigit [a] = [a]
calcSuperDigit xs  = calcSuperDigit $ calcOnceSuperDigit xs

main :: IO ()
main = do
    input <- getLine 
    let inputWords = words input
        n = head inputWords
        k = read (inputWords !! 1) :: Int
    putStrLn (superdigit n k)



