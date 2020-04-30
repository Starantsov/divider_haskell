main :: IO ()

divide :: Integer -> Integer -> (Integer, Integer)
divide x y = do
    let sign = if x * y > 0 then 1 else -1
    if y /= 0 
        then do
            let (result, remains) = makeDivision (abs x) (abs y) 0
            (result * sign, remains)
        else (0, 0)
    where makeDivision x y c = if x >= y then makeDivision (x - y) y (c + 1) else (c, x)

main = do 
    let (result, remains) = divide (-45) 20
    print ("Result is " ++ show result ++ ", Remainder " ++ show remains)