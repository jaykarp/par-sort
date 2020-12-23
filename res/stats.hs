stats :: String -> IO (Double, Double)
stats fn = do
    s <- readFile fn 
    let l = (read . init) <$> (lines s) :: [Double]
    let n = fromIntegral (length l)
    let m = sum l / n
    return $ (m, sqrt $ (sum $ map (\x -> (x-m)^2) l) / n)