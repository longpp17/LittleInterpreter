module Main where


applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

main :: IO ()
main = do
    x <- putStrLn "Hello, what's your name?"
    print $ applyMaybe (Just 3) (\x -> Just (x+1))