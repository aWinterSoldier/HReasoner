module RunTests where

import Test.QuickCheck

import PropResolution

main = do
    putStrLn "Testing resolution..."
    quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propEquiv 
    putStrLn "Testing knowledge base..."
    quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propConsistent 
    quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propProve
    putStrLn "Done."


