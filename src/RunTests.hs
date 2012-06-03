module RunTests where

import Test.QuickCheck

import PropResolution

main = do
    print "Testing resolution..."
    quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propEquiv 
    print "Testing knowledge base..."
    quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propConsistent 
    quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propProve
    print "Done."


