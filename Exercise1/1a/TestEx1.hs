module TestEx1 ( tests ) where

import Distribution.TestSuite

tests :: IO [Test]
tests = return [ Test one, Test two ]
	where
		one = TestInstance
			{ run = return $ Finished $ Pass
			, name = "One"
            , tags = []
            , options= []
            , setOption = \_ _ -> Right one
			}
		two = TestInstance
			{ run = return $ Finished $ Fail "faileroonie"
			, name = "Two"
            , tags = []
            , options= []
            , setOption = \_ _ -> Right two
			}
			
