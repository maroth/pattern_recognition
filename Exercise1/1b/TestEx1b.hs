module TestEx1 ( tests ) where

import Distribution.TestSuite

tests :: IO [Test]
tests = return [ Test one, Test two ]
	where
		one = TestInstance
			{ run = 
                let f = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
                let center = clusterCenter f
                let correctResult = [4, 5, 6]
                let correct = center == correctResult

                return $ Finished $ Pass
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
			
