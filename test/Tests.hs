import Test.HUnit

import qualified ParserTests
import qualified EvalTests

main = do runTestTTAndExit $ TestList
            [ ParserTests.tests
            , EvalTests.tests
            ]
