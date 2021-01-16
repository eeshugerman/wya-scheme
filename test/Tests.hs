import Test.HUnit

import qualified ParserTests
import qualified EvalTests
import qualified PrimativesTests

main = do runTestTTAndExit $ TestList
            [ ParserTests.tests
            , EvalTests.tests
            , PrimativesTests.tests
            ]
