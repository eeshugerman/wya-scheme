import Test.HUnit

import qualified ParserTests
import qualified EvalTests
import qualified PrimitivesTests

main = do runTestTTAndExit $ TestList
            [ ParserTests.tests
            , EvalTests.tests
            , PrimitivesTests.tests
            ]
