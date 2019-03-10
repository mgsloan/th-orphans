{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Lift
import System.Timeout
import Test.Hspec
import TestUtil
import qualified Data.ByteString as BS

main :: IO ()
main = hspec $ do
    -- See https://github.com/mgsloan/th-orphans/issues/13
    it "Doesn't infinite loop when comparing types" $ do
        -- This can either yield LT or GT because different TH
        -- versions define the constructors in different orders.
        result <- timeout (1000 * 100) $
            compare (AppT (ConT ''Maybe) (ConT ''Int)) (ConT ''Char)
                `shouldSatisfy` (/= EQ)
        result `shouldBe` Just ()
    -- See https://github.com/mgsloan/th-orphans/issues/14
    it "Compares types correctly" $
        compare (AppT (ConT ''Maybe) (ConT ''Int)) (AppT (ConT ''Maybe) (ConT ''Char))
            `shouldBe` GT
    it "Lifts bytes" $ do
        let addr = $(pure $(lift (LitE (BytesPrimL (bsToBytes testBytes)))))
        bs <- addrToBs addr (BS.length testBytes)
        bs `shouldBe` testBytes
