{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Instances
import System.Timeout
import Test.Hspec

main :: IO ()
main = hspec $ do
    -- See https://github.com/mgsloan/th-orphans/issues/13
    it "Doesn't infinite loop when comparing types" $ do
        result <- timeout (1000 * 100) $
            compare (AppT (ConT ''Maybe) (ConT ''Int)) (ConT ''Char)
                `shouldBe` LT
        result `shouldBe` Just ()
    -- See https://github.com/mgsloan/th-orphans/issues/14
    it "Compares types correctly" $
        compare (AppT (ConT ''Maybe) (ConT ''Int)) (AppT (ConT ''Maybe) (ConT ''Char))
            `shouldBe` GT
