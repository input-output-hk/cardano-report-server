{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module MainSpec (spec) where

import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as Aeson
import           Data.Text.Arbitrary     ()
import           Data.Time               (UTCTime (..))
import           Data.Time.Calendar      (fromGregorian)
import           Data.Typeable           (typeRep)
import           Data.Version            (Version (..))
import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Arbitrary (arbitrary), Gen, choose,
                                          counterexample, elements, getPositive, listOf1,
                                          oneof, scale, (===))
import           Universum

import           Pos.ReportServer.Report (ReportInfo (..), ReportType (..), supportedApps)



spec :: Spec
spec =
    describe "Network types" $
    describe "Json instances" $ do
        jsonIdentityTest @ReportType
        jsonIdentityTest @ReportInfo

----------------------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------------------

instance Arbitrary ReportType where
    arbitrary = do
        let crashA = RCrash <$> choose (0,1000)
        let errorA = RError <$> arbitrary
        let misbehA = RMisbehavior <$> arbitrary <*> arbitrary
        let infoA = RInfo <$> arbitrary
        oneof [crashA, errorA, misbehA, infoA]

instance Arbitrary ReportInfo where
    arbitrary = do
        rApplication <- elements supportedApps
        rVersion <-
            Version <$> listOf1 (getPositive <$> arbitrary)
                    <*> (pure empty)
        rBuild <- show <$> choose (0,1000::Int)
        rOS <- scale (min 100) arbitrary
        rDate <- arbitrary
        rMagic <- arbitrary
        rReportType <- arbitrary
        pure $ ReportInfo{..}

instance Arbitrary UTCTime where
    arbitrary = do
        randomDay <- choose (1, 27) :: Gen Int
        randomMonth <- choose (1, 12) :: Gen Int
        randomYear <- choose (2015,2016) :: Gen Integer
        randomTime <- choose (0, 50000) :: Gen Int
        let day = fromGregorian randomYear randomMonth randomDay
        let time = fromIntegral randomTime
        pure $ UTCTime day time

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

type IdTestContext a = (Arbitrary a, Typeable a, Show a, Eq a)

typeName :: forall a. Typeable a => String
typeName = show $ typeRep (Proxy @a)

jsonIdentityTest :: forall a. (IdTestContext a, FromJSON a, ToJSON a) => Spec
jsonIdentityTest = prop (typeName @a) $ \(x :: a) -> do
    let y = Aeson.eitherDecode $ Aeson.encode x
    case y of
      Right s    -> s === x
      f@(Left t) -> counterexample t $ f === Right x
