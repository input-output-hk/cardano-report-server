{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module MainSpec (spec) where

import           Universum

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import           Data.Text.Arbitrary ()
import           Data.Time (UTCTime (..))
import           Data.Time.Calendar (fromGregorian)
import           Data.Typeable (typeRep)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, counterexample, elements,
                                  getPositive, oneof, scale, (===))

import           Pos.ReportServer.Report (BackendVersion (..), FrontendVersion (..),
                                          IOHKProduct (..), InstallerVersion (..), Network (..),
                                          ReportInfo (..), V1ReportInfo (..), ReportType (..),
                                          Version (..), supportedApps)

{-

import Test.QuickCheck
import Pos.ReportServer.Report
import Pos.ReportServer.Util

reportInfo <- generate (arbitrary :: Gen V1ReportInfo)
generateProductVersion reportInfo

-}

spec :: Spec
spec =
    describe "Network types" $
    describe "Json instances" $ modifyMaxSuccess (const 1000) $ do
        jsonIdentityTest @ReportType
        jsonIdentityTest @ReportInfo
        jsonIdentityTest @V1ReportInfo

----------------------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------------------

instance Arbitrary ReportType where
    arbitrary = do
        let crashA  = RCrash        <$> choose (0,1000)
        let errorA  = RError        <$> arbitrary
        let misbehA = RMisbehavior  <$> arbitrary <*> arbitrary
        let infoA   = RInfo         <$> arbitrary
        oneof [crashA, errorA, misbehA, infoA]

-- | A simple version generator with no tags.
validVersion :: Gen Version
validVersion = do
    major <- arbitraryPositive
    minor <- arbitraryPositive

    let version = show major <> "." <> show minor

    pure $ Version version
  where
    arbitraryPositive :: Gen Int
    arbitraryPositive = getPositive <$> arbitrary

instance Arbitrary FrontendVersion where
    arbitrary = FrontendVersion <$> validVersion

instance Arbitrary BackendVersion where
    arbitrary = BackendVersion <$> validVersion

instance Arbitrary InstallerVersion where
    arbitrary = InstallerVersion <$> validVersion

instance Arbitrary Network where
    arbitrary = oneof
        [ pure Mainnet
        , pure Testnet
        ]

instance Arbitrary V1ReportInfo where
    arbitrary = do
        riProduct           <- IOHKProduct <$> elements supportedApps
        riFrontendVersion   <- arbitrary
        riBackendVersion    <- arbitrary
        riNetwork           <- arbitrary
        riBuild             <- show <$> choose (0,1000::Int)
        riInstallerVersion  <- arbitrary
        riOS                <- scale (min 100) arbitrary
        riDate              <- arbitrary
        riMagic             <- arbitrary
        riReportType        <- arbitrary

        pure $ V1ReportInfo{..}

instance Arbitrary UTCTime where
    arbitrary = do
        randomDay   <- choose (1, 27) :: Gen Int
        randomMonth <- choose (1, 12) :: Gen Int
        randomYear  <- choose (2010,2018) :: Gen Integer
        randomTime  <- choose (0, 50000) :: Gen Int
        let day = fromGregorian randomYear randomMonth randomDay
        let time = fromIntegral randomTime
        pure $ UTCTime day time


instance Arbitrary ReportInfo where
    arbitrary = do
        rApplication        <- elements supportedApps
        rVersion            <- arbitrary
        rBuild              <- show <$> choose (0,1000::Int)
        rOS                 <- scale (min 100) arbitrary
        rDate               <- arbitrary
        rMagic              <- arbitrary
        rReportType         <- arbitrary

        pure $ ReportInfo{..}

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

