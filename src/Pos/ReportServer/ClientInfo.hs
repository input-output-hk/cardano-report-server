{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Information about client we want to store.

module Pos.ReportServer.ClientInfo
       ( ClientInfo (..)
       , getClientInfo
       ) where

import           Data.Aeson                (ToJSON (..), Value (Array, String), object,
                                            (.=))
import           Data.CaseInsensitive      (original)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Vector               as V
import           Formatting                (sformat)
import           Formatting.Formatters     (hex)
import           Network.HTTP.Types.Header (Header)
import           Network.Socket            (hostAddressToTuple)
import           Network.Socket.Internal   (SockAddr (..))
import           Network.Wai               (isSecure, remoteHost, requestHeaders)
import           Universum                 hiding (decodeUtf8)
import           Web.Scotty.Internal.Types (ActionT (..), ScottyError, getReq)

newtype SockAddrW = SockAddrW SockAddr
newtype HeaderW = HeaderW Header

-- | Data identifying client-server connection.
data ClientInfo = ClientInfo
    { ciSockAddr :: SockAddrW
    , ciSSL      :: Bool
    , ciHeaders  :: [HeaderW]
    }

instance ToJSON HeaderW where
    toJSON (HeaderW (hname, bs)) =
        Array $ V.fromList [String (decodeUtf8 $ original hname), String (decodeUtf8 bs)]

-- Returns 4 word32
flattenWord32 :: Word32 -> [Word8]
flattenWord32 w = let (a,b,c,d) = hostAddressToTuple w in [a,b,c,d]

instance ToJSON SockAddrW where
    toJSON (SockAddrW (SockAddrInet port host)) =
        let addr = flattenWord32 host
        in String $ T.intercalate "." (map show addr) <> ":" <>
                    show (fromIntegral port :: Integer)
    toJSON (SockAddrW (SockAddrInet6 port _ (h1,h2,h3,h4) _)) =
        let addr = concat $ map flattenWord32 [h1,h2,h3,h4]
        in String $ T.intercalate ":" (map (sformat hex) addr) <> "/" <>
                    show (fromIntegral port :: Integer)
    toJSON (SockAddrW (SockAddrUnix str)) = String $ "Unix: " <> T.pack str
    toJSON (SockAddrW (SockAddrCan i)) = String $ "Can: " <> show i


instance ToJSON ClientInfo where
    toJSON ClientInfo {..} =
        object ["addr" .= ciSockAddr, "ssl" .= ciSSL, "headers" .= ciHeaders]

-- | Retrieves client info from scotty 'ActionT' monad.
getClientInfo :: (Monad m, ScottyError e) => ActionT e m ClientInfo
getClientInfo = do
    req <- getReq <$> ActionT ask
    pure $
        ClientInfo
            (SockAddrW $ remoteHost req)
            (isSecure req)
            (map HeaderW $ requestHeaders req)
