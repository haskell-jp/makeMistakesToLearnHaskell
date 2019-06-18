module Education.MakeMistakesToLearnHaskell.Report.Client
  ( Report (..)
  , Result (..)
  , postReport
  , EndpointUrl
  , ReportClientError
  ) where

import           Network.HTTP.Client                                (newManager)
import           Network.HTTP.Client.TLS                            (tlsManagerSettings)
import           Servant.API                                        ((:<|>) ((:<|>)))
import           Servant.Client                                     (ClientM, ServantError,
                                                                     client,
                                                                     mkClientEnv,
                                                                     parseBaseUrl,
                                                                     runClientM)

import           Education.MakeMistakesToLearnHaskell.Report.Server (Report (..),
                                                                     Result (..),
                                                                     api)

type EndpointUrl = String

type ReportClientError = ServantError

postReportM :: Report -> ClientM Result
_ :<|> postReportM = client api


postReport :: EndpointUrl -> Report -> IO (Either ReportClientError Result)
postReport url r = do
  manager <- newManager tlsManagerSettings
  env <- mkClientEnv manager <$> parseBaseUrl url
  runClientM (postReportM r) env
