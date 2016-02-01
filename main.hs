{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           System.Environment
import           System.IO
import           Control.Lens
import           Control.Monad
import           Data.Text
import           Data.String
import           Network.AWS hiding (getEnv)
import           Network.AWS.S3
import           Data.Conduit
import           Data.Conduit.Binary

data S3App = S3App
    { s3BucketName :: BucketName
    , s3AccessKeyId :: AccessKey
    , s3SecretAccessKey :: SecretKey
    }

data Upload = Upload
    { uFileInfo :: FileInfo
    }

mkYesod "S3App" [parseRoutes|
/ HomeR POST
|]

instance Yesod S3App

instance RenderMessage S3App FormMessage where
    renderMessage _ _ = defaultFormMessage

postHomeR :: Handler Html
postHomeR = do
    upload <- runInputPost $ Upload
        <$> ireq fileField "file"
    uploadBody upload >>= putToAws (uploadObjectName upload)
    defaultLayout [whamlet|S3 upload POC!|]

uploadObjectName = fromString . unpack . fileName . uFileInfo

uploadBody upload = do
    let source = fileSource $ uFileInfo upload
    source $$ sinkLbs >>= return . toBody

putToAws name body = do
    S3App {..} <- getYesod
    s3Env <- newEnv Frankfurt $ FromKeys s3AccessKeyId s3SecretAccessKey
    s3Logger <- newLogger Debug stdout
    runResourceT . runAWS (s3Env & envLogger .~ s3Logger) $
        void . send $ putObject s3BucketName name body

main :: IO ()
main = do
    bucket <- fmap fromString $ getEnv "AWS_BUCKET_NAME"
    accessKeyId <- fmap fromString $ getEnv "AWS_ACCESS_KEY_ID"
    secretAccessKey <- fmap fromString $ getEnv "AWS_SECRET_ACCESS_KEY"
    warp 3000 $ S3App bucket accessKeyId secretAccessKey
