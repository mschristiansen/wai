{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.GzipSpec
    ( main
    , spec
    ) where

import Test.Hspec

import Network.Wai.Middleware.Gzip
import Network.Wai.Test
import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAcceptEncoding)
import Codec.Compression.GZip (decompress)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "defaultCheckMime" $ do
    it "compresses text, json and x-icon MIME types" $ do
      defaultCheckMime "application/json" `shouldBe` True
      defaultCheckMime "application/javascript" `shouldBe` True
      defaultCheckMime "application/ecmascript" `shouldBe` True
      defaultCheckMime "application/something" `shouldBe` False
      defaultCheckMime "text/something" `shouldBe` True
      defaultCheckMime "text/html" `shouldBe` True
      defaultCheckMime "image/x-icon" `shouldBe` True
      defaultCheckMime "foo/bar" `shouldBe` False

    it "correctly handles charset" $ do
      defaultCheckMime "application/json; charset=utf-8" `shouldBe` True
      defaultCheckMime "text/html; charset=utf-8" `shouldBe` True

  describe "gzip" $ do
    it "returns json compressed if accept-encoding is present" $ do
      resp <- runApp (gzip def) [(hAcceptEncoding, "gzip")] [(hContentType, "application/json")]
      simpleHeaders resp `shouldBe` [(hContentEncoding, "gzip"), (hContentType, "application/json")]
      decompress (simpleBody resp) `shouldBe` "test"

    it "returns json unchanged if accept-encoding is not present" $ do
      resp <- runApp (gzip def) [] [(hContentType, "application/json")]
      simpleHeaders resp `shouldBe` [(hContentType, "application/json")]
      simpleBody resp `shouldBe` "test"

    it "returns json unchanged if user-agent is MSIE6" $ do
      resp <- runApp (gzip def) [ (hAcceptEncoding, "gzip")
                                , (hUserAgent, "Mozilla/4.0 (Windows; MSIE 6.0; Windows NT 6.0)")
                                ] [(hContentType, "application/json")]
      simpleHeaders resp `shouldBe` [(hContentType, "application/json")]
      simpleBody resp `shouldBe` "test"

    it "returns json unchanged if headers show already compressed" $ do
      -- Lie a little and don't compress the body.  This way we test
      -- that the compression is skipped based on the presence of
      -- the Content-Encoding header.
      resp <- runApp (gzip def) [(hAcceptEncoding, "gzip")] [(hContentEncoding, "gzip"), (hContentType, "application/json")]
      simpleHeaders resp `shouldBe` [(hContentEncoding, "gzip"), (hContentType, "application/json")]
      simpleBody resp `shouldBe` "test"

runApp :: Middleware -> RequestHeaders -> ResponseHeaders -> IO SResponse
runApp mw reqHeaders respHeaders = runSession (request defaultRequest { requestHeaders = reqHeaders }) (mw app)
  where
    app _ respond = respond $ responseLBS status200 respHeaders "test"
