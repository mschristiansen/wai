---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Gzip
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic gzip compression of responses.
--
---------------------------------------------------------
module Network.Wai.Middleware.Gzip
    ( gzip
    , GzipSettings
    , gzipFiles
    , GzipFiles (..)
    , gzipCheckMime
    , def
    , defaultCheckMime
    ) where

import Network.Wai
import Data.Maybe (fromMaybe, isJust)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import Data.Default.Class
import Network.HTTP.Types ( Status, Header, hContentEncoding, hUserAgent
                          , hContentType, hContentLength)
import Network.HTTP.Types.Header (hAcceptEncoding)
import System.Directory (doesFileExist, createDirectoryIfMissing, renameFile, removeFile)
import Blaze.ByteString.Builder (fromByteString)
import Control.Exception (try, SomeException)
import qualified Data.Set as Set
import Network.Wai.Header
import Network.Wai.Internal
import qualified Data.Streaming.Blaze as B
import qualified Data.Streaming.Zlib as Z
import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad (unless)
import Data.Function (fix)
import Control.Exception (throwIO)
import qualified System.IO as IO
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Word8 (_semicolon)
import Control.Monad ((<=<))
import Debug.Trace (traceIO)


data GzipSettings = GzipSettings
    { gzipFiles :: GzipFiles
    , gzipCheckMime :: S.ByteString -> Bool
    }

-- | Gzip behavior for files.
data GzipFiles
    = GzipIgnore -- ^ Do not compress file responses.
    | GzipCompress -- ^ Compress files. Note that this may counteract
                   -- zero-copy response optimizations on some
                   -- platforms.
    | GzipCacheFolder FilePath -- ^ Compress files, caching them in
                               -- some directory.
    | GzipPreCompressed GzipFiles -- ^ If we use compression then try to use the filename with ".gz"
                                  -- appended to it, if the file is missing then try next action
                                  --
                                  -- @since 3.0.17
    deriving (Show, Eq, Read)

-- | Use default MIME settings; /do not/ compress files.
instance Default GzipSettings where
    def = GzipSettings GzipIgnore defaultCheckMime

-- | MIME types that will be compressed by default:
-- @text/*@, @application/json@, @application/javascript@,
-- @application/ecmascript@, @image/x-icon@.
defaultCheckMime :: S.ByteString -> Bool
defaultCheckMime bs =
    S8.isPrefixOf "text/" bs || bs' `Set.member` toCompress
  where
    bs' = fst $ S.break (== _semicolon) bs
    toCompress = Set.fromList
        [ "application/json"
        , "application/javascript"
        , "application/ecmascript"
        , "image/x-icon"
        ]

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- File responses will be compressed according to the 'GzipFiles' setting.
gzip :: GzipSettings -> Middleware
gzip set app req sendResponse =
    -- check request to see if support for gzip
    app req $ sendResponse <=< if notAcceptGzip || isMSIE6 then return else gzipResponse
  where
    gzipResponse :: Response -> IO Response
    gzipResponse resp = do
      -- check response to decide to gzip
      (if notGzipMime || encoded || small then return else encode (gzipFiles set)) resp
      where
        notGzipMime = not $ (gzipCheckMime set) contentType
        encoded = isJust $ lookup hContentEncoding respHeaders
        -- any type of content encoding
        small = case contentLength respHeaders of
          Nothing -> False -- This could be a streaming case
          Just len -> len < minimumLength
        -- For a small enough response, gzipping will actually increase the size
        -- Potentially for anything less than 860 bytes gzipping could be a net loss
        -- The actual number is application specific though and may need to be adjusted
        -- http://webmasters.stackexchange.com/questions/31750/what-is-recommended-minimum-object-size-for-gzip-performance-benefits
        minimumLength = 860
        contentType = fromMaybe "" $ lookup hContentType respHeaders
        respHeaders = responseHeaders resp
    notAcceptGzip = not $ "gzip" `elem` enc
    enc = fromMaybe [] $ (splitCommas . S8.unpack)
                    <$> lookup hAcceptEncoding reqHeaders
    isMSIE6 = "MSIE 6" `S.isInfixOf` ua
    ua = fromMaybe "" $ lookup hUserAgent reqHeaders
    reqHeaders = requestHeaders req

encode :: GzipFiles -> Response -> IO Response
encode set res = do
    let gh = fixHeaders $ responseHeaders res
    case res of
      (ResponseFile s rh file Nothing) ->
        case set of
          GzipIgnore -> return res
          GzipCompress -> do
            traceIO "gzip-compress"
            -- file' <- encodeFile file
            return $ ResponseFile s gh file Nothing
          (GzipCacheFolder cache) -> do
            file' <- cacheCheck file cache
            return $ ResponseFile s gh file' Nothing
          (GzipPreCompressed next) -> do
            let file' = file ++ ".gz"
            exist <- doesFileExist file'
            if exist
              then return $ ResponseFile s (fixHeaders rh) file' Nothing
              else encode next res
      ResponseRaw _ re -> encode set re
      resp -> compressE resp

compressE :: Response -> IO Response
compressE res = do
    wb $ \body -> return $ responseStream s hs' $ \sendChunk flush -> do
      (blazeRecv, _) <- B.newBlazeRecv B.defaultStrategy
      deflate <- Z.initDeflate 1 (Z.WindowBits 31)
      let sendBuilder builder = do
              popper <- blazeRecv builder
              fix $ \loop -> do
                  bs <- popper
                  unless (S.null bs) $ do
                      sendBS bs
                      loop
          sendBS bs = Z.feedDeflate deflate bs >>= deflatePopper
          flushBuilder = do
              sendBuilder Blaze.flush
              deflatePopper $ Z.flushDeflate deflate
              flush
          deflatePopper popper = fix $ \loop -> do
              result <- popper
              case result of
                  Z.PRDone -> return ()
                  Z.PRNext bs' -> do
                      sendChunk $ fromByteString bs'
                      loop
                  Z.PRError e -> throwIO e
      body sendBuilder flushBuilder
      sendBuilder Blaze.flush
      deflatePopper $ Z.finishDeflate deflate
  where
    (s, hs, wb) = responseToStream res
    hs' = fixHeaders hs

-- Remove Content-Length header, since we will certainly have a
-- different length after gzip compression.
fixHeaders :: [Header] -> [Header]
fixHeaders =
    ((hContentEncoding, "gzip") :) . filter notLength
  where
    notLength (x, _) = x /= hContentLength

splitCommas :: String -> [String]
splitCommas [] = []
splitCommas x =
    let (y, z) = break (== ',') x
     in y : splitCommas (dropWhile (== ' ') $ drop 1 z)

    -- res@ResponseFile{} -> sendResponse (gzipFile (gzipFiles set) res)
    --     _ -> if acceptGzip && not isMSIE6 && not (isEncoded res) && (bigEnough res)
    --             then
    --                 let runAction x = case x of
    --                         (ResponseFile s hs file Nothing, GzipPreCompressed nextAction) ->
    --                              let
    --                                 compressedVersion = file ++ ".gz"
    --                              in
    --                                 doesFileExist compressedVersion >>= \x ->
    --                                    if x
    --                                      then (sendResponse $ ResponseFile s (fixHeaders hs) compressedVersion Nothing)
    --                                      else (runAction (ResponseFile s hs file Nothing, nextAction))
    --                         (ResponseFile s hs file Nothing, GzipCacheFolder cache) ->
    --                             case lookup hContentType hs of
    --                                 Just m
    --                                     | gzipCheckMime set m -> compressFile s hs file cache sendResponse
    --                                 _ -> sendResponse res
    --                         (ResponseFile {}, GzipIgnore) -> sendResponse res
    --                         _ -> compressE set res sendResponse
    --                 in runAction (res, gzipFiles set)
    --             else sendResponse res
  -- where
  --   acceptGzip = "gzip" `elem` enc
  --   enc = fromMaybe [] $ (splitCommas . S8.unpack)
  --                   `fmap` lookup "Accept-Encoding" (requestHeaders env)
  --   ua = fromMaybe "" $ lookup hUserAgent $ requestHeaders env
  --   isMSIE6 = "MSIE 6" `S.isInfixOf` ua
  --   isEncoded res = isJust $ lookup hContentEncoding $ responseHeaders res

  --   bigEnough rsp = case contentLength (responseHeaders rsp) of
  --     Nothing -> True -- This could be a streaming case
  --     Just len -> len >= minimumLength

    -- For a small enough response, gzipping will actually increase the size
    -- Potentially for anything less than 860 bytes gzipping could be a net loss
    -- The actual number is application specific though and may need to be adjusted
    -- http://webmasters.stackexchange.com/questions/31750/what-is-recommended-minimum-object-size-for-gzip-performance-benefits
    -- minimumLength = 860


-- compressFile :: Status -> [Header] -> FilePath -> FilePath -> (Response -> IO a) -> IO a
-- compressFile s hs file cache sendResponse = do
--     e <- doesFileExist tmpfile
--     if e
--         then onSucc
--         else do
--             createDirectoryIfMissing True cache
--             x <- try $
--                  IO.withBinaryFile file IO.ReadMode $ \inH ->
--                  IO.withBinaryFile tmpfile IO.WriteMode $ \outH -> do
--                     deflate <- Z.initDeflate 7 $ Z.WindowBits 31
--                     -- FIXME this code should write to a temporary file, then
--                     -- rename to the final file
--                     let goPopper popper = fix $ \loop -> do
--                             res <- popper
--                             case res of
--                                 Z.PRDone -> return ()
--                                 Z.PRNext bs -> do
--                                     S.hPut outH bs
--                                     loop
--                                 Z.PRError ex -> throwIO ex
--                     fix $ \loop -> do
--                         bs <- S.hGetSome inH defaultChunkSize
--                         unless (S.null bs) $ do
--                             Z.feedDeflate deflate bs >>= goPopper
--                             loop
--                     goPopper $ Z.finishDeflate deflate
--             either onErr (const onSucc) (x :: Either SomeException ()) -- FIXME bad! don't catch all exceptions like that!
--   where
--     onSucc = sendResponse $ responseFile s (fixHeaders hs) tmpfile Nothing

--     onErr _ = sendResponse $ responseFile s hs file Nothing -- FIXME log the error message

--     tmpfile = cache ++ '/' : map safe file
--     safe c
--         | 'A' <= c && c <= 'Z' = c
--         | 'a' <= c && c <= 'z' = c
--         | '0' <= c && c <= '9' = c
--     safe '-' = '-'
--     safe '_' = '_'
--     safe _ = '_'

cacheCheck :: FilePath -> FilePath -> IO FilePath
cacheCheck file cache = do
  createDirectoryIfMissing True cache
  exist <- doesFileExist safePath
  if exist then return safePath else file'
  where
    file' = undefined
      -- IO.withBinaryFile file IO.ReadMode $ \inH -> do
      --   (temp, outH) <- IO.openBinaryTempFile cache file
      --   d <- Z.initDeflate 7 $ Z.WindowBits 31
      --   let goPopper popper = fix $ \loop -> do
      --         res <- popper
      --         case res of
      --           Z.PRDone -> return ()
      --           Z.PRNext bs -> do
      --             S.hPut outH bs
      --             loop
      --           Z.PRError ex -> throwIO ex
      --   fix $ \loop -> do
      --     bs <- S.hGetSome inH defaultChunkSize
      --     unless (S.null bs) $ do
      --       popper <- Z.feedDeflate d bs
      --       goPopper popper
      --       loop
      --   goPopper $ Z.finishDeflate d
      --   renameFile temp safePath
      --   removeFile temp
      --   return safePath

    safePath = cache ++ '/' : map safe file
    safe c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
    safe '-' = '-'
    safe '_' = '_'
    safe _ = '_'
