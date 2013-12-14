{-# Language OverloadedStrings #-}
module Network.WAI.Application.StaticPages (
    parseRoutePaths
  , renderStaticPages
  , renderStaticPagesTo
) where

import Data.List (partition)
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types as H
import Data.Conduit (runResourceT)
import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Monoid (mappend)
import System.Directory (createDirectoryIfMissing)

-- | Render the paths in the application, passing the path through the given function to determine
-- the filepath on disk.
renderStaticPagesTo :: Application 
                      -> [Text] -- ^ request paths
                      -> (Text -> Request -> FilePath) -- ^ convert the request path and request to a FilePath
                      -> IO ()
renderStaticPagesTo app requests toFp = do
  flip mapM_ requests $ \path -> do
    let p = notEmpty $ noFrontSlash $ noTrailSlash path
    let req = setRawPathInfo defaultRequest $ encodeUtf8 p 
    let outPath = toFp path req
    print (p, outPath)
    rsp <- runResourceT $ app req
    case rsp of
      ResponseBuilder s h b ->
        let body = toLazyByteString b
        in  if s /= H.status200
              then error $ "unexpected status: " ++ show s ++
                           "\nheaders: " ++ show h ++
                           "\nbody: " ++ show body
              else do
                createDirectoryIfMissing True $ dirname outPath
                LBS.writeFile outPath body
      _ -> error "expected ResponseBuilder"
  where
    dirname = T.unpack . T.intercalate "/" . init . T.splitOn "/" . T.pack
    notEmpty t | T.null t = "/"
               | otherwise = t

-- | Render the paths in the application, writing the results to the given directory with an .html
-- extension.
renderStaticPages :: Application
                  -> Text   -- ^ directory
                  -> [Text] -- ^ request paths
                  -> IO ()
renderStaticPages app directory requests =
    renderStaticPagesTo app requests $ \_ req ->
       T.unpack $ directory `mappend` emptyIndex (noTrailSlash $ decodeUtf8 $ rawPathInfo req) `mappend` ".html"
  where
    emptyIndex t | T.null t = "index"
                 | otherwise = t

noTrailSlash :: Text -> Text
noTrailSlash str | T.null str = str
noTrailSlash str | T.last str == '/' = T.init str
noTrailSlash str = str

noFrontSlash :: Text -> Text
noFrontSlash str | T.null str = str
noFrontSlash str | T.head str == '/' = T.tail str
noFrontSlash str = str

-- | Conveniently specify paths through nested indentation
-- This is a partial function which calls 'error' on
-- invalid input.
--
-- > import Shakespeare.Text (st)
-- >
-- > staticPaths = parseRoutePaths [st|
-- >/pages
-- >        about
-- >        faq
-- >        /
-- >-- commented out
-- >|]
--
-- > staticPaths == ["/pages/about", "/pages/faq", "/pages"]
parseRoutePaths :: Text -> [Text]
parseRoutePaths =
    parse [(0, "")] . filter (not . commentedOut) . filter (not . T.null) . T.lines
  where
    commentedOut = T.isPrefixOf "-- "

    parse :: [(Int, Text)] -> [Text] -> [Text]
    parse _ [] = []
    parse prefixes (line:[]) = [snd $ parseLine prefixes line]
    parse prefixes (line:nextLine:otherLines) =
      let (prefixes', parsed) = parseLine prefixes line
          (nextPrefixes, nextParsed) = parseLine prefixes' nextLine
          rest = nextParsed : parse nextPrefixes otherLines
      in  if length nextPrefixes > length prefixes'
            then rest
            else parsed : rest

    parseLine :: [(Int, Text)] -> Text -> ([(Int, Text)], Text)
    parseLine prefixes line = (prefixes', parsed)
      where
        (smaller_prefixes, _) = partition (\(indent, _) -> numSpaces > indent) prefixes
        numSpaces = T.length spaces
        spaces = T.takeWhile (== ' ') line

        prefixes' =
          if null smaller_prefixes || numSpaces > (fst $ last smaller_prefixes)
            then smaller_prefixes ++ [newPrefix]
            else smaller_prefixes
        newPrefix = (numSpaces, path)

        parsed = foldl1 (</>) (map snd smaller_prefixes ++ [path])
          where
            a </> b = noTrailSlash a `mappend` "/" `mappend` noFrontSlash b

        path =
          case T.words line of
              [p] -> p
              _ -> error $ "Invalid line: " ++ show line
