{-# Language OverloadedStrings #-}
module Network.WAI.Application.StaticPages (
    parseRoutePaths
  , renderStaticPages
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

renderStaticPages :: Application -> Text -> [Text] -> IO ()
renderStaticPages app directory requests = do
  flip mapM_ requests $ \path -> do
    let p = noFrontSlash $ noTrailSlash path
    let req = setRawPathInfo defaultRequest $ encodeUtf8 p 
    let outPath = directory `mappend` (decodeUtf8 $ rawPathInfo req) `mappend` ".html"
    print (p, outPath)
    rsp <- runResourceT $ app req
    case rsp of
      ResponseBuilder s h b ->
        let body = toLazyByteString b
        in  if s /= H.status200 then error $ "unexpected status: " ++ show s ++ "\nheaders: " ++ show h ++ "\nbody: " ++ show body else
              LBS.writeFile (T.unpack outPath) body 
      _ -> error "expected ResponseBuilder"

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
-- >|]
--
-- > staticPaths == ["/pages/about", "/pages/faq", "/pages"]
parseRoutePaths :: Text -> [Text]
parseRoutePaths =
    parse [(0, "")] . filter (not . T.null) . T.lines
  where
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
