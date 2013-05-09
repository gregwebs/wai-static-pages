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

renderStaticPages :: [Char] -> [String] -> Application -> IO ()
renderStaticPages directory requests app = do
  flip mapM_ requests $ \path -> do
    let p = noFrontSlash $ noTrailSlash path
    let req = setRawPathInfo defaultRequest $ BS8.pack p 
    let outPath = (directory ++ (BS8.unpack $ rawPathInfo req) ++ ".html")
    print (p, outPath)
    rsp <- runResourceT $ app req
    case rsp of
      ResponseBuilder s h b ->
        let body = toLazyByteString b
        in  if s /= H.status200 then error $ "unexpected status: " ++ show s ++ "\nheaders: " ++ show h ++ "\nbody: " ++ show body else
              LBS.writeFile outPath body 
      _ -> error "expected ResponseBuilder"

noTrailSlash :: String -> String
noTrailSlash = reverse . noFrontSlash . reverse

noFrontSlash :: String -> String
noFrontSlash ('/':str) = str
noFrontSlash str = str

-- | Conveniently specify paths through nested indentation
-- This is a partial function which calls 'error' on
-- invalid input.
--
-- > import Shakespeare.Text (st)
-- >
-- > staticPaths = parseRoutePaths $ T.unpack [st|
-- >/pages
-- >        about
-- >        faq
-- >        /
-- >|]
--
-- > staticPaths == ["/pages/about", "/pages/faq", "/pages"]
parseRoutePaths :: String -> [String]
parseRoutePaths =
    parse [(0, "")] . filter (not . null) . lines
  where
    parse :: [(Int, String)] -> [String] -> [String]
    parse _ [] = []
    parse prefixes (line:[]) = [snd $ parseLine prefixes line]
    parse prefixes (line:nextLine:otherLines) =
      let (prefixes', parsed) = parseLine prefixes line
          (nextPrefixes, nextParsed) = parseLine prefixes' nextLine
          rest = nextParsed : parse nextPrefixes otherLines
      in  if length nextPrefixes > length prefixes'
            then rest
            else parsed : rest

    parseLine :: [(Int, String)] -> String -> ([(Int, String)], String)
    parseLine prefixes line = (prefixes', parsed)
      where
        (smaller_prefixes, _) = partition (\(indent, _) -> numSpaces > indent) prefixes
        numSpaces = length spaces
        spaces = takeWhile (== ' ') line

        prefixes' =
          if null smaller_prefixes || numSpaces > (fst $ last smaller_prefixes)
            then smaller_prefixes ++ [newPrefix]
            else smaller_prefixes
        newPrefix = (numSpaces, path)

        parsed = foldl1 (</>) (map snd smaller_prefixes ++ [path])
          where
            a </> b = noTrailSlash a ++ "/" ++ noFrontSlash b

        path =
          case words line of
              [p] -> p
              _ -> error $ "Invalid line: " ++ line
