import Network.HTTP
import Text.HTML.TagSoup

getArticle :: String -> [Tag String]
getArticle pg = parseTags pg

-- String: url
--
getHttpBody :: String -> IO String
getHttpBody url = simpleHTTP (getRequest url) >>= getResponseBody

--look for div id = "body" tag is div, attr is id value of attr is body
main = do
  cont <- getContents
  let lns = lines cont
  -- mapM_ print lns
  mapM_ (\x -> getHttpBody x >>= print . show . getArticle) lns
