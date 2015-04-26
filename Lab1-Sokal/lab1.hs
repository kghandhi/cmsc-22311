import Network.HTTP
import Text.HTML.TagSoup

-- getArticle :: String -> [Tag String]
-- getArticle pg = parseTags pg

-- String: url
--
openURL :: String -> IO String
openURL url = simpleHTTP (getRequest url) >>= getResponseBody

--look for div id = "body" tag is div, attr is id value of attr is body
-- main = do
--   cont <- getContents
--   let lns = lines cont
--   -- mapM_ print lns
--   mapM_ (\x -> getHttpBody x >>= print . show . getArticle) lns


main = do
  tags <- fmap parseTags $ openURL "http://muse.jhu.edu/journals/postmodern_culture/v024/24.1.marriott.html"
  let body = head $ sections (~== "<div id=body>") tags
  print . show $ body

-- main = do
--   src <- openURL "http://muse.jhu.edu/journals/postmodern_culture/v024/24.1.marriott.html"
--   writeFile "temp.html" src
