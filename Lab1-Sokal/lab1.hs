import Network.HTTP


-- String: url
--
getBody :: String -> IO String
getBody url = simpleHTTP (getRequest url) >>= getResponseBody

main = do
  cont <- getContents
  let lns = lines cont
  mapM_ (\x -> getBody x >>= print) lns
