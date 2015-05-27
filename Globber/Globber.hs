module Globber (matchGlob) where

import Data.List (any, drop, map)

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('*':xs) ys = any (matchGlob xs)
                        (map (\i -> drop i ys) [0..(length ys)])
matchGlob ('?':xs) (y:ys) = y /= ' ' &&  matchGlob xs ys
matchGlob ('\\':x:xs) (y:ys) = x == y && matchGlob xs ys
matchGlob (x:xs) (y:ys) = x == y && matchGlob xs ys
matchGlob [] [] = True
matchGlob _ _ = False
