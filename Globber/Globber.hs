module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('?':xs) (_:ys) = matchGlob xs ys
-- other cases
matchGlob (x:xs) (y:ys) = x == y && matchGlob xs ys
