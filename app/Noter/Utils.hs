module Noter.Utils(if') where

if' :: Bool -> a -> a -> a
if' True r _  = r
if' False _ r = r
