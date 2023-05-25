module Colours
 ( color
 , red
 , blue
 , green
 , orange
 , purple
 )
where

-- | colours
color :: Int -> String -> String
color code text = "\x1b[" ++ show code ++ "m" ++ text ++ "\x1b[0m"
red :: String -> String
red = color 31
blue :: String -> String
blue = color 34
green :: String -> String
green = color 32
orange :: String -> String
orange = color 33
purple :: String -> String
purple = color 35
