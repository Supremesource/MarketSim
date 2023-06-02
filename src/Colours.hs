module Colours
 ( color
 , red
 , blue
 , green
 , orange
 , purple
 , cyan
 ,gray
 )
where

-- | colours:
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
cyan :: String -> String
cyan = color 36
gray :: String -> String
gray = color 37
