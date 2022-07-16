module Util.Colors where

type Color = String

black :: String
black   = "\x1b[30m"

red :: String
red = "\x1b[31m"

green :: String
green = "\x1b[32m"

yellow :: String
yellow  = "\x1b[33m"

blue :: String
blue = "\x1b[34m"

magenta :: String
magenta = "\x1b[35m"

cyan :: String
cyan = "\x1b[36m"

white :: String
white = "\x1b[37m"

reset :: String
reset = "\x1b[37m"

colorize :: Color -> String -> String
colorize color s = color ++ s ++ reset
