module Utils where

import Debug.Trace

trace' :: Show a => a -> b -> b
trace' a = trace (show a)