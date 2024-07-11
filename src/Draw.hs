{-# LANGUAGE ForeignFunctionInterface #-}

module Main(main) where

import Foreign
import Foreign.C.Types

-- foreign export ccall setup :: IO ()
foreign import ccall "createCanvas" createCanvas :: Int -> Int -> IO ()
foreign import ccall "openWindow"   openWindow   :: IO ()

main :: IO ()
main = do
    createCanvas 400 400
    openWindow
