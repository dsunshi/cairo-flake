{-# LANGUAGE ForeignFunctionInterface #-}

module Draw() where

foreign export ccall setup :: IO ()
foreign import ccall "createCanvas" createCanvas :: Int -> Int -> IO ()

setup :: IO ()
setup = do
    createCanvas 200 200
