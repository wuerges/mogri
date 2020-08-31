module Image
    (
        convertToJpeg,
        optSize
    ) where

import System.Process
import System.IO.Temp
import Graphics.Image
import System.PosixCompat

binarySearch :: (Int -> IO Int) -> Int -> Int -> (Int -> Bool) -> IO Int 
binarySearch f a b check = 
    if a == b 
        then return a
        else do
            let m = (a + b + 1) `div` 2
            r <- f m
            if check r 
                then binarySearch f m b check
                else binarySearch f a (m - 1) check


getFileSize :: String -> IO Int
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral (fileSize stat)

redim :: Int -> (Int, Int) -> (Int, Int)
redim factor (a, b) = (f a, f b)
    where f x = (x * factor) `div` 100 


checkSize :: FilePath -> FilePath -> Int -> IO Int 
checkSize input output factor = do    
    convertSize input output factor    
    s <- getFileSize output
    return s

convertSize :: FilePath -> FilePath -> Int  -> IO ()
convertSize input output factor = do
    image <- readImageRGB VU input
    writeImage output $ resize Bilinear Edge (redim factor $ dims image) image
    callProcess "jpegoptim" ["-m", "80", output ]


optSize :: FilePath -> FilePath -> Int  -> IO ()
optSize input output size = do
    f <- binarySearch (checkSize input output) 1 100 (<= size) 
    convertSize input output f


convertToJpeg :: FilePath -> FilePath -> IO ()
convertToJpeg input output = do
    temp <- emptySystemTempFile "temp.jpg"
    image <- readImageRGB VU input
    putStrLn $ "image cols:" ++ show (cols image)
    writeImage temp $ resize Bilinear Edge (dims image) image
    callProcess "jpegoptim" ["-m", "80", temp]

        

