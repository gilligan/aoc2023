module AOC.Util
  ( module Text.Trifecta,
    readItemsFromFile,
    readItemsFromFileWith,
    parseFile,
    chunksOf,
    update,
  )
where

import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Trifecta

update :: Integer -> (a -> a) -> [a] -> [a]
update idx f xs = take (fromIntegral idx) xs ++ [f (xs !! fromIntegral idx)] ++ drop (fromIntegral idx + 1) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0 = error "invalid split size"
  | null xs = []
  | otherwise = take n xs : chunksOf n (drop n xs)

parseFile :: FilePath -> Parser a -> IO (Result a)
parseFile f p = TIO.readFile f <&> (parseString p mempty . T.unpack)

readItemsFromFile :: (Read a) => FilePath -> IO [a]
readItemsFromFile p = readItemsFromFileWith p (read . T.unpack)

readItemsFromFileWith :: FilePath -> (T.Text -> a) -> IO [a]
readItemsFromFileWith p f = do
  contents <- TIO.readFile p
  let lines = init $ T.splitOn "\n" contents
  return $ f <$> lines
