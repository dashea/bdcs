module Utils.Monad(concatForM,
                   concatMapM,
                   foldMaybeM,
                   mapMaybeM)
 where

import Data.Maybe(catMaybes)

concatForM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
concatForM lst fn = fmap concat (mapM fn lst)

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = fmap concat (mapM fn lst)

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM fn = fmap catMaybes . mapM fn

-- foldM, but skip Nothing results
foldMaybeM :: (Monad m) => (b -> a -> m (Maybe b)) -> b -> [a] -> m b
foldMaybeM _ acc [] = return acc
foldMaybeM action acc (x:xs) = do
    result <- action acc x
    case result of
        -- skip this element, continue with the original accumulator
        Nothing -> foldMaybeM action acc xs
        -- Keep this one
        Just r  -> foldMaybeM action r xs
