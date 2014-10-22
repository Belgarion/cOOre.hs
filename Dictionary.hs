module Dictionary where
import qualified Data.Map as Map

lookupinsert :: Ord k => k -> a -> Map.Map k a -> Maybe (Map.Map k a)
lookupinsert key value mp = case (Map.lookup key mp) of
						Nothing -> Just (Map.insert key value mp)
						Just _  -> Nothing