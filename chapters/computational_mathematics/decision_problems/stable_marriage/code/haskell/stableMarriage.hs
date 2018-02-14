import           Data.Map as M (Map, (!))
import qualified Data.Map as M
import           Data.List (elemIndex)
import           Control.Monad.State

stableMatching :: (Ord a, Ord b) => [(a, [b])] -> [(b, [a])] -> [(a, b)]
stableMatching men women = evalState (propose (M.fromList women) men) M.empty

propose :: (Ord a, Ord b) => Map b [a] ->
                            [(a, [b])] ->
                            State (Map b (a, [b])) [(a, b)]
propose _ [] = get >>=  return . map (\(w, (m,_)) -> (m, w)) . M.assocs
propose women ((man, pref):bachelors) = do
  let theOne = head pref
  couples <- get
  case M.lookup theOne couples of
    Nothing -> do
      modify $ M.insert theOne (man, (tail pref))
      propose women bachelors
    Just (boyfriend, planB) -> do
      let rank x = elemIndex x (women!theOne)
      if rank boyfriend < rank man
        then propose women $ (man, tail pref): bachelors
        else do
          modify $ M.insert theOne (man, (tail pref)) . M.delete theOne
          propose women $ (boyfriend, planB): bachelors

main = do
  let aPref = [('A',"YXZ"), ('B',"ZYX"),('C', "XZY")]
      bPref = [('X',"BAC"), ('Y',"CBA"),('Z', "ACB")]
  print $ stableMatching aPref bPref
