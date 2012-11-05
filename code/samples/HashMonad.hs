module PThash
       ( HashTab ,
         HM ,
         HMT ,
         hashing ,
         hashing_trans ,
         null_tab ,
         init_hash ,
         init_hash_trans ,
         eval_hash ,
         eval_hash_trans ,
       ) where 

import Data.Word 
import Data.Char 
import Control.Monad.Trans

type HashTab s = Word -> Maybe s
newtype HM s t = HM {hashState :: HashTab s -> (t, HashTab s)}
newtype (Monad m) => HMT s m t = HMT {hashStateT :: HashTab s -> m (t, HashTab s)}

instance Monad (HM s) where 
  return x = HM $ \ht -> (x, ht) 
  (HM hs) >>= f = 
    HM $ \ht -> let (x, new_ht) = hs ht 
                    (HM hs_b) = f x
                in hs_b new_ht
                   
instance Monad m => Monad (HMT s m) where 
  return x = HMT $ (\ht -> return (x, ht))
  (HMT hst) >>= f = 
    HMT $ \ht -> do (x, new_ht) <- hst ht 
                    let (HMT hst_b) = f x 
                    hst_b new_ht

instance MonadTrans (HMT s) where 
  lift c = HMT $ \ht -> c >>= (\x -> return (x, ht))

null_tab :: HashTab s
null_tab n = Nothing

init_hash :: HM s ()
init_hash = HM $ \ht -> ((), null_tab)

init_hash_trans :: (Monad m) => HMT s m ()
init_hash_trans = HMT $ \ht -> return ((), null_tab)

eval_hash :: HM s t -> HashTab s -> t 
eval_hash (HM hs) ht =
  fst $ hs ht 
      
eval_hash_trans :: (Monad m) => HMT s m t -> HashTab s -> m t 
eval_hash_trans (HMT hst) ht =
  (hst ht) >>= (return . fst )
      
base_hashing :: String -> (Word, Word)   
base_hashing l = 
  case l of 
    [] -> (0, 127)
    c : l' -> 
      let (v, d) = base_hashing l' 
          c_val = fromIntegral (ord c)
      in (v + c_val, d * base + c_val)
      where base = 127 

hash_probing :: (Word, String) -> Word -> HashTab String -> (Word, HashTab String) 
hash_probing (hval, s) disp htab  
  | htab hval == Nothing = (hval, \x -> if x == hval then Just s else htab x)
  | htab hval == Just s = (hval, htab) 
  | otherwise = hash_probing (hval+disp, s) disp htab

hashing :: String -> HM String Word
hashing s = 
  HM $ \ht -> let (v, d) = base_hashing s 
              in if d `mod` 2 == 0 
                 then hash_probing (v, s) (d+1) ht
                 else hash_probing (v, s) d ht
                      
hashing_trans :: (Monad m) => String -> HMT String m Word 
hashing_trans s =
  HMT $ \ht -> let (v, d) = base_hashing s
               in if d `mod` 2 == 0
                  then return $ hash_probing (v, s) (d+1) ht 
                  else return $ hash_probing (v, s) d ht