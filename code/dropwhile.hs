dwr p l = fst $ foldr accum ([], []) l
  where accum x (npl, pl) = if p x then (npl, x : pl) else if pl == [] then (x : npl, pl) else (x : (pl ++  npl), [])
  
dwri pred l = snd $ lf ()
  where lf = foldr accum (\_ -> ([], [])) l
        accum x f () = let (l1, l2) = f () 
                       in if pred x then (x : l1, l2) else ([], x : (l1 ++ l2))
                                                           
dropWhileFoldr :: (a -> Bool) -> [a] -> [a]
dropWhileFoldr p l = (foldr accum (\b -> []) l) True
  where accum x f = \b -> if p x && b then f b else x : (f False)

dwl :: (a -> Bool) -> [a] -> [a] 
dwl p l = fst $ foldl accum (l, False) l
  where accum (l, dropped) x = if p x && not dropped then (tail l, dropped) else (l, True)

