













module Data.FiniteMap (
	FiniteMap,		-- abstract type

	emptyFM, unitFM, listToFM,

	addToFM,
	addToFM_C,
	addListToFM,
	addListToFM_C,
	delFromFM ,
	delListFromFM,

	plusFM,
	plusFM_C,
	minusFM,
	foldFM,

	intersectFM ,
	intersectFM_C ,
	mapFM , mapMaybeFM , filterFM ,

	sizeFM, isEmptyFM, elemFM, lookupFM, lookupWithDefaultFM,

	fmToList, keysFM, eltsFM


    ) where

import Data.Maybe ( isJust )




-- SIGH: but we use unboxed "sizes"...




--	BUILDING
emptyFM		:: FiniteMap key elt
unitFM		:: key -> elt -> FiniteMap key elt
listToFM	:: (Ord key {--}) => [(key,elt)] -> FiniteMap key elt
			-- In the case of duplicates, the last is taken


--	ADDING AND DELETING
		   -- Throws away any previous binding
		   -- In the list case, the items are added starting with the
		   -- first one in the list
addToFM		:: (Ord key {--}) => FiniteMap key elt -> key -> elt  -> FiniteMap key elt
addListToFM	:: (Ord key {--}) => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt

		   -- Combines with previous binding
		   -- In the combining function, the first argument is the "old" element,
		   -- while the second is the "new" one.
addToFM_C	:: (Ord key {--}) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> key -> elt
			   -> FiniteMap key elt
addListToFM_C	:: (Ord key {--}) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> [(key,elt)]
			   -> FiniteMap key elt

		   -- Deletion doesn't complain if you try to delete something
		   -- which isn't there
delFromFM	:: (Ord key {--}) => FiniteMap key elt -> key   -> FiniteMap key elt
delListFromFM	:: (Ord key {--}) => FiniteMap key elt -> [key] -> FiniteMap key elt

--	COMBINING
		   -- Bindings in right argument shadow those in the left
plusFM		:: (Ord key {--}) => FiniteMap key elt -> FiniteMap key elt
			   -> FiniteMap key elt

		   -- Combines bindings for the same thing with the given function
plusFM_C	:: (Ord key {--}) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

minusFM		:: (Ord key {--}) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
		   -- (minusFM a1 a2) deletes from a1 any bindings which are bound in a2

intersectFM	:: (Ord key {--}) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
intersectFM_C	:: (Ord key {--}) => (elt1 -> elt2 -> elt3)
			   -> FiniteMap key elt1 -> FiniteMap key elt2 -> FiniteMap key elt3

--	MAPPING, FOLDING, FILTERING
foldFM		:: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
mapFM		:: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
filterFM	:: (Ord key {--}) => (key -> elt -> Bool)
			   -> FiniteMap key elt -> FiniteMap key elt
mapMaybeFM      :: (Ord key {--})
                => (key -> elt1 -> Maybe elt2)
                -> FiniteMap key elt1
		-> FiniteMap key elt2

--	INTERROGATING
sizeFM		:: FiniteMap key elt -> Int
isEmptyFM	:: FiniteMap key elt -> Bool

elemFM		:: (Ord key {--}) => key -> FiniteMap key elt -> Bool
lookupFM	:: (Ord key {--}) => FiniteMap key elt -> key -> Maybe elt
lookupWithDefaultFM
		:: (Ord key {--}) => FiniteMap key elt -> elt -> key -> elt
		-- lookupWithDefaultFM supplies a "default" elt
		-- to return for an unmapped key

--	LISTIFYING
fmToList	:: FiniteMap key elt -> [(key,elt)]
keysFM		:: FiniteMap key elt -> [key]
eltsFM		:: FiniteMap key elt -> [elt]

data FiniteMap key elt
  = EmptyFM
  | Branch key elt	    	-- Key and elt stored here
    Int{-STRICT-}	-- Size >= 1
    (FiniteMap key elt)	    	-- Children
    (FiniteMap key elt)

emptyFM = EmptyFM
{-
emptyFM
  = Branch bottom bottom 0 bottom bottom
  where
    bottom = panic "emptyFM"
-}

-- #define EmptyFM (Branch _ _ 0 _ _)

unitFM key elt = Branch key elt 1 emptyFM emptyFM

listToFM = addListToFM emptyFM



addToFM fm key elt = addToFM_C (\ old new -> new) fm key elt

addToFM_C combiner EmptyFM key elt = unitFM key elt
addToFM_C combiner (Branch key elt size fm_l fm_r) new_key new_elt

  | new_key < key = mkBalBranch key elt (addToFM_C combiner fm_l new_key new_elt) fm_r
  | new_key > key = mkBalBranch key elt fm_l (addToFM_C combiner fm_r new_key new_elt)
  | otherwise	  = Branch new_key (combiner elt new_elt) size fm_l fm_r


addListToFM fm key_elt_pairs = addListToFM_C (\ old new -> new) fm key_elt_pairs

addListToFM_C combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs	-- foldl adds from the left
  where
    add fmap (key,elt) = addToFM_C combiner fmap key elt

delFromFM EmptyFM del_key = emptyFM
delFromFM (Branch key elt size fm_l fm_r) del_key

  | del_key > key
  = mkBalBranch key elt fm_l (delFromFM fm_r del_key)

  | del_key < key
  = mkBalBranch key elt (delFromFM fm_l del_key) fm_r

  | key == del_key
  = glueBal fm_l fm_r


delListFromFM fm keys = foldl delFromFM fm keys

plusFM_C combiner EmptyFM fm2 = fm2
plusFM_C combiner fm1 EmptyFM = fm1
plusFM_C combiner fm1 (Branch split_key elt2 _ left right)
  = mkVBalBranch split_key new_elt
		 (plusFM_C combiner lts left)
		 (plusFM_C combiner gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key
    new_elt = case lookupFM fm1 split_key of
		Nothing   -> elt2
		Just elt1 -> combiner elt1 elt2

-- It's worth doing plusFM specially, because we don't need
-- to do the lookup in fm1.

plusFM EmptyFM fm2 = fm2
plusFM fm1 EmptyFM = fm1
plusFM fm1 (Branch split_key elt1 _ left right)
  = mkVBalBranch split_key elt1 (plusFM lts left) (plusFM gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key

minusFM EmptyFM fm2 = emptyFM
minusFM fm1 EmptyFM = fm1
minusFM fm1 (Branch split_key elt _ left right)
  = glueVBal (minusFM lts left) (minusFM gts right)
	-- The two can be way different, so we need glueVBal
  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

intersectFM fm1 fm2 = intersectFM_C (\ left right -> right) fm1 fm2

intersectFM_C combiner fm1 EmptyFM = emptyFM
intersectFM_C combiner EmptyFM fm2 = emptyFM
intersectFM_C combiner fm1 (Branch split_key elt2 _ left right)

  | isJust maybe_elt1	-- split_elt *is* in intersection
  = mkVBalBranch split_key (combiner elt1 elt2) (intersectFM_C combiner lts left)
						(intersectFM_C combiner gts right)

  | otherwise			-- split_elt is *not* in intersection
  = glueVBal (intersectFM_C combiner lts left) (intersectFM_C combiner gts right)

  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

    maybe_elt1 = lookupFM fm1 split_key
    Just elt1  = maybe_elt1

foldFM k z EmptyFM = z
foldFM k z (Branch key elt _ fm_l fm_r)
  = foldFM k (k key elt (foldFM k z fm_r)) fm_l

mapFM f EmptyFM = emptyFM
mapFM f (Branch key elt size fm_l fm_r)
  = Branch key (f key elt) size (mapFM f fm_l) (mapFM f fm_r)

mapMaybeFM f EmptyFM = emptyFM
mapMaybeFM f (Branch key elt _ fm_l fm_r) =
  case f key elt of
    Nothing   -> glueVBal (mapMaybeFM f fm_l) (mapMaybeFM f fm_r)
    Just elt' -> mkVBalBranch key elt' (mapMaybeFM f fm_l) (mapMaybeFM f fm_r)

filterFM p EmptyFM = emptyFM
filterFM p (Branch key elt _ fm_l fm_r)
  | p key elt		-- Keep the item
  = mkVBalBranch key elt (filterFM p fm_l) (filterFM p fm_r)

  | otherwise		-- Drop the item
  = glueVBal (filterFM p fm_l) (filterFM p fm_r)

--{-# INLINE sizeFM #-}
sizeFM EmptyFM		     = 0
sizeFM (Branch _ _ size _ _) =  size

isEmptyFM fm = sizeFM fm == 0

lookupFM EmptyFM key = Nothing
lookupFM (Branch key elt _ fm_l fm_r) key_to_find

  | key_to_find < key = lookupFM fm_l key_to_find
  | key_to_find > key = lookupFM fm_r key_to_find
  | otherwise	  = Just elt


key `elemFM` fm
  = case (lookupFM fm key) of { Nothing -> False; Just elt -> True }

lookupWithDefaultFM fm deflt key
  = case (lookupFM fm key) of { Nothing -> deflt; Just elt -> elt }

fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm
keysFM fm   = foldFM (\ key elt rest -> key : rest)       [] fm
eltsFM fm   = foldFM (\ key elt rest -> elt : rest)       [] fm

sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: (Ord key {--}) 		-- Used for the assertion checking only
	 => Int
	 -> key -> elt
	 -> FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

mkBranch which key elt fm_l fm_r
  = --{--}

    let
	result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
--    if sizeFM result <= 8 then
	result
--    else
--	pprTrace ("mkBranch:"++(show which)) (ppr PprDebug result) (
--	result
--	)
  where
    left_ok  = case fm_l of
		EmptyFM		         -> True
		Branch left_key _ _ _ _  -> let
						biggest_left_key = fst (findMax fm_l)
					    in
					    biggest_left_key < key
    right_ok = case fm_r of
		EmptyFM		         -> True
		Branch right_key _ _ _ _ -> let
						smallest_right_key = fst (findMin fm_r)
					    in
					    key < smallest_right_key
    balance_ok = True -- sigh
{- LATER:
    balance_ok
      = -- Both subtrees have one or no elements...
	(left_size + right_size <= 1)
-- NO	      || left_size == 0  -- ???
-- NO	      || right_size == 0 -- ???
    	-- ... or the number of elements in a subtree does not exceed
	-- sIZE_RATIO times the number of elements in the other subtree
      || (left_size  * sIZE_RATIO >= right_size &&
    	  right_size * sIZE_RATIO >= left_size)
-}

    left_size  = sizeFM fm_l
    right_size = sizeFM fm_r


    unbox :: Int -> Int
    unbox x = x


mkBalBranch :: (Ord key {--})
	    => key -> elt
	    -> FiniteMap key elt -> FiniteMap key elt
	    -> FiniteMap key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l	-- Right tree too big
  = case fm_R of
	Branch _ _ _ fm_rl fm_rr
		| sizeFM fm_rl < 2 * sizeFM fm_rr -> single_L fm_L fm_R
		| otherwise	   	          -> double_L fm_L fm_R
	-- Other case impossible

  | size_l > sIZE_RATIO * size_r	-- Left tree too big
  = case fm_L of
	Branch _ _ _ fm_ll fm_lr
		| sizeFM fm_lr < 2 * sizeFM fm_ll -> single_R fm_L fm_R
		| otherwise		          -> double_R fm_L fm_R
	-- Other case impossible

  | otherwise				-- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = sizeFM fm_L
    size_r   = sizeFM fm_R

    single_L fm_l (Branch key_r elt_r _ fm_rl fm_rr)
	= mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr

    double_L fm_l (Branch key_r elt_r _ (Branch key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
	= mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
				 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)

    single_R (Branch key_l elt_l _ fm_ll fm_lr) fm_r
	= mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)

    double_R (Branch key_l elt_l _ fm_ll (Branch key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
	= mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
				 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)

mkVBalBranch :: (Ord key {--})
	     => key -> elt
	     -> FiniteMap key elt -> FiniteMap key elt
	     -> FiniteMap key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--	   (a) all keys in l are < all keys in r
--	   (b) all keys in l are < key
--	   (c) all keys in r are > key

mkVBalBranch key elt EmptyFM fm_r = addToFM fm_r key elt
mkVBalBranch key elt fm_l EmptyFM = addToFM fm_l key elt

mkVBalBranch key elt fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
		     fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    size_l = sizeFM fm_l
    size_r = sizeFM fm_r

glueBal :: (Ord key {--})
	=> FiniteMap key elt -> FiniteMap key elt
	-> FiniteMap key elt

glueBal EmptyFM fm2 = fm2
glueBal fm1 EmptyFM = fm1
glueBal fm1 fm2
	-- The case analysis here (absent in Adams' program) is really to deal
	-- with the case where fm2 is a singleton. Then deleting the minimum means
	-- we pass an empty tree to mkBalBranch, which breaks its invariant.
  | sizeFM fm2 > sizeFM fm1
  = mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)

  | otherwise
  = mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2
  where
    (mid_key1, mid_elt1) = findMax fm1
    (mid_key2, mid_elt2) = findMin fm2

glueVBal :: (Ord key {--})
	 => FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

glueVBal EmptyFM fm2 = fm2
glueVBal fm1 EmptyFM = fm1
glueVBal fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
	 fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)

  | otherwise		-- We now need the same two cases as in glueBal above.
  = glueBal fm_l fm_r
  where
    size_l = sizeFM fm_l
    size_r = sizeFM fm_r

splitLT, splitGT :: (Ord key {--}) => FiniteMap key elt -> key -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT EmptyFM split_key = emptyFM
splitLT (Branch key elt _ fm_l fm_r) split_key

  | split_key < key = splitLT fm_l split_key
  | split_key > key = mkVBalBranch key elt fm_l (splitLT fm_r split_key)
  | otherwise	    = fm_l


splitGT EmptyFM split_key = emptyFM
splitGT (Branch key elt _ fm_l fm_r) split_key

  | split_key > key = splitGT fm_r split_key
  | split_key < key = mkVBalBranch key elt (splitGT fm_l split_key) fm_r
  | otherwise	    = fm_r


findMin :: FiniteMap key elt -> (key,elt)
findMin (Branch key elt _ EmptyFM _) = (key,elt)
findMin (Branch key elt _ fm_l    _) = findMin fm_l

deleteMin :: (Ord key {--}) => FiniteMap key elt -> FiniteMap key elt
deleteMin (Branch key elt _ EmptyFM fm_r) = fm_r
deleteMin (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt (deleteMin fm_l) fm_r

findMax :: FiniteMap key elt -> (key,elt)
findMax (Branch key elt _ _ EmptyFM) = (key,elt)
findMax (Branch key elt _ _    fm_r) = findMax fm_r

deleteMax :: (Ord key {--}) => FiniteMap key elt -> FiniteMap key elt
deleteMax (Branch key elt _ fm_l EmptyFM) = fm_l
deleteMax (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt fm_l (deleteMax fm_r)




instance (Eq key, Eq elt) => Eq (FiniteMap key elt) where
  fm_1 == fm_2 = (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 == fmToList fm_2)

{- NO: not clear what The Right Thing to do is:
instance (Ord key, Ord elt) => Ord (FiniteMap key elt) where
  fm_1 <= fm_2 = (sizeFM   fm_1 <= sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 <= fmToList fm_2)
-}
