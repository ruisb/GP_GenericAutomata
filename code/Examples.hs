
type CDA a s = LTS Id a s 
type PDA a s = LTS Maybe a s 
type NDA a s = LTS Set a s
type LSA a  s = LTS [] a s
newtype Id x = Id {unId :: x}


pda1 :: PDA Char Int
pda1 = M.fromList  [ ('a', funcFromList  [ (1,  Just 2   )
                                         , (2,  Nothing  )
                                         , (3,  Nothing  )
                                         ])
                   , ('b', funcFromList  [ (1,  Just 3   )
                                         , (2,  Just 3   )
                                         , (3,  Nothing  )
					 ])
                   ]

lsa1 :: LSA Char Int
lsa1 = M.fromList  [ ('a', funcFromList  [ (1,  [2,1]    )
                                         , (2,  []       )
                                         , (3,  []       )
                                         ])
                   , ('b', funcFromList  [ (1,  [3]      )
                                         , (2,  [1,3]    )
                                         , (3,  []       )
					 ])
                   ]
		  
funcFromList :: (Eq a) => [(a,b)] -> a -> b
funcFromList assoc x = guardFromJust "function not defined" (lookup x assoc)




instance ParRegular [] where
  type PBF []= Unit :+: Par :*: Rec
  from [] = L Unit
  from (x:xs) = R (Par x :*: Rec xs)
  to (L Unit) = []
  to (R (Par x:*:Rec xs)) = x : xs


data WeightedTrans s = WT { weight :: Int, nextst :: s} deriving (Show) --, Eq, Ord)
type NDWeightTrans = Set :.: WeightedTrans

type WeightedAut s = LTS NDWeightTrans () s

instance ParRegular WeightedTrans where
  type PBF WeightedTrans =  (K Int :*: Par)
--  efrom = PSet . S.map (\(WT w s) -> K w :*: Par s) . unNWT
--  eto   = NWT . S.map (\(K w :*: Par s) -> WT w s) - unPSet
  from (WT w s) = K w :*: Par s
  to (K w :*: Par s) = WT w s

instance EParRegular Set where
  type EPBF Set = PSet Par
  efrom = PSet . S.map Par 
  eto   = S.map unPar . unPSet

unPSet (PSet s) = s
unPar (Par x ) = x

instance Ord1 WeightedTrans where
  compare1 (WT w1 s1) (WT w2 s2)
           | s1 < s2   = LT
           | s1 > s2   = GT                    
           | w1 < w2   = LT
           | w1 > w2   = GT                    
           | otherwise = EQ

wa1 :: WeightedAut Int
wa1 = M.fromList [( () , 
       funcFromList [ (1 , Comp $ S.fromList [WT 1 3, WT 2 2 , WT 5 3] ) 
                    , (2 , Comp $ S.fromList [WT 4 2, WT 7 4] )
                    , (3 , Comp $ S.fromList [WT 1 3, WT 2 2, WT 5 1] )
                    , (4 , Comp $ S.fromList [WT 1 2, WT 0 4] )
                    ]
      )]

pulse n = replicate n ()

instance Monad WeightedTrans where
   return x = WT 0 x
   (WT w1 x) >>= k = let WT w2 y = k x
                     in WT (w1+w2) y






--concrete examples
nothing = L Unit
just = R . Id
dfa1 :: LTS Char (Unit :+: Id) Int
dfa1 = M.fromList [ ('a', funcFromList [ (1,just 2)
                                       , (2,nothing)
                                       , (3,nothing)
                                       ])
                  , ('b', funcFromList [ (1,just 3)
                                       , (2,just 3)
                                       , (3,nothing)])
                  ]


l :: [a] -> List a
l = Lst . l2list

--stupid non generic way, just to check that fnctions are cprrectly defined
instance Crush [] where
   crush = foldr

lfa1 :: LTS Char [] Int
lfa1 = M.fromList [ ('a', funcFromList [ (1,[2,1])
                                       , (2,[] )
                                       , (3,[])
                                       ])
                  , ('b', funcFromList [ (1,[3])
                                       , (2,[1,3])
                                       , (3,[])])
                  ]


lfa2 :: LTS Char [] Int

lfa2 = M.fromList [ ('a', funcFromList [ (1,[2,4])
                                       , (3,[2,4])
                                       , (2,[])
                                       , (4,[])
                                       ])
                  , ('b', funcFromList [ (1,[2,4])
                                       , (3,[2,4])
                                       , (2,[])
                                       , (4,[])
                                       ])
                  ]

