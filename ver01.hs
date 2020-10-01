import qualified Data.Set as S

data Foo = Foo { left :: [Foo], right :: [Foo] }
    deriving (Show)
    
instance Ord Foo where
    lfo <= rfo = (not (S.member True (S.fromList (map (rfo <=) (left lfo))))) && (not(S.member True (S.fromList (map (<= lfo) (right rfo)))))                    

instance Eq Foo where
    lfo == rfo = (lfo <= rfo) && (rfo <= lfo)

instance Num Foo where
    (+) lfo rfo = Foo (S.toList (S.union (S.fromList (map (lfo +) (left rfo))) (S.fromList (map (rfo +) (left lfo))))) (S.toList (S.union (S.fromList (map (rfo +) (right lfo))) (S.fromList (map (lfo +) (right rfo)))))