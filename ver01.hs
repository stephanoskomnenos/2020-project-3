import qualified Data.Set as S

data Foo = Foo { left :: [Foo], right :: [Foo] }
    
instance Ord Foo where
    lfo <= rfo = (not (S.member True (S.fromList (map (rfo <=) (left lfo))))) && (not(S.member True (S.fromList (map (<= lfo) (right rfo)))))                    

instance Eq Foo where
    lfo == rfo = (lfo <= rfo) && (rfo <= lfo)

instance Num Foo where
    (+) lfo rfo = Foo (S.toList (S.union (S.fromList (map (lfo +) (left rfo))) (S.fromList (map (rfo +) (left lfo))))) (S.toList (S.union (S.fromList (map (rfo +) (right lfo))) (S.fromList (map (lfo +) (right rfo)))))

instance Show Foo where --这里还不完善，还需要把"\"和"""去除掉
    show (Foo lfo rfo) = show ("{" ++ init(tail(show lfo)) ++ "|" ++ init(tail(show rfo)) ++ "}")
    
