-- 46. (**) Truth tables for logical expressions.
-- λ> table (\a b -> (and' a (or' a b))) ~~~>
-- True True True
-- True False True
-- False True False
-- False False False


-- 47. (*) Truth tables for logical expressions (part 2).
-- λ> table2 (\a b -> a `and'` (a `or'` not b)) ~~~>
-- True True True
-- True False True
-- False True False
-- False False False


-- 48. (*) Truth tables for logical expressions (part 3).
-- λ> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c) ~~~> 
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True


-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False


-- 49. (**) Gray codes.
-- λ> gray 3 ~~~> ["000","001","011","010","110","111","101","100"]


-- 50. (***) Huffman codes.
-- λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] ~~~> [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
