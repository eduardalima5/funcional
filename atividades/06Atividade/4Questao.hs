-- dado três listas zipálas numa lista de triplas de forma 
-- semelhante ao comando zip. 
zip'linha :: [a] -> [b] -> [c] -> [(a,b,c)] 
zip'linha (x:xs) (y:ys) (z:zs) = (x, y, z) : zip'linha xs ys zs
zip'linha _ _ _ = []
-- teste 01
-- zip'linha
-- $ [1,2,3] "abc" [TRUE,FALSE,TRUE] 
-- $ [(1,"a", TRUE), (2, "b", FALSE), (3, "c", TRUE)] 
-- teste 02
-- $ zip'linha [1,2,3,4] "abc" [TRUE] 
-- $ [(1,"a",TRUE)]