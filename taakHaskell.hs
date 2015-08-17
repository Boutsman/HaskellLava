--Taak Haskell
insert x [] = [x]
insert x (y:ys)
   | x < y     = x:y:ys
   | otherwise = y:(insert x ys)
   
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

som [] = 0
som (x:xs) = x + som xs

-- x = filesize
-- y = available space
teGroot x y
	| som x < som y		= False
	| otherwise			= True