--Taak Haskell
insert x [] = [x]
insert x (y:ys)
   | x < y     = x:y:ys
   | otherwise = y:(insert x ys)
   
sort [] = []
sort (x:xs) = insert x (sort xs)

som [] = 0
som (x:xs) = x + som xs

grootsteWaarde x [] = x
grootsteWaarde x (y:ys)
	| x < y	= grootsteWaarde y ys
	| otherwise		= grootsteWaarde x ys
hoogsteWaarde (x:xs) = grootsteWaarde x xs

kleinsteWaarde x [] = x
kleinsteWaarde x (y:ys)
	| x > y	= grootsteWaarde y ys
	| otherwise		= grootsteWaarde x ys
laagsteWaarde (x:xs) = grootsteWaarde x xs

-- x = filesize
-- y = available space
teGroot x y
	| som x < som y		= False
	| otherwise			= True
	
-- Extra interessante Functies
maxInt = maxBound :: Int
minInt = minBound :: Int