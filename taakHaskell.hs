import System.IO
import Control.Monad (liftM)

transform :: [String] -> [String]
transform [] = []
transform (filesize:usbSpace:dss) =
	let (nrOfFiles:filesizes) = words filesize;
		(nrOfSticks:stickSizes) = words usbSpace;
		result = if (teGroot filesizes stickSizes) == True then "onmogelijk":[]
				else (show(som stickSizes - som filesizes)):[] --a:e:(transform dss); --result = "onmogelijk":[]
	in result

main :: IO ()
main = do
  n <- read `liftM` getLine :: IO Int 		-- Hier halen we eerst een regel op met getLine, en dat "liften" we naar een geheel getal met de read-functie. n is dus van het type Int.
  lss <- lines `liftM` getContents    		-- Alles regels ophalen, lss is type [String]

  let cases = take (2*n) lss			  	-- eerste n regels nemen, cases is type [String]
      chars = transform cases

  putStr $ unlines $ chars
  
-- Functies
insert x [] = [x]
insert x (y:ys)
   | x < y     = x:y:ys
   | otherwise = y:(insert x ys)
   
sort [] = []
sort (x:xs) = insert x (sort xs)

som [] = 0
som (x:y:xs) = (read x :: Int) + (read y :: Int) + som xs

isEven x
	| mod x 2 == 0	= True
	| otherwise		= False

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
	| som x <= som y		= False
	| otherwise			= True
	
testLet x = 
	let result = som x; 
		out = result
	in out
	
-- Extra interessante Functies
maxInt = maxBound :: Int
minInt = minBound :: Int