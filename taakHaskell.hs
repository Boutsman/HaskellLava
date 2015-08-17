--module Main where
import System.IO
import Control.Monad (liftM)
--import taakHaskell

transform :: [String] -> [String]
transform lines =
	--a is de regel met eerste data
	--b is de regel met de 2e data
	let (a:b:ys) = words (show lines)	--probleem: words heeft String nodig, niet [String]
	in a:[]

isEven x
	| mod x 2 == 0	= True
	| otherwise		= False

main :: IO ()
main = do
  n <- read `liftM` getLine :: IO Int -- Hier halen we eerst een regel op met getLine, en dat "liften" we naar een geheel getal met de read-functie. n is dus van het type Int.
  lss <- lines `liftM` getContents    -- Alles regels ophalen, lss is type [String]

  let cases = take (2*n) lss			  -- eerste n regels nemen, cases is type [String]
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