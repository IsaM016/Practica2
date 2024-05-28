import Data.List (splitAt)
import System.Random

separar :: String -> (String, String, String)
divis :: Int -> [Int]
mocus :: [Int] -> String
esPar :: Int -> String
randomizar :: (Int,Int) -> IO Int

randomizar rango = do
    generacion <- newStdGen
    let (randNumber, _) = randomR rango generacion
    return randNumber



separar xs = 
    let (primero, faltante) = splitAt 3 xs
        (segundo, tercero) = splitAt 2 faltante
    in (primero, segundo, tercero)


divis n = [x | x <- [1..n], n mod x == 0]


mocus xs 
    | suma == last xs = "Engineering"
    | suma < last xs = "Humanities"
    | suma > last xs = "Administrative"
    where suma = sum (init xs)


esPar n
    | n mod 2 == 0 = "even"
    | otherwise = "odd"

main :: IO()
main = do
    a <- getLine
    let (n1, n2, n3) = separar a
    let n2int = read n2 :: Int
    let n3int = read n3 :: Int
    let (año, sem) = splitAt 2 n1
    let divisores = divis n2int
    putStrLn $ "20" ++ año ++ "-" ++ sem ++ " " ++ mocus divisores ++ " " ++ "num" ++ show n3int ++ " " ++ esPar n3int