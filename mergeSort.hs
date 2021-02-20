--ghc 8.0.2

merge::[Int] -> [Int] -> [Int] -- СЛИЯНИЕ: на входе 2 int массива, на выходе 1 int массив
merge [] arr2 = arr2
merge arr1 [] = arr1
merge (x:arr1) (y:arr2) = if(x<y) -- x,y -  heads | arr1,arr2 - tails
                            then x: merge arr1 (y:arr2) 
                            else y: merge (x:arr1) arr2 
						
mergeSort :: [Int] -> [Int] -- НИСХОДЯЩИЙ: int массив на входе, int массив на выходе
mergeSort [x] = [x]
mergeSort arr = merge (mergeSort arr1) (mergeSort arr2)
    where len = length arr
          mid = len `div` 2
          (arr1,arr2) = splitAt mid arr -- получаем половинки массива

main = print(mergeSort [46,9,4,2,5,3,6,7,11,45,10,0,90,1])