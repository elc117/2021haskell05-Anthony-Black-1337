-- Pratica 05 de Haskel
--Nome: Anthony Carlos Da Silva

--Q1

bmi :: Float -> Float -> String
bmi a b = 
        let x = a 
            y = b
            k = a*b ^ 2
         in if k <= 18.5 then "Você está abaixo do peso" 
                         else if k <= 25.0 then "Você é supostamente normal" 
                         else if k <= 30.0 then "Você está gordo!"
                         else "Você é uma baleia!"

--Q2 

bmi' :: Float -> Float -> String  
bmi' x y  
    | bmi <= 18.5 = "Você está abaixo do peso!"  
    | bmi <= 25.0 = "Você é supostamente normal!" 
    | bmi <= 30.0 = "Você está gordo!" 
    | otherwise   = "Você é uma baleia!" 
    where bmi = x / y ^ 2  
--Q3

 
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
      where digits = take 9 cpf
            dv1 = cpfDV digits [10,9..]
            dv2 = cpfDV (digits ++ [dv1]) [11,10..]
     
  
cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
              let expr = (sum $ zipWith (*) digits mults) `mod` 11
              in if expr < 2 then 0 else 11-expr

--Q4

andTable :: [(Bool, Bool, Bool)]
andTable = [(False,False,False),
  (False,True,False),
  (False,False,False),
  (False,True,False),
  (False,False,False),
  (True,False,False),
  (False,False,True),
  (True,False,False),
  (False,True,False)]

