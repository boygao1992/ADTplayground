module FortyNine where

import Prelude

-- solve :: Int -> Integer
-- solve 1 = 2
-- solve 2 = 3
-- solve 3 = 5
-- solve n = minimum $ filter (> solve (n-1))$ map solve [1 .. (n-1)] >>= (\x -> map (x *) [2, 3, 5])

solve :: Int -> [Integer] -> ([Integer], [Integer], [Integer]) -> [Integer]
solve 1 xs _ = xs
solve n xs (q2, q3, q5) = solve (n-1) (xs ++ [x]) update
  where
    x = minimum $ map head [q2, q3, q5]
    update
      | x == head q2 = ((tail q2) ++ [x*2], q3 ++ [x*3], q5 ++ [x*5])
      | x == head q3 = (q2, (tail q3) ++ [x*3], q5 ++ [x*5])
      | otherwise = (q2, q3, (tail q5) ++ [x*5])

solution :: Int -> Integer
solution n = last $ solve n [1] ([2], [3], [5])
