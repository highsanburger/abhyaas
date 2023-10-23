-- 31. Determine whether a given integer number is prime.
-- λ> isPrime 7 ~~~> True


-- 32. Determine the greatest common divisor of two positive integer numbers.
-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] ~~~> [9,3,3]


-- 33. Determine whether two positive integer numbers are coprime.
-- λ> coprime 35 64 ~~~> True


-- 34. Calculate Euler's totient function phi(m).
-- λ> totient 10 ~~~> 4


-- 35. Determine the prime factors of a given positive integer.
-- λ> primeFactors 315 ~~~> [3, 3, 5, 7]


-- 36. Determine the prime factors and their multiplicities of a given positive integer.
-- λ> prime_factors_mult 315 ~~~> [(3,2),(5,1),(7,1)]


-- 37. Calculate Euler's totient function phi(m) (improved).
-- λ> primesR 10 20 ~~~> [11,13,17,19]


-- 38. Compare the two methods of calculating Euler's totient function.(no solution required)
-- Use the solutions of Problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency.
-- Try to calculate phi(10090) as an example.


-- 39. A list of prime numbers in a given range.
-- λ> primesR 10 20 ~~~> [11,13,17,19]


-- 40. Goldbach's conjecture.
-- λ> goldbach 28 ~~~> (5, 23)


-- 41.  A list of even numbers and their Goldbach compositions in a given range.
-- λ> goldbachList 9 20 ~~~> [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50 ~~~> [(73,919),(61,1321),(67,1789),(61,1867)]
