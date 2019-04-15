
-- prime
prime_set = sieve [2..]

sieve (p:ps) = p : sieve [x | x <- ps, mod x p /= 0]

-- even
even_set = [ 2 * x | x <- [1..]] 

-- odd
odd_set = [ 2 * x + 1 | x <- [0..]]



gp' :: Int -> Int -> Int -> (Int, Int)
gp' e i n = if num == e then (i, n) else
              if n >= e && i >= e then (0, 0) else
                if n >= e then gp' e (i + 1) 0 else
                  gp' e i (n + 1)
  where num = (prime_set !! i) + (prime_set !! n)

gp e = gp' e 0 0 

gp_primes e = (prime_set !! a, prime_set !! b)
  where (a, b) = gp e


p_pseq p = gp (p + 1)

m_pseq p = gp (p - 1)

pseq p = (p_pseq p, m_pseq p)

p_pseq_set = [ p_pseq i | i <- prime_set]
m_pseq_set = [ m_pseq i | i <- prime_set]
pseq_set = [ pseq i | i <- prime_set]


     
  
