sumTriple :: (Integer, Integer, Integer) -> Integer
sumTriple (a,b,c) = a+b+c

rightTriangle = [(a,b,c) | c <- [1..100],
                           a <- [1..c],
                           b <- [1..a],
                           a^2 + b^2 == c^2]

rightTriangleSum x = [triple | triple <- rightTriangle,
                               sumTriple triple == x]