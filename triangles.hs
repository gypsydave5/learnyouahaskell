triples = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a] ]

rightTriangles = [ (a,b,c) | c <- [1..10],
                             a <- [1..c],
                             b <- [1..a],
                             a^2 + b^2 == c^2]

rightTriangles' = [ (a,b,c) | c <- [1..10],
                              a <- [1..c],
                              b <- [1..a],
                              a^2 + b^2 == c^2,
                              a+b+c == 24]

rightAngled :: (Int, Int, Int) -> Bool
rightAngled (a, b, c) = a^2 + b^2 == c^2

sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

rightTriangles'' = [ t | t <- triples,
                         rightAngled t,
                         sumTriple t == 24]
