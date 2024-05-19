module Helper where


-- uncurry3 is a modified version of the uncurry built-in for packaging data into a tuple that works with 3-tuples
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
