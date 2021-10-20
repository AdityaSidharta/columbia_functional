aux [] cur res = 0
aux [x] cur res = 1
aux [x, y] cur res | x /= y = maximum [cur + 1, res, 1]
aux [x, y] cur res | x == y = maximum [cur + 2, res]
aux (x : y : xs) cur res | x == y = aux (y : xs) (cur + 1) (res)
aux (x : y : xs) cur res | x /= y = aux (y : xs) (0) (maximum [cur +1, res])
aux _ cur res = error "should not be here"