class Matrix a where
    size :: a -> (Int, Int)
    index :: a -> Int -> Int -> b

data Grid a = Empty
            | Row [a] (Grid a)
              deriving (Show)

instance Matrix (Grid a) where
    size Empty = (0, 0)
    size (Row list next) = (rows next, length list)
        where rows Empty = 1
              rows (Row _ next) = 1 + rows next

    index grid row column | row < 0 || column < 0 = error "huh?"
                          | row >= rows || column >= columns = error "huh?"
                          | otherwise = index' grid row
                          where (rows, columns) = size grid
                                index' (Row l next) row | row == 0 = l !! column
                                                        | otherwise = index' next (row - 1)
