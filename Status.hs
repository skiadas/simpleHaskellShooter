module Status (
   Status,
   start, over,
   isOver
) where

data Status = Ongoing | Over deriving (Eq, Show)

start = Ongoing
over = Over

isOver = (== Over)
