module Utils exposing (lookup, update, delete)


lookup : k -> List (k, a) -> Maybe a
lookup searchKey list =
  case list of
    [] -> Nothing

    (key, value) :: xs ->
      if key == searchKey then
        Just value
      else
        lookup searchKey xs

update : k -> a -> List (k, a) -> List (k, a)
update searchKey newValue list =
  case list of
    [] -> []

    (key, value) :: xs ->
      if key == searchKey then
        (key, newValue) :: xs
      else
        (key, value) :: (update searchKey newValue xs)

delete : k -> List (k, a) -> List (k, a)
delete searchKey list =
  case list of
    [] -> []

    (key, value) :: xs ->
      if key == searchKey then
        xs
      else
        (key, value) :: (delete searchKey xs)
