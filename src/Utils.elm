module Utils exposing (lookup, lookupAndUpdate)


lookup : k -> List (k, a) -> Maybe a
lookup searchKey list =
  case list of
    [] -> Nothing

    (key, value) :: xs ->
      if key == searchKey
      then Just value
      else lookup searchKey xs

lookupAndUpdate : (a -> a) -> k -> List (k, a) -> List (k, a)
lookupAndUpdate f searchKey list =
  case list of
    [] -> []

    (key, value) :: xs ->
      if key == searchKey
      then (key, f value) :: xs
      else (key, value) :: lookupAndUpdate f searchKey xs