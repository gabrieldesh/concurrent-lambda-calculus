module Utils exposing (lookup)


lookup : k -> List (k, a) -> Maybe a
lookup searchKey list =
  case list of
    [] -> Nothing

    (key, value) :: xs ->
      if key == searchKey
      then Just value
      else lookup searchKey xs