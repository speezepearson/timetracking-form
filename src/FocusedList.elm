module FocusedList exposing (..)

type alias FocusedList a =
  { left : List a
  , focus : a
  , right : List a
  }

updateFocus : (a -> a) -> FocusedList a -> FocusedList a
updateFocus f fl =
  { fl | focus = f fl.focus }

stepLeft : FocusedList a -> Maybe (FocusedList a)
stepLeft fl =
  case fl.left of
    [] -> Nothing
    x :: xs -> Just { fl | left = xs , focus = x , right = fl.focus :: fl.right }

stepRight : FocusedList a -> Maybe (FocusedList a)
stepRight fl =
  case fl.right of
    [] -> Nothing
    x :: xs -> Just { fl | right = xs , focus = x , left = fl.focus :: fl.left }

concatLeft : List a -> FocusedList a -> FocusedList a
concatLeft xs fl =
  { fl | left = fl.left ++ xs }

head : FocusedList a -> a
head {left, focus} =
  case left of
    [] -> focus
    x :: xs -> List.head (List.reverse xs) |> Maybe.withDefault x
