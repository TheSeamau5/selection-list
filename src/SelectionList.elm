module SelectionList where
{-| Module defining a selection list data structure.

A selection list is a collection that keeps track of which item in
the collection is currently selected. This data structure is especially
useful when working with components such as tabs, pages, slideshows and
anything pagination related.

# Definition
@docs SelectionList

# Cycle through items
@docs next, previous, goto

# Query Selection List
@docs length, selectedIndex

# Common Operations
@docs map, map2, andMap, indexedMap, selectedMap, updateSelected, updateN

# Conversion
@docs fromList, toList
-}

import List


{-| Selection List type.
A selection list is composed of two lists and a single value representing
the currently selected value.
-}
type alias SelectionList a =
  { previous : List a
  , selected : a
  , next     : List a
  }

{-| Convert a selection list to a list
-}
toList : SelectionList a -> List a
toList list =
  List.reverse list.previous ++ [ list.selected ] ++ list.next


{-| Construct a selection list from a list and an initial value which is
considered to be selected.
-}
fromList : a -> List a -> SelectionList a
fromList selected next =
  SelectionList [] selected next

{-| Get the length of a selection list.
-}
length : SelectionList a -> Int
length list =
  List.length list.previous + 1 + List.length list.next

{-| Map a function over a selection list.
-}
map : (a -> b) -> SelectionList a -> SelectionList b
map f list =
  { list | previous = List.map f list.previous
         , selected = f list.selected
         , next     = List.map f list.next
  }

{-| Update only the selected item.
-}
updateSelected : (a -> a) -> SelectionList a -> SelectionList a
updateSelected f list =
  { list | selected = f list.selected }

{-| Update only the nth value of a selection list.
This is a no-op if the index is out of bounds.
-}
updateN : Int -> (a -> a) -> SelectionList a -> SelectionList a
updateN n f list =
  indexedMap (\index value -> if index == n then f value else value) list


{-| Map a function over two selection lists.

Be careful, this is a naive map over two selection lists. It is recomended
to use this function only on two lists whose selected item is the very first
item. This will avoid truncation problems due to the lengths of the previous
and next elements in each list not matching.
-}
map2 : (a -> b -> c) -> SelectionList a -> SelectionList b -> SelectionList c
map2 f listA listB =
  { listA | previous = List.map2 f listA.previous listB.previous
          , selected = f listA.selected listB.selected
          , next     = List.map2 f listA.next listB.next
  }

{-| Chain multiple `map` operations together.
-}
andMap : SelectionList (a -> b) -> SelectionList a -> SelectionList b
andMap =
  map2 (<|)

{-| Map a function over a selection list. The function provided will be
passed the index of each item it is called with.
-}
indexedMap : (Int -> a -> b) -> SelectionList a -> SelectionList b
indexedMap f list =
  let
      previousLength = List.length list.previous
      nextLength = List.length list.next
  in
      { list | previous = List.reverse (List.indexedMap f (List.reverse list.previous))
             , selected = f previousLength list.selected
             , next     = List.indexedMap (\index -> f (index + previousLength + 1)) list.next
      }



{-| Map a function over a selection list. The function provided will be
passed `True` only for the selected item. This allows one apply different
functions depending on whether the item is selected or not.
-}
selectedMap : (Bool -> a -> b) -> SelectionList a -> SelectionList b
selectedMap f list =
  { list | previous = List.map (f False) list.previous
         , selected = f True list.selected
         , next     = List.map (f False) list.next
  }

{-| Go to the next item in the selection list.
This is a no-op if the selected item is the last item.
-}
next : SelectionList a -> SelectionList a
next list =
  case list.next of
    [] ->
      list

    x :: xs ->
      { list | selected = x
             , previous = list.selected :: list.previous
             , next     = xs
      }


{-| Go to the previous item in the selection list.
This is a no-op if the selected item is the first item.
-}
previous : SelectionList a -> SelectionList a
previous list =
  case list.previous of
    [] ->
      list

    x :: xs ->
      { list | selected = x
             , previous = xs
             , next     = list.selected :: list.next
      }

{-| Get the selected index of the selected item.
-}
selectedIndex : SelectionList a -> Int
selectedIndex list =
  List.length list.previous

{-| Go to the nth item in the list.
If the index is out of bounds, it will go to the index closest to the given
index. In essence, if you provide an index that is too great,
it will go to the last index. Similarly if you provide an index that is
too small, it will go to the very first index.
-}
goto : Int -> SelectionList a -> SelectionList a
goto n list =
  let
      curIndex =
        selectedIndex list
  in
      if curIndex == n then
        list
      else if curIndex < n && List.length list.next > 0 then
        goto n (next list)
      else if curIndex > (max 0 n) then
        goto n (previous list)
      else
        list
