module Utilities (..) where

import List


longestZip : List a -> List a -> List a
longestZip xs ys =
  case ( xs, ys ) of
    ( [], y :: ys' ) ->
      y :: longestZip [] ys'

    ( x :: xs', [] ) ->
      x :: longestZip xs' []

    ( x :: xs', y :: ys' ) ->
      x :: y :: longestZip xs' ys'

    ( _, _ ) ->
      []
