import "adj" Adj

import "base" Data.Int (Int)
import "base" System.IO (print)

import Adj.Utils

example_maybe :: Maybe Int
example_maybe = Some 1

example_list :: List Int
example_list = FG . Some .: Construct 1 None

main = print example_list
