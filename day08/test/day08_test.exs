defmodule Day08Test do
  use ExUnit.Case
  doctest Day08

  test "greets the world" do
    expected = { :tree_node,
                 [ { :tree_node,
                     [],
                     [10, 11, 12]
                   },
                   { :tree_node,
                     [ { :tree_node,
                         [],
                         [99]
                       }
                     ],
                     [2]
                   }
                 ],
                 [1, 1, 2] }
    assert Day08.test_data() |> Day08.parse_root == expected
  end
end
