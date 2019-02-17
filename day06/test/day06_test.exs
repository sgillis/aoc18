defmodule Day06Test do
  use ExUnit.Case
  doctest Day06

  test "infinite anchors" do
    anchors = Day06.test_anchors()
    box = Day06.max_square(anchors)
    dsquare = Day06.dists_square(anchors)
    expected = Enum.sort [{1, 1}, {1, 6}, {8, 3}, {8, 9}]
    assert Day06.infinite_anchors(box, dsquare) == expected
  end
end
