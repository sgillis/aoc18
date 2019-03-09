defmodule ZipperTest do
  use ExUnit.Case
  doctest Zipper

  test "insert" do
    z = Zipper.empty |> Zipper.insert(1) |> Zipper.insert(2) |> Zipper.to_list
    assert [1, 2] == z
  end

  test "backward" do
    z = Zipper.from_list([1, 2]) |> Zipper.backward(4) |> Zipper.insert(3)
    assert [3, 1, 2] == Zipper.to_list(z)
  end

  test "forward" do
    z = Zipper.from_list([1, 2, 3]) |> Zipper.forward(5) |> Zipper.insert(4)
    assert [1, 2, 4, 3] == Zipper.to_list(z)
  end

  test "pop" do
    {x, z} = Zipper.from_list([1, 2, 3, 4]) |> Zipper.backward(2) |> Zipper.pop
    assert 3 == x
    assert [1, 2, 4] == Zipper.to_list(z)
  end
end
