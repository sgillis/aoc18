defmodule ZipperGameTest do
  use ExUnit.Case
  import ZipperGame
  doctest ZipperGame

  test "run_game" do
    assert 32 == run_game(9, 25)
    assert 8317 == run_game(10, 1618)
    assert 146373 == run_game(13, 7999)
    assert 2764 == run_game(17, 1104)
    assert 54718 == run_game(21, 6111)
    assert 37305 == run_game(30, 5807)
  end

  test "steps" do
    s = ZipperGame.init_state(9,25)
    assert [0, 1] == Zipper.to_list(state(ZipperGame.step(s, 1), :marbles))
    assert [0, 2, 1] == Zipper.to_list(state(ZipperGame.step(s, 2), :marbles))
    assert [0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 10, 21, 5, 22, 11, 1, 12, 6,
            13, 3, 14, 7, 15] ==
      Zipper.to_list(state(ZipperGame.step(s, 24), :marbles))
  end
end
