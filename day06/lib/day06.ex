defmodule Day06 do
  def main do
    first_answer =
      read_file()
      |> parse_coords
      |> largest_area
    second_answer =
      read_file()
      |> parse_coords
      |> close_region(10000)
  end

  @doc """
  iex> Day06.test_anchors |> Day06.close_region(32)
  16
  """
  def close_region(anchors, max_d) do
    total_dists_square(anchors)
    |> Enum.filter(fn {_p, d} -> d < max_d end)
    |> length
  end

  @doc """
  iex> Day06.test_anchors |> Day06.largest_area
  17
  """
  def largest_area(anchors) do
    box = Day06.max_square(anchors)
    dsquare = Day06.dists_square(anchors)
    possible_anchors =
      Enum.filter(anchors, &(&1 not in infinite_anchors(box, dsquare)))
    Enum.reduce(dsquare, %{}, fn {p, anchor}, acc ->
      if anchor in possible_anchors do
        Map.update(acc, anchor, [p], &([p | &1]))
      else
        acc
      end
    end)
    |> Enum.map(fn {_k,v} -> length(v) end)
    |> Enum.max
  end

  @doc """
  Find the anchors at the edges of the box, these will be infinite
  """
  def infinite_anchors(box, dsquare) do
    dsquare
      |> Enum.filter(&(is_edge(&1, box)))
      |> Enum.filter(fn {_, anchor} -> anchor != :none end)
      |> Enum.map(&(elem(&1, 1)))
      |> Enum.uniq
      |> Enum.sort
  end

  def is_edge({{min_x, _}, _}, {{min_x, _}, _}), do: true
  def is_edge({{max_x, _}, _}, {{_, max_x}, _}), do: true
  def is_edge({{_, min_y}, _}, {_, {min_y, _}}), do: true
  def is_edge({{_, max_y}, _}, {_, {_, max_y}}), do: true
  def is_edge(_, _), do: false

  def dists_square(anchors) do
    {{min_x, max_x}, {min_y, max_y}} = Day06.max_square(anchors)
    for x <- min_x..max_x do
      for y <- min_y..max_y do
        p = {x, y}
        {p, nearest_anchor(anchors, p)}
      end
    end |> List.flatten
  end

  def total_dists_square(anchors) do
    {{min_x, max_x}, {min_y, max_y}} = Day06.max_square(anchors)
    for x <- min_x..max_x do
      for y <- min_y..max_y do
        p = {x, y}
        {p, total_distance(anchors, p)}
      end
    end |> List.flatten
  end

  @doc """
  iex> Day06.test_anchors |> Day06.total_distance({4,3})
  30
  """
  def total_distance(anchors, p) do
    Enum.reduce(anchors, 0, fn anchor, acc ->
      dist = metro_distance(anchor, p)
      acc + dist
    end)
  end

  @doc """
  iex> Day06.test_data() |> Day06.parse_coords |> Day06.nearest_anchor({0, 0})
  {1,1}

  iex> Day06.test_data() |> Day06.parse_coords |> Day06.nearest_anchor({5, 0})
  :none

  iex> Day06.test_data() |> Day06.parse_coords |> Day06.nearest_anchor({4, 5})
  {5,5}
  """
  def nearest_anchor(anchors, p) do
    dists =
      anchors |> Enum.map(fn anchor -> {anchor, metro_distance(anchor, p)} end)
    {anchor, min_dist} =
      dists |> Enum.min_by(fn {_, x} -> x end)
    case Enum.filter(dists, fn {_, x} -> x == min_dist end) do
      [_] -> anchor
      _ -> :none
    end
  end

  def read_file do
    {:ok, contents} = File.read "input.txt"
    String.trim contents
  end

  @doc """
  iex> Day06.test_data |> Day06.parse_coords
  [{1,1}, {1,6}, {8,3}, {3,4}, {5,5}, {8,9}]
  """
  def parse_coords(entries) do
    entries
    |> String.split("\n")
    |> Enum.map(&parse_coord/1)
  end

  @doc """
  iex> Day06.parse_coord "1, 1"
  {1, 1}
  """
  def parse_coord(coord) do
    [x, y] = coord |> String.split(", ")
    {String.to_integer(x), String.to_integer(y)}
  end

  @doc """
  Calculate the max and min x and y coords in which we will calculate.

  iex> Day06.test_data |> Day06.parse_coords |> Day06.max_square
  {{0, 9}, {0, 10}}
  """
  def max_square(anchors) do
    get_x = &(elem(&1, 0))
    get_y = &(elem(&1, 1))
    { {elem(Enum.min_by(anchors, get_x), 0) - 1,
       elem(Enum.max_by(anchors, get_x), 0) + 1},
      {elem(Enum.min_by(anchors, get_y), 1) - 1,
       elem(Enum.max_by(anchors, get_y), 1) + 1}
    }
  end

  @doc """
  iex> Day06.metro_distance({0, 0}, {0, 1})
  1

  iex> Day06.metro_distance({0, 0}, {1, 1})
  2

  iex> Day06.metro_distance({1, 1}, {2, 3})
  3
  """
  def metro_distance({x1, y1}, {x2, y2}) do
    abs(x2 - x1) + abs(y2 - y1)
  end

  def test_data do
    """
    1, 1
    1, 6
    8, 3
    3, 4
    5, 5
    8, 9
    """ |> String.trim
  end

  def test_anchors do
    test_data() |> parse_coords
  end
end
