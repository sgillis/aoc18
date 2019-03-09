defmodule Zipper do
  require Record
  Record.defrecordp(:zipper, previous: [], next: [])

  def empty do
    zipper()
  end

  def insert(z = zipper(previous: prev), x) do
    zipper(z, previous: [x | prev])
  end

  def forward(zipper(previous: [], next: [])) do
    zipper(previous: [], next: [])
  end
  def forward(zipper(previous: prev, next: [])) do
    [x | rest] = Enum.reverse(prev)
    zipper(previous: [x], next: rest)
  end
  def forward(zipper(previous: prev, next: [x | rest])) do
    zipper(previous: [x | prev], next: rest)
  end

  def forward(zipper, steps) do
    List.foldl(Enum.to_list(1..steps), zipper, fn _n, z -> forward(z) end)
  end

  def backward(zipper(previous: [], next: [])) do
    zipper(previous: [], next: [])
  end
  def backward(zipper(previous: [], next: next)) do
    [x | rest] = Enum.reverse(next)
    zipper(previous: rest, next: [x])
  end
  def backward(zipper(previous: [x | rest], next: next)) do
    zipper(previous: rest, next: [x | next])
  end

  def backward(zipper, steps) do
    List.foldl(Enum.to_list(1..steps), zipper, fn _n, z -> backward(z) end)
  end

  def to_list(zipper(previous: prev, next: next)) do
    Enum.reverse(prev) ++ next
  end

  def from_list(xs) do
    zipper(previous: Enum.reverse(xs))
  end

  def pop(z = zipper(previous: [], next: [])) do
    {nil, z}
  end
  def pop(zipper(previous: [x], next: [])) do
    {x, zipper(previous: [], next: [])}
  end
  def pop(zipper(previous: prev, next: [])) do
    [x | next] = Enum.reverse(prev)
    {x, zipper(previous: [], next: next)}
  end
  def pop(zipper(previous: prev, next: [x | rest])) do
    {x, zipper(previous: prev, next: rest)}
  end
end
