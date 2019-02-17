defmodule Day05 do
  require Integer

  def main do
    read_file() |> react |> String.length
    read_file() |> remove_react |> String.length
  end

  @doc """
  iex> "dabAcCaCBAcCcaDA" |> Day05.react
  "dabCBAcaDA"
  """
  def react(polymer) do
    polymer
    |> List.Chars.to_charlist
    |> Enum.reduce([], &reduce/2)
    |> Enum.reverse
    |> List.to_string
  end

  @doc """
  iex> "dabAcCaCBAcCcaDA" |> Day05.remove_react
  "daDA"
  """
  def remove_react(polymer) do
    ?a..?z
    |> Enum.reduce(%{}, fn c, acc ->
      Map.put(acc, c, remove_unit(polymer, c) |> react)
    end)
    |> Enum.min_by(fn {_, v} -> String.length(v) end)
    |> elem(1)
  end

  @doc """
  iex> "dabAcCaCBAcCcaDA" |> Day05.remove_unit('a')
  "dbcCCBcCcD"
  """
  def remove_unit(polymer, unit) do
    polymer
    |> String.replace(nocase(unit), "")
    |> String.replace(upcase(unit), "")
  end

  def reduce(i, []), do: [i]
  def reduce(i, [prev | tail] = acc) do
    case react(i, prev) do
      true -> tail
      false -> [i | acc]
    end
  end

  @doc """
  iex> Day05.react('a', 'A')
  true

  iex> Day05.react('A', 'a')
  true

  iex> Day05.react('a', 'a')
  false

  iex> Day05.react('a', 'B')
  false
  """
  def react(x, y) do
    ((upcase(x) == nocase(y)) and (upcase(y) != nocase(x))) or
    ((upcase(y) == nocase(x)) and (upcase(x) != nocase(y)))
  end

  def upcase(c), do: String.upcase(List.to_string([c]))
  def nocase(c), do: List.to_string([c])

  def read_file do
    {:ok, contents} = File.read "input.txt"
    String.trim contents
  end
end
