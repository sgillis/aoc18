defmodule Day08 do
  require Record
  Record.defrecord(:tree_node, children: [], metadata: [])

  @doc """
  iex> [0, 1, 2] |> Day08.parse_node
  {{:tree_node, [], [2]}, []}

  iex> [1, 1, 0, 1, 3, 2] |> Day08.parse_node
  {{:tree_node, [{:tree_node, [], [3]}], [2]}, []}

  # iex> Day08.test_data()
  # ...> |> Day08.parse_node
  # {:tree_node, [], []}
  """
  def parse_node([nr_children, nr_metadata | data]) do
    {children, meta} = parse_children(data, nr_children)
    {metadata, rest} = parse_metadata(meta, nr_metadata)
    {tree_node(children: children, metadata: metadata), rest}
  end

  def recursive_parse_nodes(data, 0) do
    parse_node(data)
  end
  def recursive_parse_nodes(data, nr_children) do
    {node, rest} = parse_node(data)
    {other_children, rest} = recursive_parse_nodes(rest, nr_children - 1)
    {[node | other_children], rest}
  end

  def parse_children(data, 0) do
    {[], data}
  end
  def parse_children(data, nr_children) do
  end

  def parse_metadata(data, nr_metadata) do
    Enum.split(data, nr_metadata)
  end

  def parse(input) do
    String.split(input, " ")
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(fn ({x, _}) -> x end)
  end

  def test_data do
    raw_test_data() |> parse
  end

  def raw_test_data do
    "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  end
end
