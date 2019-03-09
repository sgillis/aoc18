defmodule Day08 do
  require Record
  Record.defrecord(:tree_node, children: [], metadata: [])

  @doc """
  iex> Day08.first()
  41849
  """
  def first do
    Day08.tree |> get_metadata |> Enum.sum
  end

  @doc """
  iex> Day08.second()
  32487
  """
  def second do
    Day08.tree |> node_value
  end

  @doc """
  iex> Day08.node_value({:tree_node, [], [10, 11, 12]})
  33

  iex> Day08.test_data |> Day08.parse_root |> Day08.node_value
  66
  """
  def node_value({:tree_node, [], metadata}) do
    Enum.sum(metadata)
  end
  def node_value({:tree_node, children, metadata}) do
    Enum.map(metadata, fn i -> get_node_value_at(children, i) end)
    |> List.flatten
    |> Enum.sum
  end

  def get_node_value_at(_nodes, 0) do
    0
  end
  def get_node_value_at(nodes, i) do
    case Enum.at(nodes, i - 1) do
      nil -> 0
      node -> node_value(node)
    end
  end

  @doc """
  iex> Day08.test_data() |> Day08.parse_root |> Day08.get_metadata
  [10, 11, 12, 99, 2, 1, 1, 2]
  """
  def get_metadata({:tree_node, children, metadata}) do
    List.flatten([Enum.map(children, &get_metadata/1), metadata])
  end

  @doc """
  iex> [0, 1, 2] |> Day08.parse_root
  {:tree_node, [], [2]}

  iex> [1, 1, 0, 1, 3, 2] |> Day08.parse_root
  {:tree_node, [{:tree_node, [], [3]}], [2]}
  """
  def parse_root(root) do
    {tree, []} = parse_node(root)
    tree
  end

  def parse_node([nr_children, nr_metadata | data]) do
    {children, meta} = parse_children(data, nr_children)
    {metadata, rest} = parse_metadata(meta, nr_metadata)
    {tree_node(children: children, metadata: metadata), rest}
  end

  def parse_children(data, 0) do
    {[], data}
  end
  def parse_children(data, nr_children) do
    {node, rest} = parse_node(data)
    {other_children, rest} = parse_children(rest, nr_children-1)
    {[node | other_children], rest}
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

  def tree do
    read_file() |> parse |> parse_root
  end

  def data do
    read_file()
  end

  def read_file do
    {:ok, contents} = File.read "input.txt"
    String.trim contents
  end
end
