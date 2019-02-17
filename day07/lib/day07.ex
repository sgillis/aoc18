defmodule Day07 do
  @doc """
  iex> Day07.first()
  "LAPFCRGHVZOTKWENBXIMSUDJQY"
  """
  def first do
    read_file()
    |> parse
    |> create_graph
    |> traverse_graph([])
    |> Enum.join("")
  end

  @doc """
  iex> Day07.read_file() |> Day07.parse |> Day07.create_graph |> Day07.init_worklog(5)
  ...> |> Day07.test_traverse_iterations(991)
  nil
  """
  def second do
    read_file()
    |> parse
    |> create_graph
    |> parallel_traverse_graph(5)
    |> Map.get(:workt)
  end

  @doc """
  iex> Day07.test_graph |> Day07.traverse_graph([])
  ["C", "A", "B", "D", "F", "E"]
  """
  def traverse_graph(graph, acc) do
    case Graph.num_vertices(graph) do
      0 -> Enum.reverse acc
      _ ->
        v = List.first available_steps(graph)
        traverse_graph(Graph.delete_vertex(graph, v), [v | acc])
    end
  end

  def parallel_traverse_graph(graph, nr_workers) when is_integer(nr_workers) do
    parallel_traverse_graph(init_worklog(graph, nr_workers))
  end

  @doc """
  iex> Day07.test_graph
  ...> |> Day07.test_worklog(2)
  ...> |> Day07.parallel_traverse_graph()
  ...> |> Map.get(:workq)
  ["C", "A", "F", "B", "D", "E"]

  iex> Day07.test_graph
  ...> |> Day07.test_worklog(2)
  ...> |> Day07.parallel_traverse_graph()
  ...> |> Map.get(:workt)
  15
  """
  def parallel_traverse_graph(acc) do
    case graph_done(acc) do
      true -> acc
      false ->
        parallel_traverse_iteration(acc)
        |> parallel_traverse_graph
    end
  end

  @doc """
  iex> Day07.test_graph
  ...> |> Day07.test_worklog(2)
  ...> |> Day07.test_traverse_iterations(3)
  ...> |> Map.get(:in_progress)
  []

  iex> Day07.test_graph
  ...> |> Day07.test_worklog(2)
  ...> |> Day07.test_traverse_iterations(4)
  ...> |> Map.get(:in_progress)
  [{"F", 5}]
  """
  def parallel_traverse_iteration(acc) do
    grab_work(acc)
    |> step_time
  end

  @doc """
  iex> Day07.test_graph
  ...> |> Day07.test_worklog(2)
  ...> |> Day07.test_traverse_iterations(3)
  ...> |> Day07.graph_done
  false

  iex> Day07.test_graph
  ...> |> Day07.test_worklog(2)
  ...> |> Day07.test_traverse_iterations(15)
  ...> |> Day07.graph_done
  true
  """
  def graph_done(acc) do
    acc.in_progress == [] and
    Graph.vertices(acc.graph) == []
  end

  @doc """
  iex> Day07.parallel_test_graph
  ...> |> Day07.test_worklog(2) |> Day07.grab_work |> Map.get(:available_workers)
  0

  iex> Day07.parallel_test_graph
  ...> |> Day07.test_worklog(4) |> Day07.grab_work |> Map.get(:available_workers)
  0

  iex> Day07.parallel_test_graph
  ...> |> Day07.test_worklog(5) |> Day07.grab_work |> Map.get(:available_workers)
  1

  iex> Day07.parallel_test_graph
  ...> |> Day07.test_worklog(2) |> Day07.grab_work |> Map.get(:in_progress)
  [{"A", 1}, {"B", 2}]

  iex> Day07.parallel_test_graph
  ...> |> Day07.test_worklog(2) |> Day07.grab_work |> Map.get(:workq)
  ["A", "B"]
  """
  def grab_work(acc) do
    steps =
      Enum.take(available_steps(acc.graph), acc.available_workers)
      |> Enum.filter(fn step -> not step_in_progress(acc, step) end)
      |> Enum.sort
    work = Enum.map(steps, fn step -> {step, acc.work_time.(step)} end)
    %{acc |
      workq: acc.workq ++ steps,
      available_workers: acc.available_workers - length(steps),
      in_progress: acc.in_progress ++ work}
  end

  def step_in_progress(acc, step) do
    Enum.member?(:proplists.get_keys(acc.in_progress), step)
  end

  @doc """
  iex> Day07.parallel_test_graph
  ...> |> Day07.test_worklog(2) |> Day07.grab_work |> Day07.step_time
  ...> |> Map.get(:in_progress)
  [{"B", 1}]
  """
  def step_time(acc) do
    {in_progress, finished} =
      List.foldr(
        acc.in_progress,
        {[], []},
        fn {step, time_left}, {progress_acc, finished_acc} ->
          case time_left do
            1 -> {progress_acc, [step | finished_acc]}
            _ -> {[{step, time_left - 1} | progress_acc], finished_acc}
          end
        end
      )
    %{acc |
      workt: acc.workt + 1,
      available_workers: acc.available_workers + length(finished),
      graph: Graph.delete_vertices(acc.graph, finished),
      in_progress: in_progress}
  end

  def init_worklog(graph, nr_workers) do
    %{workq: [],
      workt: 0,
      available_workers: nr_workers,
      in_progress: [],
      graph: graph,
      work_time: &Day07.work_time/1}
  end

  def set_test_work_time(acc) do
    %{acc | work_time: &Day07.test_work_time/1}
  end

  def test_worklog(graph, nr_workers) do
    init_worklog(graph, nr_workers)
    |> set_test_work_time
  end

  @doc """
  Test function to simulate n time steps in the graph
  """
  def test_traverse_iterations(worklog, n) do
    List.foldl(
      Enum.to_list(1..n),
      worklog,
      fn _, acc -> parallel_traverse_iteration(acc) end)
  end

  def read_file do
    {:ok, contents} = File.read "input.txt"
    String.trim contents
  end

  def parse(entries) do
    entries
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&Day07.parse_dep/1)
  end

  def parse_dep(input) do
    "Step "
    <> <<first::bytes-size(1)>> <> " must be finished before step "
    <> <<last::bytes-size(1)>> <> " can begin." = input
    {first, last}
  end

  def create_graph(edges) do
    Graph.new(type: :directed)
    |> Graph.add_edges(edges)
  end

  @doc """
  iex> Day07.test_graph |> Day07.available_steps
  ["C"]

  iex> Day07.test_graph |> Graph.delete_vertex("C") |> Day07.available_steps
  ["A", "F"]
  """
  def available_steps(graph) do
    Graph.vertices(graph)
    |> Enum.filter(fn vertex -> Graph.in_degree(graph, vertex) == 0 end)
    |> Enum.sort
  end

  @doc """
  iex> Enum.map(["A", "Z"], &Day07.work_time/1)
  [61, 86]
  """
  def work_time(step) do
    number = String.to_charlist(step) |> List.first
    61 + number - ?A
  end

  def test_work_time(step) do
    number = String.to_charlist(step) |> List.first
    1 + number - ?A
  end

  @doc """
  iex> Day07.test_data() |> Day07.parse
  [{"C", "A"}, {"C", "F"}, {"A", "B"}, {"A", "D"}, {"B", "E"}, {"D", "E"}, {"F", "E"}]

  iex> Day07.test_graph() |> Graph.vertices |> Enum.sort
  ["A", "B", "C", "D", "E", "F"]
  """
  def test_data do
    "Step C must be finished before step A can begin.
    Step C must be finished before step F can begin.
    Step A must be finished before step B can begin.
    Step A must be finished before step D can begin.
    Step B must be finished before step E can begin.
    Step D must be finished before step E can begin.
    Step F must be finished before step E can begin."
  end

  def test_graph do
    Day07.test_data() |> Day07.parse |> Day07.create_graph
  end

  def parallel_test_graph do
    Graph.new()
    |> Graph.add_edges([{"A", "E"}, {"B", "E"}, {"C", "E"}, {"D", "E"}])
  end
end
