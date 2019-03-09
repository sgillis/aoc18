defmodule Day09 do
  require Record
  Record.defrecord(
    :state,
    turn: 1,
    marbles: [0],
    active_marble: 0,
    player: 1,
    total_players: nil,
    max_turns: nil,
    score: %{}
  )

  def first() do
    run_game(473, 70904, true)
    # 371284
  end

  def second() do
    # run_game(473, 7090400, true)
    :too_slow
  end

  @doc """
  iex> Day09.run_game(9, 25)
  32

  iex> Day09.run_game(10, 1618)
  8317

  iex> Day09.run_game(13, 7999)
  146373

  iex> Day09.run_game(17, 1104)
  2764

  iex> Day09.run_game(21, 6111)
  54718

  iex> Day09.run_game(30, 5807)
  37305
  """
  def run_game(players, steps, debug \\ false) do
    init_state(players, steps)
    |> step(steps, debug)
    |> state(:score)
    |> Map.values
    |> Enum.max
  end

  @doc """
  iex> Day09.init_state(9, 25) |> Day09.step(2)
  ...> |> Day09.attrs([:turn, :marbles, :active_marble, :player])
  [3, [0, 2, 1], 2, 3]

  iex> Day09.init_state(9, 25) |> Day09.step(5)
  ...> |> Day09.attrs([:turn, :marbles, :active_marble, :player])
  [6, [0, 4, 2, 5, 1, 3], 5, 6]

  iex> Day09.init_state(9, 25) |> Day09.step(25)
  ...> |> Day09.attrs([:turn, :active_marble, :player])
  [26, 25, 8]

  iex> Day09.init_state(9, 25) |> Day09.step(25)
  ...> |> Day09.state(:marbles)
  [0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 25, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15]

  iex> Day09.init_state(9, 25) |> Day09.step(25)
  ...> |> Day09.state(:score)
  %{1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 32, 6 => 0, 7 => 0, 8 => 0, 9 => 0}
  """
  def step(state, steps, debug \\ false) do
    List.foldl(
      Enum.to_list(1..steps),
      state,
      fn n, state ->
        case {debug, rem(n, 1000)} do
          {true, 0} -> IO.inspect {n, length(state(state, :marbles))}
          _ -> :ok
        end
        next_state(state)
      end
    )
  end

  @doc """
  iex> Day09.init_state(9, 25) |> Day09.next_state
  ...> |> Day09.attrs([:turn, :marbles, :active_marble, :player])
  [2, [0, 1], 1, 2]
  """
  def next_state(s = state(turn: turn)) do
    case special_turn(turn) do
      true -> update_special_turn(s)
      false -> update_normal_turn(s)
    end
  end

  def update_special_turn(s = state(marbles: marbles)) do
    {rem_marble, next_active_marble} =
      rule_23(marbles, state(s, :active_marble), 7)
    state(s,
      turn: next_turn(s),
      player: next_player(s),
      marbles: remove_marble(marbles, rem_marble),
      active_marble: next_active_marble,
      score: update_score(
        state(s, :score),
        state(s, :player),
        state(s, :turn),
        rem_marble)
    )
  end

  def update_normal_turn(s) do
    state(s,
      turn: next_turn(s),
      player: next_player(s),
      marbles: insert_marble(s),
      active_marble: state(s, :turn)
    )
  end

  def insert_marble(state(
        marbles: marbles,
        active_marble: active_marble,
        turn: turn
      )) do
    insert_after(
      marbles,
      next_marble(marbles, active_marble),
      turn
    )
  end

  @doc """
  iex> Day09.init_state(9, 25) |> Day09.state(:total_players)
  9

  # iex> Day09.init_state(9, 25) |> Day09.state(:score)
  # %{1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0, 9 => 0}
  """
  def init_state(total_players, max_turns) do
    score = for n <- 1..total_players, into: %{}, do: {n, 0}
    state(
      total_players: total_players,
      max_turns: max_turns,
      score: score
    )
  end

  @doc """
  iex> Day09.special_turn(0)
  false

  iex> Day09.special_turn(23)
  true

  iex> Day09.special_turn(46)
  true

  iex> Day09.special_turn(25)
  false
  """
  def special_turn(turn) do
    (turn != 0) && (rem(turn, 23) == 0)
  end


  ## ---------------------------------------------------------------------------
  ## Update state field function
  ##

  def next_turn(state(turn: turn)) do
    turn + 1
  end

  def next_player(state(player: player, total_players: players)) do
    next_player(player, players)
  end

  @doc """
  iex> Day09.next_player(1, 9)
  2

  iex> Day09.next_player(8, 9)
  9

  iex> Day09.next_player(9, 9)
  1
  """
  def next_player(player, players) do
    case player do
      ^players -> 1
      _ -> player + 1
    end
  end

  @doc """
  iex> Day09.next_marble([0], 0)
  0

  iex> Day09.next_marble([0, 1], 1)
  0

  iex> Day09.next_marble([0, 2, 1], 2)
  1

  iex> Day09.next_marble([0, 8, 4, 9, 2, 10, 5, 1, 6, 3, 7], 10)
  5
  """
  def next_marble(marbles, active_marble) do
    next_marble(marbles, active_marble, [])
  end

  def next_marble([active_marble | []], active_marble, []) do
    active_marble
  end
  def next_marble([active_marble | []], active_marble, prefix) do
    [next | _] = Enum.reverse(prefix)
    next
  end
  def next_marble([active_marble, next | _], active_marble, _) do
    next
  end
  def next_marble([head | rest], active_marble, prefix) do
    next_marble(rest, active_marble, [head | prefix])
  end

  @doc """
  iex> Day09.rule_23(
  ...>   [0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15],
  ...>   22,
  ...>   7)
  {9, 19}
  """
  def rule_23(marbles, current_marble, steps) do
    {m, prev_m, _} =
      List.foldl(
        Enum.to_list(1..steps),
        {current_marble, Enum.reverse(marbles)},
        fn
          _n, {marble, marbles} ->
            {next_marble(marbles, marble), marble, marbles}
          _n, {marble, _prev_marble, marbles} ->
            {next_marble(marbles, marble), marble, marbles}
        end)
    {m, prev_m}
  end

  @doc """
  iex> Day09.insert_after([0], 0, 1)
  [0, 1]

  iex> Day09.insert_after([0, 8, 4, 9, 2, 10, 5, 1, 6, 3, 7], 5, 11)
  [0, 8, 4, 9, 2, 10, 5, 11, 1, 6, 3, 7]
  """
  def insert_after(marbles, after_marble, new_insert) do
    insert_after(marbles, after_marble, new_insert, [])
  end

  def insert_after([after_marble | rest], after_marble, new_insert, prefix) do
    Enum.reverse(prefix) ++ [after_marble, new_insert | rest]
  end
  def insert_after([x | rest], after_marble, new_insert, prefix) do
    insert_after(rest, after_marble, new_insert, [x | prefix])
  end

  def remove_marble(marbles, rem_marble) do
    List.delete(marbles, rem_marble)
  end

  def update_score(score, player, turn, rem_marble) do
    Map.update!(score, player, fn x -> x + turn + rem_marble end)
  end

  ## ---------------------------------------------------------------------------
  ## Test functions
  ##

  def attrs(s, attrs) do
    for attr <- attrs, do: elem(List.keyfind(state(s), attr, 0), 1)
  end
end
