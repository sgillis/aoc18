defmodule ZipperGame do
  require Record
  Record.defrecord(
    :state,
    turn: 1,
    marbles: Zipper.empty |> Zipper.insert(0),
    player: 1,
    total_players: nil,
    max_turns: nil,
    score: %{}
  )
  def first() do
    run_game(473, 70904)
    # 371284
  end

  def second() do
    run_game(473, 7090400)
    # 3038972494
  end

  def run_game(players, steps) do
    init_state(players, steps)
    |> step(steps)
    |> state(:score)
    |> Map.values
    |> Enum.max
  end

  def step(state, steps) do
    List.foldl(
      Enum.to_list(1..steps),
      state,
      fn _n, state -> next_state(state) end
    )
  end

  def next_state(s = state(turn: turn)) do
    case special_turn(turn) do
      true -> update_special_turn(s)
      false -> update_normal_turn(s)
    end
  end

  def update_special_turn(s = state(marbles: marbles)) do
    {rem_marble, marbles} = rule_23(marbles)
    state(s,
      turn: next_turn(s),
      player: next_player(s),
      marbles: marbles,
      score: update_score(
        state(s, :score),
        state(s, :player),
        state(s, :turn),
        rem_marble)
    )
  end

  def update_normal_turn(s = state(marbles: marbles, turn: turn)) do
    state(s,
      turn: next_turn(s),
      player: next_player(s),
      marbles: normal_rule(marbles, turn)
    )
  end

  def init_state(total_players, max_turns) do
    score = for n <- 1..total_players, into: %{}, do: {n, 0}
    state(
      total_players: total_players,
      max_turns: max_turns,
      score: score
    )
  end

  def special_turn(turn) do
    (turn != 0) && (rem(turn, 23) == 0)
  end

  def next_turn(state(turn: turn)) do
    turn + 1
  end

  def next_player(state(player: player, total_players: players)) do
    next_player(player, players)
  end

  def next_player(player, players) do
    case player do
      ^players -> 1
      _ -> player + 1
    end
  end

  def update_score(score, player, turn, rem_marble) do
    Map.update!(score, player, fn x -> x + turn + rem_marble end)
  end

  def rule_23(marbles) do
    {x, marbles} = marbles |> Zipper.backward(8) |> Zipper.pop
    {x, marbles |> Zipper.forward}
  end

  def normal_rule(marbles, x) do
    marbles |> Zipper.forward |> Zipper.insert(x)
  end
end
