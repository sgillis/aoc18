defmodule Day04 do
  defmodule Timefold do
    defstruct [:on_shift, status: :awake, sleep: %{}]
  end

  def main() do
    data = TestData.data
    data = Day04.read_file
    lines = Day04.raw_lines data
    lines = for x <- lines, do: Day04.parse_line x
    lines = Enum.sort lines
    timefold = Day04.fold lines
    guard = Day04.sleepiest_guard timefold.sleep
    sleeptimes = Map.get timefold.sleep, guard
    {minute, _} = Day04.sleepiest_minute sleeptimes
    first_answer = guard * minute
    sleepiest_minutes =
      Enum.map(timefold.sleep, fn {k, v} -> {k, Day04.sleepiest_minute(v)} end)
    {id, {min, _}} = Enum.max_by(sleepiest_minutes, fn {id, {m, x}} -> x end)
    second_answer = id * min
  end

  def read_file do
    {:ok, contents} = File.read "input"
    String.trim contents
  end

  def raw_lines(string) do
    String.split(string, "\n")
  end

  def parse_line(line) do
    regex = Regex.compile!("\\[(.*) (.*)\\] (.*)")
    [_, date, time, action] = Regex.run regex, line
    [year, month, day] = String.split(date, "-")
    date = { String.to_integer(year),
             String.to_integer(month),
             String.to_integer(day) }
    [hour, minute] = String.split(time, ":")
    time = { String.to_integer(hour), String.to_integer(minute)}
    action = parse_action action
    {{date, time}, action}
  end

  def parse_action("wakes up"), do: :wakes_up
  def parse_action("falls asleep"), do: :falls_asleep
  def parse_action(action) do
    regex = Regex.compile!("Guard #(\\d*)")
    [_, id] = Regex.run(regex, action)
    {:begin_shift, String.to_integer(id)}
  end

  def fold(logs) do
    List.foldl(logs, %Day04.Timefold{}, &Day04.time_fold/2)
  end

  def time_fold({_datetime, {:begin_shift, id}}, timefold) do
    %{timefold | on_shift: id, status: :awake}
  end
  def time_fold({{_date, {_h, m}}, :falls_asleep}, timefold) do
    %{timefold | status: {:asleep, m}}
  end
  def time_fold({{_date, {_h, m}}, :wakes_up}, timefold) do
    {:asleep, sleep_start} = timefold.status
    sleeptime = {sleep_start, m - 1}
    id = timefold.on_shift
    sleepmap =
      Map.update(timefold.sleep, id, [sleeptime],
        fn times -> [sleeptime | times] end)
    %{timefold | status: :awake, sleep: sleepmap}
  end

  def sleepiest_guard(sleepmap) do
    sleeptimes = Enum.map(sleepmap, fn {k, v} -> {k, Day04.sleep_time(v)} end)
    Day04.max_sleep(sleeptimes, {0, 0})
  end

  def sleep_time(times) do
    Enum.sum(for {start, stop} <- times, do: stop - start)
  end

  def max_sleep([], {max_id, _}), do: max_id
  def max_sleep([{id, sleep} | rest], {max_id, max_sleep}) do
    case sleep > max_sleep do
      true -> max_sleep(rest, {id, sleep})
      false -> max_sleep(rest, {max_id, max_sleep})
    end
  end

  def sleepiest_minute(sleeptimes) do
    seqs = for {start, stop} <- sleeptimes, do: Enum.to_list start..stop
    seqs = List.flatten seqs
    seqmap = List.foldl(seqs, %{},
      fn x, acc ->
        Map.update(acc, x, 1, fn x -> x + 1 end)
      end)
    Enum.max_by(seqmap, fn {_, v} -> v end)
  end
end

defmodule TestData do
  def data do
    "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"
  end
end
