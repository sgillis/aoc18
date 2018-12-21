-module(day04).

-export([ main/0
        , main/1
        , read_file/0
        , test_data/0
        , fold/1
        ]).

-record(timefold, { on_shift = undefined
                  , status = awake
                  , sleep = #{}
                  }).

main() ->
  main(live).

main(Mode) ->
  Data = case Mode of
           test -> test_data();
           live -> read_file()
         end,
  Logs = lists:sort([time_string_line(Line) || Line <- raw_lines(Data)]),
  Timefold = fold(Logs),
  SleepiestGuard = sleepiest_guard(Timefold#timefold.sleep),
  maps:get(SleepiestGuard, Timefold#timefold.sleep).

read_file() ->
  {ok, Bin} = file:read_file("input"),
  binary:bin_to_list(Bin).

raw_lines(String) ->
  string:split(string:trim(String, trailing, "\n"), "\n", all).

time_string_line([ $[ | Rest ]) ->
  time_string_line(Rest, {datetime, []}).

time_string_line([ $], $\s | Rest ], {datetime, DateTime}) ->
  {parse_datetime(lists:reverse(DateTime)), parse_action(Rest)};
time_string_line([ X | Rest ], {datetime, DateTime}) ->
  time_string_line(Rest, {datetime, [X | DateTime]}).

parse_datetime(DateTime) ->
  [Date, Time] = string:split(DateTime, "\s"),
  [Year, Month, Day] = string:split(Date, "-", all),
  [Hour, Minute] = string:split(Time, ":"),
  { {list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)}
  , {list_to_integer(Hour), list_to_integer(Minute)}
  }.

parse_action("wakes up") ->
  wakes_up;
parse_action("falls asleep") ->
  falls_asleep;
parse_action(X) ->
  Id = string:trim(string:trim(X, leading, "Guard #"), trailing, " begins shift"),
  {begin_shift, list_to_integer(Id)}.

fold(Logs) ->
  lists:foldl(fun time_fold/2, #timefold{}, Logs).

time_fold({_Datetime, {begin_shift, Id}}, Timefold) ->
  Timefold#timefold{ on_shift = Id
                   , status = awake
                   };
time_fold({{_Date, {_H, M}}, falls_asleep}, Timefold) ->
  Timefold#timefold{ status = {asleep, M} };
time_fold({{_Date, {_H, M}}, wakes_up}, Timefold) ->
  {asleep, SleepStart} = Timefold#timefold.status,
  SleepTime = {SleepStart, M},
  Id = Timefold#timefold.on_shift,
  SleepMap =
    maps:update_with(
      Id,
      fun(Times) -> [SleepTime | Times] end,
      [SleepTime],
      Timefold#timefold.sleep
     ),
  Timefold#timefold{ status = awake
                   , sleep = SleepMap
                   }.

%% Sleepmap :: #{Id => [{Start, End} | _]}
sleepiest_guard(Sleepmap) ->
  SleepTimes = maps:map(fun(_K, V) -> sleep_time(V) end, Sleepmap),
  max_sleep(maps:to_list(SleepTimes), {0, 0}).

%% Times :: [{Start, End} | _]
sleep_time(Times) ->
  lists:sum(lists:map(fun({Start, End}) -> End - Start end, Times)).

max_sleep([], {MaxId, _MaxSleep}) ->
  MaxId;
max_sleep([{Id, Sleep} | Rest], {MaxId, MaxSleep}) ->
  case Sleep > MaxSleep of
    true -> max_sleep(Rest, {Id, Sleep});
    false -> max_sleep(Rest, {MaxId, MaxSleep})
  end.

%% Times :: [{Start, End} | _]
sleepiest_minute(Times) ->
  Minutes = lists:seq(0, 59).

test_data() ->
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
[1518-11-05 00:55] wakes up".
