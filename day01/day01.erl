-module(day01).

-export([ main/0
        , dupes/1
        ]).

main() ->
  Bin = read_file(),
  Input = parse_bin(Bin),
  calc(Input),
  dupes(Input).

read_file() ->
  {ok, Bin} = file:read_file("input"),
  Bin.

parse_bin(Bin) ->
  String = binary:bin_to_list(Bin),
  Chunks = string:split(String, "\n", all),
  lists:filtermap(fun parse_chunk/1, Chunks).

parse_chunk([]) ->
  false;
parse_chunk(Xs) ->
  {true, erlang:list_to_integer(Xs)}.

calc(Input) ->
  lists:sum(Input).

dupes(Input) ->
  StartFreq = 0,
  loop(Input, StartFreq, [StartFreq]).

loop([X | Xs], Freq, SeenFreqs) ->
  NewFreq = Freq + X,
  case lists:member(NewFreq, SeenFreqs) of
    true ->
      NewFreq;
    false ->
      loop(Xs ++ [X], NewFreq, SeenFreqs ++ [NewFreq])
  end.
