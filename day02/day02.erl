-module(day02).

-export([ main/0
        , repetitions/1
        , checksum/1
        , count_rep/2
        , similar/2
        , find_similar/1
        , test_input/0
        , test_input2/0
        ]).

main() ->
  Ids = parse_bin(read_file()),
  Reps = [repetitions(Id) || Id <- Ids],
  checksum(Reps),
  find_similar(Ids).

read_file() ->
  {ok, Bin} = file:read_file("input"),
  Bin.

parse_bin(Bin) ->
  String = binary:bin_to_list(Bin),
  string:split(string:trim(String, trailing, "\n"), "\n", all).

repetitions(Id) ->
  repetitions(Id, #{}).

repetitions([], Map) ->
  Map;
repetitions([Char | Rest], Map) ->
  NewMap = maps:update_with(Char, fun(X) -> X + 1 end, 1, Map),
  repetitions(Rest, NewMap).

checksum(Reps) ->
  Twos = count_rep(2, Reps),
  Threes = count_rep(3, Reps),
  Twos * Threes.

count_rep(X, Reps) ->
  Result = lists:filter(
             fun(Rep) ->
                 lists:member(X, maps:values(Rep))
             end, Reps),
  length(Result).

find_similar([Id | Ids]) ->
  case [X || X <- Ids, similar(Id, X)] of
    [] ->
      find_similar(Ids);
    Similars ->
      [Id | Similars]
  end.

similar(Id1, Id2) ->
  compare(Id1, Id2, 1).

compare(_, _, N) when N < 0 ->
  false;
compare([], [], _) ->
  true;
compare([X | Xs1], [X | Xs2], N) ->
  compare(Xs1, Xs2, N);
compare([_ | Xs1], [_ | Xs2], N) ->
  compare(Xs1, Xs2, N-1).


test_input() ->
  [ "abcdef"
  , "bababc"
  , "abbcde"
  , "abcccd"
  , "aabcdd"
  , "abcdee"
  , "ababab"
  ].

test_input2() ->
  [ "abcde"
  , "fghij"
  , "klmno"
  , "pqrst"
  , "fguij"
  , "axcye"
  , "wvxyz"
  ].
