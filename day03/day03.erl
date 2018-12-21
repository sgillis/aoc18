-module(day03).

-export([ main/0
        , expand/1
        , squares/1
        , find_overlap/2
        , test_square/1
        ]).

main() ->
  Squares = [expand(parse_line(Line)) || Line <- parse_bin(read_file())],
  %% length(maps:values(maps:filter(fun(_, V) -> V > 1 end, squares(Squares)))).
  SquareCounts = squares(Squares),
  find_overlap(Squares, SquareCounts).

read_file() ->
  {ok, Bin} = file:read_file("input"),
  Bin.

parse_bin(Bin) ->
  String = binary:bin_to_list(Bin),
  string:split(string:trim(String, trailing, "\n"), "\n", all).

parse_line(Line) ->
  [Id, Rest] = string:split(Line, " @ "),
  [Location, Size] = string:split(Rest, ": "),
  {parse_id(Id), parse_location(Location), parse_size(Size)}.

parse_id(Id) ->
  list_to_integer(string:trim(Id, leading, "#")).

parse_location(Location) ->
  [X, Y] = string:split(Location, ","),
  {list_to_integer(X), list_to_integer(Y)}.

parse_size(Size) ->
  [X, Y] = string:split(Size, "x"),
  {list_to_integer(X), list_to_integer(Y)}.

expand({Id, {LX, LY}, {SX, SY}}) ->
  { Id
  , lists:flatten(
      [ [{X, Y} || X <- lists:seq(LX, LX + SX - 1)]
        || Y <- lists:seq(LY, LY + SY - 1) ])
  }.

squares(Squares) ->
  lists:foldl(fun square_map/2, #{}, Squares).

find_overlap(Squares, SquareCounts) ->
  lists:filtermap(fun(Square) -> no_patch_overlaps(Square, SquareCounts) end, Squares).

no_patch_overlaps({Id, Xs}, SquareCounts) ->
  case lists:filter(fun(X) -> maps:get(X, SquareCounts) =:= 1 end, Xs) of
    Xs ->
      {true, Id};
    _ ->
      false
  end.

square_map({_, Locations}, Map) ->
  lists:foldl(
    fun(Loc, M) ->
        maps:update_with(Loc, fun(X) -> X + 1 end, 1, M)
    end,
    Map,
    Locations
   ).

test_square(1) ->
  expand(parse_line("#1 @ 1,3: 4x4"));
test_square(2) ->
  expand(parse_line("#2 @ 3,1: 4x4"));
test_square(3) ->
  expand(parse_line("#3 @ 5,5: 2x2")).
