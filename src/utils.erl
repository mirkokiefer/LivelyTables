-module(utils).
-export([encode/1]).


encode(String) ->
  unicode:characters_to_binary(io_lib:format("~ts", [String])).
