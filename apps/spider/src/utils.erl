-module(utils).

-export([is_string/1]).


is_string([])->
        true;
is_string([H|T]) when H>=$ ,H=<$~->
        is_string(T);
is_string(_)->
        false.


