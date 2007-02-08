%% @hidden
-module(test_agi).
-export([test/2]).

test(Req,C) ->
    io:format("test ~p~n",[Req]),
    R1=fast_agi:say_number(C,234),
    io:format("SAY Result ~p~n",[R1]),
    R2=fast_agi:stream_file(C,"enter-ext-of-person","1234"),
    io:format("SAY Result ~p~n",[R2]),
    R10=fast_agi:hangup(C),
    io:format("Hangup Result ~p~n",[R10]).
