%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%%-------------------------------------------------------------------
%%% File    : fast_agi.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2006 Anders Nygren
%%% @version {@vsn}
%%% @doc Asterisk fast_agi server. 
%%% @end.
%%% Created : 24 Feb 2006 
%%%-------------------------------------------------------------------
%% 
%% 

-module(fast_agi).

%% API

-export([get_var/2,
	 trace/1,

	 answer/1,
	 channel_status/1,
	 channel_status/2,
	 db_del/3,
	 db_del_tree/2,
	 db_del_tree/3,
	 db_get/3,
	 db_put/4,
	 exec/3,
	 get_data/4,
	 get_full_variable/2,
	 get_full_variable/3,
	 get_option/3,
	 get_option/4,
	 get_variable/2,
	 hangup/1,
	 hangup/2,
	 noop/1,
	 noop/2,
	 receive_char/2,
	 record_file/5,
	 say_alpha/3,
	 say_date/3,
	 say_datetime/3,
	 say_digits/3,
	 say_number/2,
	 say_number/3,
	 say_phonetic/3,
	 say_time/3,
	 send_image/2,
	 send_text/2,
	 set_autohangup/2,
	 set_callerid/2,
	 set_context/2,
	 set_extension/2,
	 set_music_on/2,
	 set_music_on/3,
	 set_priority/2,
	 set_variable/3,
	 stream_file/2,
	 stream_file/3,
	 tdd_mode/2,
	 verbose/3,
	 wait_for_digit/1,
	 wait_for_digit/2]).

%%====================================================================
%% API
%%====================================================================

%% @spec answer(C) -> Result
%%    C      = Connection
%%    Result = ok | {error, Reason}
%%
%% @doc Answers the channel (if it is not already in an answered state).
%% @end
answer(C) ->
    case fast_agi_socket:send(C,"ANSWER\n") of
	{ok,"0"} ->
	    ok;
	{ok,"-1"} ->
	    {error,-1};
	Error ->
	    Error
    end.

%% @spec channel_status(C) -> Result
%%    C      = Connection
%%    Result = error | {ok,integer()}
%%
%% @doc Get the status of the current channel.
%% @end
channel_status(C)->
    case fast_agi_socket:send(C,"CHANNEL STATUS\n") of
	{ok,"-1"} ->
	    error;
	{ok,S} ->
	    {ok, list_to_integer(S)};
	Error ->
	    Error
    end.

%% @spec channel_status(C,Channel) -> Result
%%    C      = Connection
%%    Channel= String
%%    Result = integer
%%
%% @doc Get the status of the specified channel.
%% @end
channel_status(C,Channel)->
    case fast_agi_socket:send(C,["CHANNEL STATUS ",Channel,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,S} ->
	    {ok, list_to_integer(S)};
	Error ->
	    Error
    end.

%% @spec db_del(C,Family,Key) -> Result
%%    C      = Connection
%%    Family = string
%%    Key    = string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Delete the entry from the Asterisk DB given by Family and Key.
%% @end
db_del(C,Family,Key) ->
    case fast_agi_socket:send(C,["DATABASE DEL ",Family," ",Key,"\n"]) of
	{ok,"0"} ->
	    error;
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec db_del_tree(C,Family) -> Result
%%    C      = Connection
%%    Family = string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Deletes the Family from the Asterisk DB.
%% @end
db_del_tree(C,Family) ->
    case fast_agi_socket:send(C,["DATABASE DELTREE ",Family,"\n"]) of
	{ok,"0"} ->
	    error;
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec db_del_tree(C,Family,Keytree) -> Result
%%    C      = Connection
%%    Family = string
%%    Keytree= string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Deletes the Family from the Asterisk DB.
%% @end
db_del_tree(C,Family,Keytree) ->
    case fast_agi_socket:send(C,["DATABASE DELTREE ",Family," ",Keytree,"\n"]) of
	{ok,"0"} ->
	    error;
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec db_get(C,Family,Key)-> Result
%%    C      = Connection
%%    Family = string
%%    Key    = string
%%    Result = notset | {value,Value}
%%
%% @doc Get the value from the Asterisk DB.
%% @end
db_get(C,Family,Key)->
    case fast_agi_socket:send(C,["DATABASE GET ",Family," ",Key,"\n"]) of
	{ok,"0"} ->
	    error;
	{ok,"1 ("++Val} ->
	    {value,string:strip(Val,right,$))};
	Error ->
	    Error
    end.

%% @spec db_put(C,Family,Key,Value) -> Result
%%    C      = Connection
%%    Family = string
%%    Key    = string
%%    Value  = string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Adds or updates an entry in the Asterisk database for the 
%%      specified family and key, with the specified value.
%% @end
db_put(C,Family,Key,Value) ->
    case fast_agi_socket:send(C,["DATABASE PUT ",Family," ",Key," ",Value,"\n"]) of
	{ok,"0"} ->
	    error;
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec exec(C,Appl,Opts) -> Result
%%    C      = Connection
%%    Appl   = string
%%    Opts   = string
%%    Result = not_found|{result,Retval}
%%    Retval = string
%%
%% @doc Execute a dialplan application.
%% @end
exec(C,Appl,Opts) ->
    fast_agi_socket:send(C,["EXEC ",Appl," ",Opts,"\n"]).

%% @spec get_data(C,File,Timeout,MaxDigits) -> Result
%%    C      = Connection
%%    File   = string
%%    Timeout= integer
%%    MaxDigits=integer
%%    Result = {ok, timeout} | {ok, string()} | {ok,string(),timeout}
%%
%% @doc Play the audio file File and accept up to MaxDigits DTMF digits.
%% @end
get_data(C,File,Timeout,MaxDigits) ->
    case fast_agi_socket:send(C,["GET DATA ",File," ",integer_to_list(Timeout)," ",
				 integer_to_list(MaxDigits),"\n"]) of
	{ok," (timeout)"} ->
	    {ok,timeout};
	{ok,S} ->
	    case string:tokens(S," ") of
		[Str] ->
		    {ok,Str};
		[Str,"(timeout)"] ->
		    {ok,Str,timeout}
	    end;
	Error ->
	    Error
    end.

%% @spec get_full_variable(C,Var) -> Result
%%    C      = Connection
%%    Var    = string
%%    Result = error|{value,Value}
%%    Value  = string
%%
%% @doc Get the value of Var.
%% @end
get_full_variable(C,Var) ->
    case fast_agi_socket:send(C,["GET FULL VARIABLE ",Var,"\n"]) of
	{ok,"0"} ->
	    {error,var_not_set};
	{ok,"1 ("++Val} ->
	    {value,string:strip(Val,right,$))};
	Error ->
	    Error
    end.

%% @spec get_full_variable(C,Var,Channel) -> Result
%%    C      = Connection
%%    Var    = string
%%    Chnnel = string
%%    Result = error|{value,Value}
%%    Value  = string
%%
%% @doc Get the value of Var.
%% @end
get_full_variable(C,Var,Channel) ->
    case fast_agi_socket:send(C,["GET FULL VARIABLE ",Var," ",Channel,"\n"]) of
	{ok,"0"} ->
	    {error,var_not_set};
	{ok,"1 ("++Val} ->
	    {value,string:strip(Val,right,$))};
	Error ->
	    Error
    end.

%% @spec get_option(C,File,Escape) -> Result
%%    C      = Connection
%%    Result = digits
%%
%% @doc Same as stream_file.
%% @end
get_option(C,File,Escape) ->
    get_option(C,File,Escape,"").

%% @spec get_option(C,File,Escape,Timeout) -> Result
%%    C      = Connection
%%    Result = {ok,timeout} | {ok,Digit::string(),EPos::integer()} | error 
%%
%% @doc Same as stream_file but with a timeout in seconds.
%% @end
get_option(C,File,Escape,Timeout) when is_integer(Timeout) ->
    get_option(C,File,Escape,integer_to_list(Timeout));
get_option(C,File,Escape,Timeout) ->
    case fast_agi_socket:send(C,["GET OPTION ",File," ",Escape," ",Timeout,"\n"]) of
	{ok,"0 endpos=0"} ->
	    error;
	{ok,"0 endpos="++_EPos} ->
	    {ok,timeout};
	{ok,S} ->    % S = "NN endpos=nnn"
	    [D,M]=string:tokens(S," "),
	    [_,EP]=string:tokens(M,"="),
	    {ok,D,list_to_integer(EP)};
	Error ->
	    Error
    end.

%% @spec get_variable(C,Var) -> Result
%%    C      = Connection
%%    Var    = string
%%    Result = error|{value,Value}
%%
%% @doc Get the value of Var.
%% @end
get_variable(C,Var) ->
    case fast_agi_socket:send(C,["GET VARIABLE ",Var,"\n"]) of
	{ok,"0"} ->
	    {error,var_not_set};
	{ok,"1 ("++Val} ->
	    {value,string:strip(Val,right,$))};
	Error ->
	    Error
    end.

%% @spec hangup(C) -> Result
%%    C      = Connection
%%    Result = ok | {error, nochannel}
%%
%% @doc Hangup the current channel.
%% @end
hangup(C) ->
    case fast_agi_socket:send(C,"HANGUP\n") of
	{ok,"-1"} ->
	   {error,no_channel};
	{ok,"1"} ->
	    ok
    end.

%% @spec hangup(C,Channel) -> Result
%%    C      = Connection
%%    Channel= string
%%    Result = ok | {error, nochannel}
%%
%% @doc Hangup the specified channel.
%% @end
hangup(C,Channel) ->
    case fast_agi_socket:send(C,["HANGUP ",Channel,"\n"]) of
	{ok,"-1"} ->
	   {error,no_channel};
	{ok,"1"} ->
	    ok
    end.

%% @spec noop(C) -> Result
%%    C      = Connection
%%    Result = ok
%%
%% @doc Does nothing
%% @end
noop(C) ->
    case fast_agi_socket:send(C,"NOOP\n") of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec noop(C,Text) -> Result
%%    C      = Connection
%%    Text   = string
%%    Result = ok
%%
%% @doc Does nothing, but prints Text on the Asterisk console.
%% @end
noop(C,Text) ->
    case fast_agi_socket:send(C,["NOOP ",Text,"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec receive_char(C,Timeout) -> Result
%%    C      = Connection
%%    Timeout=integer
%%    Result = what
%%
%% @doc Receive a character on the current channel.
%% @end
receive_char(C,Timeout) ->
    fast_agi_socket:send(C,["RECEIVE CHAR ",integer_to_list(Timeout),"\n"]).

%% @spec record_file(C,File,Format,Escape,Timeout) -> Result
%%    C      = Connection
%%    File   = string
%%    Format = string
%%    Escape = string
%%    Timeout= integer
%%    Result = what
%%
%% @doc Record audio.
%% @end
record_file(C,File,Format,Escape,Timeout) ->
    case fast_agi_socket:send(C,["RECORD FILE ",File," ",Format," ",Escape," ",
				 integer_to_list(Timeout),"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,"0"} ->
	    ok
    end.

%% @spec say_alpha(C,Number,Escape) -> Result
%%    C      = Connection
%%    Number = integer
%%    Escape = string
%%    Result = what
%%
%% @doc 
%% @end
say_alpha(C,Number,Escape) ->
    case fast_agi_socket:send(C,["SAY ALPHA ",integer_to_list(Number)," ",
				 Escape,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,_Key}=Res ->
	    Res
    end.

%% @spec say_date(C,Date,Escape) -> Result
%%    C      = Connection
%%    Date   = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Says the given date. The Date is given as a UNIX time, i.e. number of secunds since 00:00:00 on January 1, 1970.
%% @end
say_date(C,Date,Escape) ->
    case fast_agi_socket:send(C,["SAY DATE ",integer_to_list(Date)," ",
				 Escape,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,_Key}=Res ->
	    Res
    end.

%% @spec say_datetime(C,Datetime,Escape) -> Result
%%    C      = Connection
%%    Datetime= integer
%%    Escape = string
%%    Result = what
%%
%% @doc Says the given datetime. The Datetime is given as a UNIX time, i.e. 
%%      number of secunds since 00:00:00 on January 1, 1970.
%% @end
say_datetime(C,Datetime,Escape) ->
    case fast_agi_socket:send(C,["SAY DATETIME ",integer_to_list(Datetime)," ",
				 Escape,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,_Key}=Res ->
	    Res
    end.

%% @spec say_digits(C,Number,Escape) -> Result
%%    C      = Connection
%%    Number = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Say digits.
%% @end
say_digits(C,Number,Escape) ->
    case fast_agi_socket:send(C,["SAY DIGITS ",integer_to_list(Number)," ",
				 Escape,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,_Key}=Res ->
	    Res
    end.

%% @spec say_number(C,Number) -> Result
%%    C      = Connection
%%    Number = integer
%%    Result = ok | error | {error, Reason}
%%
%% @doc Say number.
%% @end
say_number(C,Number) ->
    say_number(C,Number,"\"\"").
%%     case fast_agi_socket:send(C,["SAY NUMBER ",integer_to_list(Number),
%% 				 " \"\" ", "\n"]) of
%% 	{ok,"-1"} ->
%% 	    error;
%% 	{ok,"0"} ->
%% 	    ok;
%% 	Error ->
%% 	    Error
%%     end.

%% @spec say_number(C,Number,Escape) -> Result
%%    C      = Connection
%%    Number = integer
%%    Escape = string
%%    Result = ok | {digit, Digit} | {error, Reason}
%%
%% @doc Say number.
%% @end
say_number(C,Num,Escape) when is_list(Escape)->
    case fast_agi_socket:send(C,["SAY NUMBER ",integer_to_list(Num),
				 " ", Escape, " \n"]) of
	{ok,"-1"} ->
	    error;
	{ok,"0"} ->
	    ok;
	{ok,Digit} ->
	    {digit,Digit};
	Error ->
	    Error
    end.

%% @spec say_phonetic(C,String,Escape) -> Result
%%    C      = Connection
%%    String = string
%%    Result = what
%%
%% @doc Say string with phonetics.
%% @end
say_phonetic(C,String,Escape) ->
    case fast_agi_socket:send(C,["SAY PHONETIC ",String," ",Escape,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,_Key}=Res ->
	    Res
    end.

%% @spec say_time(C,Time,Escape) -> Result
%%    C      = Connection
%%    Time   = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Say time. The Time is given as a UNIX time, i.e. 
%%      number of secunds since 00:00:00 on January 1, 1970.
%% @end
say_time(C,Time,Escape) ->
    case fast_agi_socket:send(C,["SAY TIME ",integer_to_list(Time)," ",
				 Escape,"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,_Key}=Res ->
	    Res
    end.

%% @spec send_image(C,Image) -> Result
%%    C      = Connection
%%    Image  = string
%%    Result = what
%%
%% @doc Send an image on the current channel.
%% @end
send_image(C,Image) ->
    fast_agi_socket:send(C,["SEND IMAGE ",Image,"\n"]).

%% @spec send_text(C,Text) -> Result
%%    C      = Connection
%%    Text   = string
%%    Result = what
%%
%% @doc Send a text on the current channel.
%% @end
send_text(C,Text) ->
    fast_agi_socket:send(C,["SEND TEXT ",Text,"\n"]).

%% @spec set_autohangup(C,Time) -> Result
%%    C      = Connection
%%    Time   = integer
%%    Result = what
%%
%% @doc Set the channel to hangup after Time seconds.
%% @end
set_autohangup(C,Time) ->
    case fast_agi_socket:send(C,["SET AUTOHANGUP ",integer_to_list(Time),"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec set_callerid(C,Number) -> Result
%%    C      = Connection
%%    Number = string
%%    Result = what
%%
%% @doc Set callerid for the current channel.
%% @end
set_callerid(C,Number) ->
    case fast_agi_socket:send(C,["SET CALLERID ",Number,"\n"]) of
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec set_context(C,Context) -> Result
%%    C      = Connection
%%    Context= string
%%    Result = what
%%
%% @doc Set the context for continuation upon exiting the AGI application.
%% @end
set_context(C,Context) ->
    case fast_agi_socket:send(C,["SET CONTEXT ",Context,"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec set_extension(C,Extension) -> Result
%%    C      = Connection
%%    Extension=string
%%    Result = what
%%
%% @doc Changes the extension for continuation upon exiting the AGI application.
%% @end
set_extension(C,Extension) ->
    case fast_agi_socket:send(C,["SET EXTENSION ",Extension,"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec set_music_on(C,State) -> Result
%%    C      = Connection
%%    State  = on|off
%%    Result = what
%%
%% @doc Enable/disable the Music on Hold generator.
%% @end
set_music_on(C,State) ->
    set_music_on(C,State,"").

%% @spec set_music_on(C,State,Class) -> Result
%%    C      = Connection
%%    State  = on|off
%%    Class  = string
%%    Result = what
%%
%% @doc Enable/disable the Music on Hold generator.
%% @end
set_music_on(C,State,Class) ->
    case fast_agi_socket:send(C,["SET MUSIC ON ",State," ",Class,"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec set_priority(C,Priority) -> Result
%%    C      = Connection
%%    Priority=string
%%    Result = what
%%
%% @doc Changes the priority for continuation upon exiting the AGI application.
%% @end
set_priority(C,Priority) ->
    case fast_agi_socket:send(C,["SET PRIORITY ",Priority,"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec set_variable(C,Variable,Value) -> Result
%%    C      = Connection
%%    Variable = string
%%    Value  = string
%%    Result = ok
%%
%% @doc Sets or updates the value of Variable.
%% @end
set_variable(C,Variable,Value) ->
    case fast_agi_socket:send(C,["SET VARIABLE ",Variable," ",Value,"\n"]) of
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec stream_file(C,File) -> Result
%%    C      = Connection
%%    File   = string
%%    Result = what
%%
%% @doc Play audio file indicated by File.
%% @end
stream_file(C,File) ->
    stream_file(C,File,"\"\"").

%% @spec stream_file(C,File,Escape) -> Result
%%    C      = Connection
%%    File   = string
%%    Escape = string
%%    Result = what
%%
%% @doc Play audio file indicated by File.
%% @end
stream_file(C,File,Escape) ->
    case fast_agi_socket:send(C,["STREAM FILE ",File," ", Escape, "\n"]) of
	{ok,"0 endpos=0"} ->
	    error;
	{ok,"0 endpos="++_EPos} ->
	    {ok,timeout};
	{ok,S} ->    % S = "NN endpos=nnn"
	    [D,M]=string:tokens(S," "),
	    [_,EP]=string:tokens(M,"="),
	    {ok,D,list_to_integer(EP)};
	Error ->
	    Error
    end.

%% @spec tdd_mode(C,Mode) -> Result
%%    C      = Connection
%%    Mode   = on|off
%%    Result = what
%%
%% @doc Enable/disable TDD on this channel.
%% @end
tdd_mode(C,Mode) ->
    case fast_agi_socket:send(C,["TDD MODE ",atom_to_list(Mode),"\n"]) of
	{ok,"0"} ->
	    error;
	{ok,"1"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec verbose(C,Message,Level) -> Result
%%    C      = Connection
%%    Message= string
%%    Level  = string
%%    Result = what
%%
%% @doc Sends Message to console via the verbose message system.
%% @end
verbose(C,Message,Level) ->
    case fast_agi_socket:send(C,["VERBOSE ",Message," ",Level,"\n"]) of
	{ok,"0"} ->
	    ok;
	Error ->
	    Error
    end.

%% @spec wait_for_digit(C) -> Result
%%    C      = Connection
%%    Result = what
%%
%% @doc Wait for a DTMF digit on the current channel.
%% @end
wait_for_digit(C) ->
    wait_for_digit(C,-1).

%% @spec wait_for_digit(C,Timeout) -> Result
%%    C      = Connection
%%    Timeout= integer
%%    Result = what
%%
%% @doc Wait for a DTMF digit on the current channel. The timeout is in milliseconds.
%% @end
wait_for_digit(C,Timeout) ->
    case fast_agi_socket:send(C,["WAIT FOR DIGIT ",
				 integer_to_list(Timeout),"\n"]) of
	{ok,"-1"} ->
	    error;
	{ok,"0"} ->
	    timeout;
	{ok,_V}=Res ->
	    Res;
	Error ->
	    Error
    end.

%% @spec get_var(Var::string(),Req::request()) -> string() | undefined
%%    Var = Variable
%%    Req = Request
%%
%% @doc Get the value of a variable passed from Asterisk. 
%% Returns undefined if the variable is not set.
%% @end
get_var(Var,Req) ->
    case lists:keysearch(Var,1,Req) of
	{value,{Var,Val}} ->
	    Val;
	false ->
	    undefined
    end.

%% @spec trace(State) -> ok
%%    State = true | false
%%
%% @doc Trace communication with Asterisk server.
%% @end
trace(true) ->
    case get(agi_trace) of
	true ->
	    nothing;
	_Any ->
	    io:format("Enable fast_agi trace on Pid=~p~n",[self()]),
	    put(agi_trace,true)
    end,
    ok;
trace(false) ->
    case get(agi_trace) of
	true ->
	    io:format("Disable fast_agi trace on Pid=~p~n",[self()]),
	    put(agi_trace,false);
	_Any ->
	    nothing
    end,
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

