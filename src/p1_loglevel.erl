%%%----------------------------------------------------------------------
%%% File    : p1_loglevel.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Loglevel switcher.
%%%           Be careful: you should not have any p1_logger module
%%%           as p1_loglevel switcher is compiling and loading
%%%           dynamically a "virtual" p1_logger module (Described
%%%           in a string at the end of this module).
%%% Created : 29 Nov 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% p1_logger, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(p1_loglevel).
-author('mickael.remond@process-one.net').

-export([set/1,
	 get/0,
	 set_custom/2,
	 clear_custom/0,
	 clear_custom/1]).

-include("p1_logger.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
-record(p1_loglevel, {ordinal,
		   name,
		   description,
		   function = no_log,
		   event_type = no_log,
		   msg_prefix = no_log}).

-define(LOG_LEVELS,
	[#p1_loglevel{ordinal = 0, name = no_log, description = "No log"},
	 #p1_loglevel{ordinal = 1, name = critical, description = "Critical",
		   function = critical_msg, event_type = error, msg_prefix = "C"},
	 #p1_loglevel{ordinal = 2, name = error, description = "Error",
		   function = error_msg, event_type = error, msg_prefix = "E"},
	 #p1_loglevel{ordinal = 3, name = warning, description = "Warning",
		   function = warning_msg, event_type = warning_msg, msg_prefix = "W"},
	 #p1_loglevel{ordinal = 4, name = info, description = "Info",
		   function = info_msg, event_type = info_msg, msg_prefix = "I"},
	 #p1_loglevel{ordinal = 5, name = debug, description = "Debug",
		   function = debug_msg, event_type = info_msg, msg_prefix = "D"}]).

%% @type level() = integer() | atom().

%% @spec () -> {DefaultLevelOrdinal::integer(), [{Module::atom(), LevelOrdinal::integer()}]}
%% @doc Get the default and all custom levels
get() ->
    {DefaultLevel, _CustomLevels} = p1_logger:get(),
    case lists:keysearch(DefaultLevel, #p1_loglevel.ordinal, ?LOG_LEVELS) of
        {value, Result = #p1_loglevel{}} ->
	    {Result#p1_loglevel.ordinal, Result#p1_loglevel.name, Result#p1_loglevel.description};
        _ ->
	    erlang:error({no_such_loglevel, DefaultLevel})
    end.

%% @spec (DefaultLevel::level() | {DefaultLevel::level(), [{Module::atom(), Level::level()}]}) ->
%%       {module, p1_logger}
%% @doc Set the default and all custom levels
set(DefaultLevel) when is_atom(DefaultLevel) orelse is_integer(DefaultLevel) ->
    set({DefaultLevel, []});
set({DefaultLevel, CustomLevels}) when is_list(CustomLevels) ->
    DefaultInt = level_to_integer(DefaultLevel),
    CustomInts = [level_to_integer(C) || C <- CustomLevels],
    Loglevel = {DefaultInt, CustomInts},
    try
        {Mod,Code} = dynamic_compile:from_string(p1_logger_src(Loglevel)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> error_logger:error_msg("Error compiling p1_logger (~p): ~p~n", [Type, Error])
    end;
set(_) ->
    exit("Invalid p1_loglevel format").

%% @spec (Module::atom(), CustomLevel::level()) -> ok
%% @doc Set a custom level
set_custom(Module, Level) ->
    {DefaultLevel, CustomLevels} = p1_logger:get(),
    case lists:keysearch(Module, 1, CustomLevels) of
	{value, {Module, Level}} ->
	    ok;
	{value, _} ->
	    set({DefaultLevel, lists:keyreplace(Module, 1, CustomLevels, {Module, Level})});
	_ ->
	    set({DefaultLevel, [{Module, Level} | CustomLevels]})
    end.

%% @spec () -> ok
%% @doc Clear all custom levels
clear_custom() ->
    {DefaultLevel, _CustomLevels} = p1_logger:get(),
    set({DefaultLevel, []}).

%% @spec (Module::atom()) -> ok
%% @doc Clear a custom level
clear_custom(Module) ->
    {DefaultLevel, CustomLevels} = p1_logger:get(),
    case lists:keysearch(Module, 1, CustomLevels) of
	{value, _} ->
	    set({DefaultLevel, lists:keydelete(Module, 1, CustomLevels)});
	_ ->
	    ok
    end.

level_to_integer(Level) when is_integer(Level) ->
    Level;
level_to_integer({Module, Level}) ->
    {Module, level_to_integer(Level)};
level_to_integer(Level) ->
    case lists:keysearch(Level, #p1_loglevel.name, ?LOG_LEVELS) of
        {value, #p1_loglevel{ordinal = Int}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the p1_logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
p1_logger_src(Loglevel) ->
    lists:flatten([header_src(),
		   get_src(Loglevel),
		   [log_src(Loglevel, LevelSpec) || LevelSpec <- ?LOG_LEVELS],
		   notify_src()]).

header_src() ->
    "-module(p1_logger).
    -author('mickael.remond@process-one.net').

    -export([debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4,
             get/0]).
    ".

get_src(Loglevel) ->
    io_lib:format("get() -> ~w.
                  ", [Loglevel]).

log_src(_Loglevel, #p1_loglevel{function = no_log}) ->
    [];
log_src({DefaultLevel, [{Module, Level} | Tail]}, Spec = #p1_loglevel{ordinal = MinLevel})
  when Level < MinLevel andalso MinLevel =< DefaultLevel ->
    [atom_to_list(Spec#p1_loglevel.function), "(", atom_to_list(Module), ", _, _, _) -> ok;
     ", log_src({DefaultLevel, Tail}, Spec)];
log_src({DefaultLevel, [{Module, Level} | Tail]}, Spec = #p1_loglevel{ordinal = MinLevel})
  when DefaultLevel < MinLevel andalso MinLevel =< Level ->
    [atom_to_list(Spec#p1_loglevel.function), "(", atom_to_list(Module), " = Module, Line, Format, Args) ->",
     log_notify_src(Spec), ";
     ", log_src({DefaultLevel, Tail}, Spec)];
log_src({DefaultLevel, [_Head | Tail]}, Spec = #p1_loglevel{}) ->
    log_src({DefaultLevel, Tail}, Spec);
log_src({DefaultLevel, []}, Spec = #p1_loglevel{ordinal = MinLevel})
  when DefaultLevel < MinLevel ->
    [atom_to_list(Spec#p1_loglevel.function), "(_, _, _, _) -> ok.
     "];
log_src({_DefaultLevel, []}, Spec = #p1_loglevel{}) ->
    [atom_to_list(Spec#p1_loglevel.function), "(Module, Line, Format, Args) ->",
     log_notify_src(Spec), ".
     "].

log_notify_src(Spec = #p1_loglevel{}) ->
    ["notify(", atom_to_list(Spec#p1_loglevel.event_type), ",
        \"", Spec#p1_loglevel.msg_prefix, "(~p:~p:~p) : \"++Format++\"~n\",
        [self(), Module, Line | Args])"].

notify_src() ->
    %% Distribute the message to the Erlang error p1_logger
    "notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
