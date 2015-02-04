%%%----------------------------------------------------------------------
%%% File    : p1_logger.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : logger wrapper
%%% Created : 10 Jul 2012 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% p1_logger, Copyright (C) 2002-2015   ProcessOne
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

-module(p1_logger).

-compile({no_auto_import, [{get, 0}]}).

%% API
-export([debug_msg/4, info_msg/4, warning_msg/4,
         error_msg/4, critical_msg/4, get/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec debug_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec info_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec warning_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec error_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec critical_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec get() -> {non_neg_integer(), [{atom(), non_neg_integer()}]}.

debug_msg(_Mod, _Line, _Format, _Args) -> ok.
info_msg(_Mod, _Line, _Format, _Args) -> ok.
warning_msg(_Mod, _Line, _Format, _Args) -> ok.
error_msg(_Mod, _Line, _Format, _Args) -> ok.
critical_msg(_Mod, _Line, _Format, _Args) -> ok.
get() -> {0, [{foo, 0}]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
