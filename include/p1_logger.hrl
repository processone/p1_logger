%%%----------------------------------------------------------------------
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

-define(PRINT(Format, Args), io:format(Format, Args)).

-define(DEBUG(Format, Args),
	p1_logger:debug_msg(?MODULE, ?LINE, Format, Args)).

-define(INFO_MSG(Format, Args),
	p1_logger:info_msg(?MODULE, ?LINE, Format, Args)).

-define(WARNING_MSG(Format, Args),
	p1_logger:warning_msg(?MODULE, ?LINE, Format, Args)).

-define(ERROR_MSG(Format, Args),
	p1_logger:error_msg(?MODULE, ?LINE, Format, Args)).

-define(CRITICAL_MSG(Format, Args),
	p1_logger:critical_msg(?MODULE, ?LINE, Format, Args)).
