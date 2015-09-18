%%% File    : mod_post_muc.erl
%%% Author  : Nuno Horta <nunobhorta@gmail.com>
%%% Copyright (C) 2015 Nuno Horta
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

-module(mod_post_muc).
-author('Nuno Horta').

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 post_muc/5]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_post_muc", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, post_muc, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_post_muc", [] ),
    ejabberd_hooks:delete(muc_filter_message, Host,
			  ?MODULE, post_muc, 10),

    ok.

post_muc(Stanza, MUCState, RoomJID, FromJID, FromNick) ->
    PostUrl = gen_mod:get_module_opt(FromJID#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Body = xml:get_path_s(Stanza, [{elem, list_to_binary("body")}, cdata]),

    if (Stanza /= "") ->
        Sep = "&",
	      Post = ["to=", RoomJID#jid.luser, Sep,
                  "from=", FromJID#jid.luser, Sep,
                  "nick=", FromNick, Sep,
                  "body=", binary_to_list(Body)],
          ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
	      httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
	      Stanza;
	    true ->
	      Stanza
    end.
