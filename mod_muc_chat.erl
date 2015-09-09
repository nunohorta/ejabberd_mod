-module(mod_muc_chat).
-author('Nuno Horta').

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 send_muc_message/5]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_muc_chat", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

%%% muc_filter_message(Stanza, MUCState, RoomJID, FromJID, FromNick) -> Stanza
init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, send_muc_message, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_muc_chat", [] ),
    ejabberd_hooks:delete(muc_filter_message, Host,
			  ?MODULE, send_muc_message, 10),

    ok.

send_muc_message(Stanza, MUCState, RoomJID, FromJID, FromNick) ->
    PostUrl = gen_mod:get_module_opt(FromJID#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Body = xml:get_path_s(Stanza, [{elem, list_to_binary("body")}, cdata]),

    if (Stanza /= "") ->
        Sep = "&",
	      Post = ["to=", RoomJID#jid.luser, Sep,
                  "from=", FromJID#jid.luser, Sep,
                  "nick=", FromNick, Sep,
                  "body=", url_encode(binary_to_list(Body))],
          ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
	      httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
	      Stanza;
	    true ->
	      Stanza
    end.

%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).
