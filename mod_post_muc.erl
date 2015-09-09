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
