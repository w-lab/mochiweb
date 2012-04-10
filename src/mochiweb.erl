%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Start and stop the MochiWeb server.

-module(mochiweb).
-author('bob@mochimedia.com').

-export([new_request/1, new_response/1]).
-export([all_loaded/0, all_loaded/1, reload/0]).
-export([ensure_started/1]).
-export([init_hook_modules/1, set_hook_modules/1, get_hook_modules/0,
         on_new_request_hook_modules/1, on_new_request_hook_modules/2,
         on_respond_hook_modules/3, on_respond_hook_modules/4, on_purge_hook_modules/1,
         terminate_hook_modules/0, terminate_hook_modules/1]).

-define(SAVE_HOOK_MODULES, mochiweb_hook_modules).

reload() ->
    [c:l(Module) || Module <- all_loaded()].

all_loaded() ->
    all_loaded(filename:dirname(code:which(?MODULE))).

all_loaded(Base) when is_atom(Base) ->
    [];
all_loaded(Base) ->
    FullBase = Base ++ "/",
    F = fun ({_Module, Loaded}, Acc) when is_atom(Loaded) ->
                Acc;
            ({Module, Loaded}, Acc) ->
                case lists:prefix(FullBase, Loaded) of
                    true ->
                        [Module | Acc];
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, [], code:all_loaded()).

init_hook_modules(undefined) ->
	ok;
init_hook_modules([]) ->
	ok;
init_hook_modules([{ModName, Args}|T]) when is_list(Args) ->
	case code:ensure_loaded(ModName) of
		{module, Module} ->
			case Module:init(Args) of
                {ok, Desc} ->
        			Loaded = case erlang:get(?SAVE_HOOK_MODULES) of
        				undefined ->
        					[];
        				Cached ->
        					Cached
        			end,
			        erlang:put(?SAVE_HOOK_MODULES, [{Module, Desc} | Loaded]),
			        init_hook_modules(T);
                InitError ->
                    InitError
            end;
		Error ->
			Error
	end.

set_hook_modules(L) when is_list(L) ->
	erlang:put(?SAVE_HOOK_MODULES, L);
set_hook_modules(_L) ->
	erlang:put(?SAVE_HOOK_MODULES, []).

get_hook_modules() ->
	erlang:get(?SAVE_HOOK_MODULES).

on_new_request_hook_modules(Req) ->
    on_new_request_hook_modules(Req, get_hook_modules()).
on_new_request_hook_modules(_Req, []) ->
    none;
on_new_request_hook_modules(_Req, undefined) ->
    none;
on_new_request_hook_modules(Req, [{Module, Desc}|T]) ->
    case Module:on_new_request(Desc, Req) of
        done ->
            done;
        _Other ->
            on_new_request_hook_modules(Req,T)
    end.

on_purge_hook_modules(Path) ->
    on_purge_hook_modules(Path, get_hook_modules()).
on_purge_hook_modules(_Path, []) ->
    void;
on_purge_hook_modules(_Path, undefined) ->
    void;
on_purge_hook_modules(Path, [{Module, Desc}|T]) ->
    Module:on_purge(Desc, Path),
    on_purge_hook_modules(Path, T).

on_respond_hook_modules(Req, ResponseHeaders, Body) ->
    on_respond_hook_modules(Req, ResponseHeaders, Body, get_hook_modules()).
on_respond_hook_modules(_Req, ResponseHeaders, _Body, []) ->
    ResponseHeaders;
on_respond_hook_modules(_Req, ResponseHeaders, _Body, undefined) ->
    ResponseHeaders;
on_respond_hook_modules(Req, ResponseHeaders, Body, [{Module, Desc}|T]) ->
    NewResHead = Module:on_respond(Desc, Req, ResponseHeaders, Body),
    on_respond_hook_modules(Req, NewResHead, Body, T).

terminate_hook_modules() ->
	terminate_hook_modules(get_hook_modules()).

terminate_hook_modules([]) ->
	ok;
terminate_hook_modules(undefined) ->
	ok;
terminate_hook_modules([{Module, Desc}|T]) ->
	Module:terminate(Desc),
	terminate_hook_modules(T).

%% @spec new_request({Socket, Request, Headers}) -> MochiWebRequest
%% @doc Return a mochiweb_request data structure.
new_request({Socket, {Method, {abs_path, Uri}, Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         mochiweb_headers:make(Headers));
% this case probably doesn't "exist".
new_request({Socket, {Method, {absoluteURI, _Protocol, _Host, _Port, Uri},
                      Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         mochiweb_headers:make(Headers));
%% Request-URI is "*"
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
new_request({Socket, {Method, '*'=Uri, Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         mochiweb_headers:make(Headers)).

%% @spec new_response({Request, integer(), Headers}) -> MochiWebResponse
%% @doc Return a mochiweb_response data structure.
new_response({Request, Code, Headers}) ->
    mochiweb_response:new(Request,
                          Code,
                          mochiweb_headers:make(Headers)).

%% @spec ensure_started(App::atom()) -> ok
%% @doc Start the given App if it has not been started already.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(treq, {path, body= <<>>, xreply= <<>>}).

ssl_cert_opts() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "support", "test-materials"]),
    CertFile = filename:join(CertDir, "test_ssl_cert.pem"),
    KeyFile = filename:join(CertDir, "test_ssl_key.pem"),
    [{certfile, CertFile}, {keyfile, KeyFile}].

with_server(Transport, ServerFun, ClientFun) ->
    ServerOpts0 = [{ip, "127.0.0.1"}, {port, 0}, {loop, ServerFun}],
    ServerOpts = case Transport of
        plain ->
            ServerOpts0;
        ssl ->
            ServerOpts0 ++ [{ssl, true}, {ssl_opts, ssl_cert_opts()}]
    end,
    {ok, Server} = mochiweb_http:start_link(ServerOpts),
    Port = mochiweb_socket_server:get(Server, port),
    Res = (catch ClientFun(Transport, Port)),
    mochiweb_http:stop(Server),
    Res.

request_test() ->
    R = mochiweb_request:new(z, z, "/foo/bar/baz%20wibble+quux?qs=2", z, []),
    "/foo/bar/baz wibble quux" = R:get(path),
    ok.

-define(LARGE_TIMEOUT, 60).

single_http_GET_test() ->
    do_GET(plain, 1).

single_https_GET_test() ->
    do_GET(ssl, 1).

multiple_http_GET_test() ->
    do_GET(plain, 3).

multiple_https_GET_test() ->
    do_GET(ssl, 3).

hundred_http_GET_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_GET(plain,100)) end}.

hundred_https_GET_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_GET(ssl,100)) end}.

single_128_http_POST_test() ->
    do_POST(plain, 128, 1).

single_128_https_POST_test() ->
    do_POST(ssl, 128, 1).

single_2k_http_POST_test() ->
    do_POST(plain, 2048, 1).

single_2k_https_POST_test() ->
    do_POST(ssl, 2048, 1).

single_100k_http_POST_test() ->
    do_POST(plain, 102400, 1).

single_100k_https_POST_test() ->
    do_POST(ssl, 102400, 1).

multiple_100k_http_POST_test() ->
    do_POST(plain, 102400, 3).

multiple_100K_https_POST_test() ->
    do_POST(ssl, 102400, 3).

hundred_128_http_POST_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(plain, 128, 100)) end}.

hundred_128_https_POST_test_() -> % note the underscore
    {timeout, ?LARGE_TIMEOUT,
     fun() -> ?assertEqual(ok, do_POST(ssl, 128, 100)) end}.

do_GET(Transport, Times) ->
    PathPrefix = "/whatever/",
    ReplyPrefix = "You requested: ",
    ServerFun = fun (Req) ->
                        Reply = ReplyPrefix ++ Req:get(path),
                        Req:ok({"text/plain", Reply})
                end,
    TestReqs = [begin
                    Path = PathPrefix ++ integer_to_list(N),
                    ExpectedReply = list_to_binary(ReplyPrefix ++ Path),
                    #treq{path=Path, xreply=ExpectedReply}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('GET', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

do_POST(Transport, Size, Times) ->
    ServerFun = fun (Req) ->
                        Body = Req:recv_body(),
                        Headers = [{"Content-Type", "application/octet-stream"}],
                        Req:respond({201, Headers, Body})
                end,
    TestReqs = [begin
                    Path = "/stuff/" ++ integer_to_list(N),
                    Body = crypto:rand_bytes(Size),
                    #treq{path=Path, body=Body, xreply=Body}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('POST', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

new_client_fun(Method, TestReqs) ->
    fun (Transport, Port) ->
            client_request(Transport, Port, Method, TestReqs)
    end.

client_request(Transport, Port, Method, TestReqs) ->
    Opts = [binary, {active, false}, {packet, http}],
    SockFun = case Transport of
        plain ->
            {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, Opts),
            fun (recv) ->
                    gen_tcp:recv(Socket, 0);
                ({recv, Length}) ->
                    gen_tcp:recv(Socket, Length);
                ({send, Data}) ->
                    gen_tcp:send(Socket, Data);
                ({setopts, L}) ->
                    inet:setopts(Socket, L)
            end;
        ssl ->
            {ok, Socket} = ssl:connect("127.0.0.1", Port, [{ssl_imp, new} | Opts]),
            fun (recv) ->
                    ssl:recv(Socket, 0);
                ({recv, Length}) ->
                    ssl:recv(Socket, Length);
                ({send, Data}) ->
                    ssl:send(Socket, Data);
                ({setopts, L}) ->
                    ssl:setopts(Socket, L)
            end
    end,
    client_request(SockFun, Method, TestReqs).

client_request(SockFun, _Method, []) ->
    {the_end, {error, closed}} = {the_end, SockFun(recv)},
    ok;
client_request(SockFun, Method,
               [#treq{path=Path, body=Body, xreply=ExReply} | Rest]) ->
    Request = [atom_to_list(Method), " ", Path, " HTTP/1.1\r\n",
               client_headers(Body, Rest =:= []),
               "\r\n",
               Body],
    ok = SockFun({send, Request}),
    case Method of
        'GET' ->
            {ok, {http_response, {1,1}, 200, "OK"}} = SockFun(recv);
        'POST' ->
            {ok, {http_response, {1,1}, 201, "Created"}} = SockFun(recv)
    end,
    ok = SockFun({setopts, [{packet, httph}]}),
    {ok, {http_header, _, 'Server', _, "MochiWeb" ++ _}} = SockFun(recv),
    {ok, {http_header, _, 'Date', _, _}} = SockFun(recv),
    {ok, {http_header, _, 'Content-Type', _, _}} = SockFun(recv),
    {ok, {http_header, _, 'Content-Length', _, ConLenStr}} = SockFun(recv),
    ContentLength = list_to_integer(ConLenStr),
    {ok, http_eoh} = SockFun(recv),
    ok = SockFun({setopts, [{packet, raw}]}),
    {payload, ExReply} = {payload, drain_reply(SockFun, ContentLength, <<>>)},
    ok = SockFun({setopts, [{packet, http}]}),
    client_request(SockFun, Method, Rest).

client_headers(Body, IsLastRequest) ->
    ["Host: localhost\r\n",
     case Body of
        <<>> ->
            "";
        _ ->
            ["Content-Type: application/octet-stream\r\n",
             "Content-Length: ", integer_to_list(byte_size(Body)), "\r\n"]
     end,
     case IsLastRequest of
         true ->
             "Connection: close\r\n";
         false ->
             ""
     end].

drain_reply(_SockFun, 0, Acc) ->
    Acc;
drain_reply(SockFun, Length, Acc) ->
    Sz = erlang:min(Length, 1024),
    {ok, B} = SockFun({recv, Sz}),
    drain_reply(SockFun, Length - Sz, <<Acc/bytes, B/bytes>>).

-endif.
