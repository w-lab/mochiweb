%% @author Yoshiyuki Kanno <yoshiyuki.kanno@stoic.co.jp>
%% @copyright 2007 Mochi Media, Inc.

%% @doc hook module for memory cache

-module(mochiweb_mod_cache).
-author('yoshiyuki.kanno@stoic.co.jp').

-export([init/0, on_new_request/1, on_respond/3, terminate/0]).

-define(SAVE_IS_CACHABLE, mochiweb_mod_cache_cachable).
-define(SAVE_IS_CACHED  , mochiweb_mod_cache_cached).
-define(MOD_CACHE_DEF_EXPIRE, 60).

-record(cache, {
        mtime        = 0    :: calendar:datetime(), % from a Date header or generates from calendar:local_time
        content_type = ""   :: list(), % from a Content-Type header
        body         = <<>> :: binary()
}).

init() ->
    application:start(ecache_app).

is_cachable_maxage(MA) ->
    case string:str(MA, "max-age") of
        0 ->
            true;
        _Index ->
            SepPos = string:str(MA, "="),
            Val = string:strip(string:substr(MA, SepPos+1)),
            case string:to_integer(Val) of
                {Int, _Ret} when Int =:= 0 ->
                    false;
                _Error ->
                    true
            end
    end.

is_cachable_cc(CC) ->
    case CC of
        undefined ->
            true;
        "no-cache" ->
            false;
        "no-store" ->
            false;
        Else ->
            is_cachable_maxage(Else)
    end.

is_cachable_pg(PG) ->
    case PG of
        undefined ->
            true;
        "no-cache" ->
            false;
        _Else ->
            true
    end.

is_cachable(Req) ->
    CC = Req:get_header_value("Cache-Control"),
    case is_cachable_cc(CC) of
        true ->
            PG= Req:get_header_value("Pragma"),
            is_cachable_pg(PG);
        false ->
            false        
    end.

on_new_request(Req) ->
    % judgement whether shold cache or not
    Cachable = is_cachable(Req),
    erlang:put(?SAVE_IS_CACHABLE, Cachable),
    case Cachable of
        false ->
            none;
        true ->
            Key = Req:get(path),
            % check cache
            % 1. exist
            % 1.1 validate return done if valid, else return none
            % 2. non
            % return none
            Ret = case ecache_server:get(Key) of
                undefined ->
                    erlang:put(?SAVE_IS_CACHED, undefined),
                    none;
                BinCached ->
                    Cached = binary_to_term(BinCached),
                    erlang:put(?SAVE_IS_CACHED, Cached),
                    Req:ok({Cached#cache.content_type, [{"Date", httpd_util:rfc1123_date(Cached#cache.mtime)}], Cached#cache.body}),
                    done
            end,
            Ret
    end.

on_respond(Req, ResponseHeaders, Body) ->
    Cachable = erlang:get(?SAVE_IS_CACHABLE),
    case Cachable of
        false ->
            ResponseHeaders;
        true ->
            Key = Req:get(path),
            NewResHead = case erlang:get(?SAVE_IS_CACHED) of
                undefined ->
                    ErlDate = case mochiweb_headers:get_value("Date", ResponseHeaders) of
                        undefined ->
                            calendar:local_time();
                        HeadDate ->
                            httpd_util:convert_request_date(HeadDate)
                    end,
                    BinVal = term_to_binary(#cache{
                        mtime = ErlDate,
                        content_type = mochiweb_headers:get_value("Content-Type", ResponseHeaders),
                        body = Body
                    }),
                    ecache_server:set(Key, BinVal),
                    mochiweb_headers:enter("Cache-Control", "max-age=" ++ integer_to_list(?MOD_CACHE_DEF_EXPIRE), ResponseHeaders);
                Cached ->
                    MT = calendar:datetime_to_gregorian_seconds(Cached#cache.mtime),
                    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
                    Diff = Now - MT,
                    case Diff > ?MOD_CACHE_DEF_EXPIRE of
                        true ->
                            %expired 
                            ecache_server:delete(Key),
                            ResponseHeaders;
                        false ->
                            mochiweb_headers:enter("Cache-Control", "max-age=" ++ integer_to_list(?MOD_CACHE_DEF_EXPIRE - Diff), ResponseHeaders)
                    end
            end,
            NewResHead
    end.

terminate() ->
    application:stop(ecache_app).
