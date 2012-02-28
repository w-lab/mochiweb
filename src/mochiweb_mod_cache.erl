%% @author Yoshiyuki Kanno <yoshiyuki.kanno@stoic.co.jp>
%% @copyright 2007 Mochi Media, Inc.

%% @doc hook module for memory cache

-module(mochiweb_mod_cache).
-author('yoshiyuki.kanno@stoic.co.jp').

-export([init/0, on_new_request/1, on_respond/3, terminate/0]).

%-define(SAVE_IS_CACHABLE, mochiweb_mod_cache_cachable).
%-define(SAVE_IS_CACHED  , mochiweb_mod_cache_cached).
-define(MOD_CACHE_DEF_EXPIRE, 60).

-record(cache, {
        mtime        = 0    :: integer(), % gregorian_seconds
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

now2sec() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

week2rfc1123(Y, M, D) ->
    case calendar:day_of_the_week(Y, M, D) of
        1 ->
            "Mon";
        2 ->
            "Tue";
        3 ->
            "Wed";
        4 ->
            "Thu";
        5 ->
            "Fri";
        6 ->
            "Sat";
        7 ->
            "Sun"
    end.

month2rfc1123(M) ->
    case M of
        1 ->
            "Jan";
        2 ->
            "Feb";
        3 ->
            "Mar";
        4 ->
            "Apr";
        5 ->
            "May";
        6 ->
            "Jun";
        7 ->
            "Jul";
        8 ->
            "Aug";
        9 ->
            "Sep";
        10 ->
            "Oct";
        11 ->
            "Nov";
        12 ->
            "Dec"
    end.

sec2rfc1123date(Sec) ->
%    httpd_util:rfc1123_date(calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Sec))).
%    io:format("~p",[calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Sec))]).
%    calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Sec)).
    {{Y,M,D},{H,MI,S}} = calendar:gregorian_seconds_to_datetime(Sec),
    Mon = month2rfc1123(M),
    W = week2rfc1123(Y,M,D),
    lists:flatten(io_lib:format("~3s, ~2.10.0B ~3s ~4.10B ~2.10.0B:~2.10.0B:~2.10.0B GMT", [W, D, Mon, Y, H, MI, S])).

on_new_request(Req) ->
    Key = Req:get(path),
    % check cache
    % 1. exist
    % 1.1 validate return done if valid, else return none
    % 2. non
    % return none
    case ecache_server:get(Key) of
        undefined ->
            none;
        BinCached ->
            Cached = binary_to_term(BinCached),
            Now = now2sec(),
            Diff = Now - Cached#cache.mtime,
            case Diff > ?MOD_CACHE_DEF_EXPIRE of
                true ->
                    ecache_server:delete(Key),
                    none;
                false ->
                    LastModified = sec2rfc1123date(Cached#cache.mtime),
                    Heads = [{"Date", LastModified}, 
                        {"Cache-Control", "max-age=" ++ integer_to_list(?MOD_CACHE_DEF_EXPIRE - Diff)}],
                    case Req:get_header_value("if-modified-since") of
                        LastModified ->
                            Req:respond({304, mochiweb_headers:make(Heads), ""});
                        _NotConditional ->
                            Req:ok({Cached#cache.content_type, Heads, Cached#cache.body})
                    end,
                    done
            end
    end.

on_respond(Req, ResponseHeaders, Body) ->
    Cachable = case iolist_size(Body) of
        0 ->
            false;
        _HasBody ->
            is_cachable(Req)
    end,
    case Cachable of
        false ->
            ResponseHeaders;
        true ->
            Key = Req:get(path),
            case mochiweb_headers:get_value("Cache-Control", ResponseHeaders) of
                undefined ->
                    DateSec = case mochiweb_headers:get_value("Date", ResponseHeaders) of
                        undefined ->
                            now2sec();
                        HeadDate ->
                            calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(HeadDate))
                    end,
                    BinVal = term_to_binary(#cache{
                        mtime = DateSec,
                        content_type = mochiweb_headers:get_value("Content-Type", ResponseHeaders),
                        body = Body
                    }),
                    ecache_server:set(Key, BinVal),
                    mochiweb_headers:enter("Cache-Control", "max-age=" ++ integer_to_list(?MOD_CACHE_DEF_EXPIRE), ResponseHeaders);
                _Else ->
                    ResponseHeaders
            end
    end.

terminate() ->
    application:stop(ecache_app).
