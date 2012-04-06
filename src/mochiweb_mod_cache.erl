%% @author Yoshiyuki Kanno <yoshiyuki.kanno@stoic.co.jp>
%% @copyright 2007 Mochi Media, Inc.

%% @doc hook module for memory cache

-module(mochiweb_mod_cache).
-author('yoshiyuki.kanno@stoic.co.jp').

-export([init/1, on_new_request/2, on_respond/4, terminate/1]).

%-define(MOD_CACHE_DEF_EXPIRE, 60).
-define(MOD_CACHE_IS_CACHABLE_KEY, "mochiweb_mod_cache_is_cachable").

-record(cache, {
        mtime        = 0    :: integer(), % gregorian_seconds
        content_type = ""   :: list(),    % from a Content-Type header
        body         = <<>> :: binary()
}).

-record(condition, {
        expire                = 0  :: integer(), % specified per sec
        max_content_len       = 0  :: integer(), % No cache if Content-Length of a response header was &gt this
        cachable_content_type = [] :: list(),    % like ["image/png", "image/gif", "image/jpeg"]
        cachable_path_pattern = [] :: list()     % compiled regular expressions
}).

init(Args) when is_list(Args) ->
    init(Args, #condition{}).

init([{expire, Expire}|T], Con) when is_integer(Expire) andalso Expire > 0 ->
    init(T, Con#condition{expire=Expire});
init([{max_content_len, Len}|T], Con) when is_integer(Len) andalso Len > 0 ->
    init(T, Con#condition{max_content_len=Len});
init([{cachable_content_type, CT}|T], Con) when is_list(CT) ->
    init(T, Con#condition{cachable_content_type = CT});
init([{cachable_path_pattern, Patterns}|T], Con) when is_list(Patterns) ->
    CompiledList = lists:foldl(fun(P, Acc) ->
        case re:compile(P) of
            {ok, MP} ->
                [MP|Acc];
            _Error ->
                Acc
        end
    end, [], Patterns),
    init(T, Con#condition{cachable_path_pattern = CompiledList});
init([], Con) ->
    case application:start(ecache_app) of
        ok ->
            {ok, Con};
        Error ->
            Error
    end.

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
% Don't use http_util:rfc1123. In this func, There is no error handling for `local_time_to_universe` so badmatch can occur
% httpd_util:rfc1123_date(calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Sec))).
    {{Y,M,D},{H,MI,S}} = calendar:gregorian_seconds_to_datetime(Sec),
    Mon = month2rfc1123(M),
    W = week2rfc1123(Y,M,D),
    lists:flatten(io_lib:format("~3s, ~2.10.0B ~3s ~4.10B ~2.10.0B:~2.10.0B:~2.10.0B GMT", [W, D, Mon, Y, H, MI, S])).

is_cachable_path(Path) ->
    fun(MP) -> 
        case re:run(Path, MP) of
            {match, _Cap} ->
                true;
            _Else ->
                false
        end
    end.

on_new_request(
    #condition{expire=0}, _Req) ->
    erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, false),
    none;
on_new_request(
    #condition{cachable_path_pattern=CPs} = Con, Req) when is_list(CPs) andalso length(CPs) > 0->
    case lists:any(is_cachable_path(Req:get(path)), CPs) of
        true ->
            NewCon = Con#condition{cachable_path_pattern=[]},
            on_new_request(NewCon, Req);
        false ->
            erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, false),
            none
    end;
on_new_request(Con, Req) ->
    case Req:get(method) == 'GET' of
        true ->
            do_hook_request(Con, Req);
        false ->
            erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, false),
            none
    end.

do_hook_request(#condition{expire=Expire}, Req) ->
    erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, true),
    Key = Req:get(path),
    case ecache_server:get(Key) of
        undefined ->
            none;
        BinCached ->
            Cached = binary_to_term(BinCached),
            Now = now2sec(),
            Diff = Now - Cached#cache.mtime,
            case Diff > Expire of
                true ->
                    ecache_server:delete(Key),
                    none;
                false ->
                    LastModified = sec2rfc1123date(Cached#cache.mtime),
                    Date = sec2rfc1123date(Now),
                    Heads = [{"Last-Modified", LastModified}, {"Date", Date}, {"Age", integer_to_list(Diff)},
                        {"Cache-Control", "max-age=" ++ integer_to_list(Expire)}],
                    case Req:get_header_value("if-modified-since") of
                        LastModified ->
                            Req:respond({304, mochiweb_headers:make(Heads), ""});
                        _NotConditional ->
                            Req:ok({Cached#cache.content_type, Heads, Cached#cache.body})
                    end,
                    done
            end
    end.

on_respond(
    #condition{cachable_content_type=CCs} = Con, Req, ResponseHeaders, Body) when is_list(CCs) andalso length(CCs) > 0->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    case lists:member(mochiweb_headers:get_value("Content-Type", HResponse), CCs) of
        true ->
            NewCon = Con#condition{cachable_content_type=[]},
            on_respond(NewCon, Req, HResponse, Body);
        false ->
            HResponse
    end;
on_respond(#condition{expire=Expire, max_content_len=MaxLen}, Req, ResponseHeaders, Body) ->
    Cachable = case iolist_size(Body) of
        0 ->
            false;
        BodySize ->
            Req:get(method) == 'GET' andalso BodySize < MaxLen andalso erlang:get(?MOD_CACHE_IS_CACHABLE_KEY) andalso is_cachable(Req)
    end,
    case Cachable of
        false ->
            ResponseHeaders;
        true ->
            HResponse = mochiweb_headers:make(ResponseHeaders),
            Key = Req:get(path),
            case mochiweb_headers:get_value("Cache-Control", HResponse) of
                undefined ->
                    DateSec = case mochiweb_headers:get_value("Date", HResponse) of
                        undefined ->
                            now2sec();
                        HeadDate ->
                            calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(HeadDate))
                    end,
                    BinVal = term_to_binary(#cache{
                        mtime = DateSec,
                        content_type = mochiweb_headers:get_value("Content-Type", HResponse),
                        body = Body
                    }),
                    ecache_server:set(Key, BinVal),
                    LastModified = sec2rfc1123date(DateSec),
                    NewRes = mochiweb_headers:enter("Last-Modified", LastModified, HResponse),
                    mochiweb_headers:enter("Cache-Control", "max-age=" ++ integer_to_list(Expire), NewRes);
                _Else ->
                    HResponse
            end
    end.

terminate(_Con) ->
    application:stop(ecache_app).
