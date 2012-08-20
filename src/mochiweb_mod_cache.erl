%% @author Yoshiyuki Kanno <yoshiyuki.kanno@stoic.co.jp>
%% @copyright 2007 Mochi Media, Inc.

%% @doc hook module for memory cache

-module(mochiweb_mod_cache).
-author('yoshiyuki.kanno@stoic.co.jp').

-export([init/1, on_new_request/2, on_purge/2, on_respond/4, terminate/1]).

%% -define(MOD_CACHE_DEF_EXPIRE, 60).
-define(MOD_CACHE_IS_CACHABLE_KEY, "mochiweb_mod_cache_is_cachable").

-record(cache, {
          mtime        = 0    :: integer(), %% gregorian_seconds
          checksum     = 0    :: integer(), %% checksum (ETag - MD5)
          content_type = ""   :: list(),    %% from a Content-Type header
          body         = <<>> :: binary()
         }).

-record(condition, {
          expire                = 0  :: integer(), %% specified per sec
          max_content_len       = 0  :: integer(), %% No cache if Content-Length of a response header was &gt this
          cachable_content_type = [] :: list(),    %% like ["image/png", "image/gif", "image/jpeg"]
          cachable_path_pattern = [] :: list(),    %% compiled regular expressions
          key                   = [] :: list()     %% global uniq key
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
    case application:start(ecache) of
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
    %% Don't use http_util:rfc1123. In this func, There is no error handling for `local_time_to_universe` so badmatch can occur
    %% httpd_util:rfc1123_date(calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Sec))).
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

gen_key(Req) ->
    EndPoints1 = case leo_s3_endpoint:get_endpoints() of
                     {ok, EndPoints0} ->
                         lists:map(fun({endpoint,EP,_}) -> EP end, EndPoints0);
                     _ -> []
                 end,
    [Host|_] = string:tokens(Req:get_header_value("host"), ":"),
    leo_http:key(EndPoints1, Host, Req:get(path)).

on_new_request(
  #condition{expire=0}, _Req) ->
    erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, false),
    none;
on_new_request(
  #condition{cachable_path_pattern=CPs} = Con, Req) when is_list(CPs) andalso length(CPs) > 0->
    Key = gen_key(Req),
    case lists:any(is_cachable_path(Key), CPs) of
        true ->
            NewCon = Con#condition{cachable_path_pattern=[], key = Key},
            on_new_request(NewCon, Req);
        false ->
            erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, false),
            none
    end;
on_new_request(#condition{key=Key} = Con, Req) ->
    case Req:get(method) == 'GET' of
        true ->
            NewCon = case Key of
                         [] ->
                             Con#condition{key = gen_key(Req)};
                         _ ->
                             Con
                     end,
            do_hook_request(NewCon, Req);
        false ->
            erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, false),
            none
    end.

do_hook_request(#condition{expire=Expire, key=Key}, Req) ->
    erlang:put(?MOD_CACHE_IS_CACHABLE_KEY, true),
    case ecache_server:get(Key) of
        undefined ->
            none;
        BinCached ->
            #cache{mtime        = MTime,
                   content_type = ContentType,
                   checksum     = Checksum,
                   body         = Body
                  } = binary_to_term(BinCached),

            Now = now2sec(),
            Diff = Now - MTime,
            case Diff > Expire of
                true ->
                    ecache_server:delete(Key),
                    none;
                false ->
                    LastModified = sec2rfc1123date(MTime),
                    Date  = sec2rfc1123date(Now),
                    Heads = [{"Server",        "LeoFS"},
                             {"Last-Modified", LastModified},
                             {"Date",          Date},
                             {"Age",           integer_to_list(Diff)},
                             {"ETag",          leo_hex:integer_to_hex(Checksum)},
                             {"Cache-Control", "max-age=" ++ integer_to_list(Expire)}],

                    case Req:get_header_value("if-modified-since") of
                        LastModified ->
                            Req:respond({304, mochiweb_headers:make(Heads), ""});
                        _NotConditional ->
                            Req:ok({ContentType, Heads, Body})
                    end,
                    done
            end
    end.

on_purge(_Con, Path) when is_list(Path) ->
    ecache_server:delete(Path).

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
                       Req:get(method) == 'GET' andalso
                           BodySize < MaxLen andalso
                           erlang:get(?MOD_CACHE_IS_CACHABLE_KEY) andalso
                           is_cachable(Req)
               end,

    case Cachable of
        false ->
            ResponseHeaders;
        true ->
            Resp0 = mochiweb_headers:make(ResponseHeaders),

            Key = gen_key(Req),

            case mochiweb_headers:get_value("Cache-Control", Resp0) of
                undefined ->
                    DateSec = case mochiweb_headers:get_value("Date", Resp0) of
                                  undefined ->
                                      now2sec();
                                  HeadDate ->
                                      calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date(HeadDate))
                              end,

                    Bin = term_to_binary(
                            #cache{mtime        = DateSec,
                                   checksum     = leo_hex:hex_to_integer(leo_hex:binary_to_hex(erlang:md5(Body))),
                                   content_type = mochiweb_headers:get_value("Content-Type", Resp0),
                                   body         = Body}),
                    _ = ecache_server:set(Key, Bin),

                    Resp1 = mochiweb_headers:enter("Server", "LeoFS", Resp0),
                    Resp2 = mochiweb_headers:enter("Last-Modified", sec2rfc1123date(DateSec), Resp1),
                    Resp3 = mochiweb_headers:enter("Cache-Control", "max-age=" ++ integer_to_list(Expire), Resp2),
                    Resp3;
                _ ->
                    Resp0
            end
    end.

terminate(_Con) ->
    application:stop(ecache).
