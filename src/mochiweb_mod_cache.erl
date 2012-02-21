%% @author Yoshiyuki Kanno <yoshiyuki.kanno@stoic.co.jp>
%% @copyright 2007 Mochi Media, Inc.

%% @doc hook module for memory cache

-module(mochiweb_mod_cache).
-author('yoshiyuki.kanno@stoic.co.jp').

-export([init/0, on_new_request/1, on_respond/3, terminate/0]).

init() ->
    application:start(ecache_app).

on_new_request(Req) ->
    Key = Req:get(raw_path),
    % judgement whether shold cache or not
    % check cache
    % 1. exist
    % 1.1 validate return done if valid, else return none
    % 2. non
    % return none
    Ret = case ecache_server:get(Key) of
        undefined ->
            none;
        Cache ->
            Req:ok({"image/png", Cache}),
            done
    end,
    Ret.

on_respond(Req, ResponseHeaders, Body) ->
    Key = Req:get(raw_path),
    NewResHead = case ecache_server:get(Key) of
        undefined ->
            ecache_server:set(Key, Body),
            mochiweb_headers:enter("Cache-Control", "max-age=31536000", ResponseHeaders);
        Cache ->
            mochiweb_headers:enter("Cache-Control", "max-age=31536000", ResponseHeaders)
    end,
    NewResHead.

terminate() ->
    application:stop(ecache_app).
