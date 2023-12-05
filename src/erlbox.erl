-module(erlbox).
-export([]).

-export([get_key/1, get_key/2, priv_dir/0, filename/1]).

-export([modules/0]).
-export([vsn/0]).
-export([description/0]).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2, success/3]).

-export([is_success/1, is_failure/1]).

-export([attributes/1]).
-export([behaviours/1, callback/4]).

-export([vsn/1]).

-export([timestamp/0]).

-type failure() :: error.
-type failure(E) :: {error, E}.
-type failure(E, R) :: {error, {E, R}}.
-type failure(E, R, S) :: {error, {E, R}, S}.

-type success() :: ok.
-type success(Res) :: {ok, Res}.
-type success(Res, S) :: {ok, Res, S}.
-type success(Res, S, A) :: {ok, Res, S, A}.

-export_type([failure/0, failure/1, failure/2, failure/3]).
-export_type([success/0, success/1, success/2, success/3]).

%%% Application API

-spec get_key(atom()) -> undefined | success(term()).
get_key(Key) ->
    get_key(?MODULE, Key).

-spec get_key(module(), atom()) -> undefined | success(term()).
get_key(App, Key) ->
    application:get_key(App, Key).

-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec filename(file:filename()) -> file:filename().
filename(Path) ->
    filename:join(priv_dir(), Path).

-spec modules() -> [module()].
modules() ->
    {ok, Mods} = get_key(?MODULE, modules),
    
    Res = Mods,
    Res.

-spec vsn() -> binary().
vsn() ->
    {ok, Vsn} = get_key(?MODULE, vsn),
    
    Res = Vsn,
    Res.

-spec description() ->  binary().
description() ->
    {ok, Desc} = get_key(?MODULE, description),
    
    Res = Desc,
    Res.

%%% Response API

-spec failure() -> failure().
failure() ->
    error.

-spec failure(term()) -> failure(term()).
failure(E) ->
    {error, E}.

-spec failure(term(), term()) -> failure(term(), term()).
failure(E, R) ->
    {error, {E, R}}.

-spec failure(term(), term(), term()) -> failure(term(), term(), term()).
failure(E, R, S) ->
    {error, {E, R}, S}.

-spec success() -> success().
success() ->
    ok.

-spec success(term()) -> success(term()).
success(Res) ->
    {ok, Res}.

-spec success(term(), term()) -> success(term(), term()).
success(Res, S) ->
    {ok, Res, S}.

-spec success(term(), term(), [term()]) -> success(term(), term(), [term()]).
success(Res, S, A) ->
    {ok, Res, S, A}.

is_success(ok) ->
    true;
is_success({ok, _Res}) ->
    true;
is_success({ok, _Res, _S}) ->
    true;
is_success({ok, _Res, _S, _A}) ->
    true;
is_success(_) ->
    false.

-spec is_failure(term()) -> boolean().
is_failure(error) ->
    true;
is_failure({error, {_E, _R}}) ->
    true;
is_failure({error, _R}) ->
    true;
is_failure({error, {_E, _R}, _S}) ->
    true;
is_failure(_) ->
    false.

%%% Module API

-spec attributes(module()) -> [{atom(), term()}].
attributes(Mod) ->
    Mod:module_info(attributes).

-spec behaviours(module()) -> [atom()].
behaviours(Mod) ->
    [Name|| {behaviour, [Name]} <- attributes(Mod)].

-spec callback(module(), atom(), [term()], term()) -> term().
callback(M, F, A, Def) ->
    case erlang:function_exported(M, F, length(A)) of
        true ->
            erlang:apply(M, F, A);
        _  -> 
            erlang:apply(Def, A) 
    end.

-spec vsn(module()) -> binary() | integer().
vsn(Mod) ->
    {_, [Vsn]} = lists:keyfind(vsn, 1, attributes(Mod)),
    
    Res = Vsn,
    Res.

%% Timestamp API

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(second) + erlang:time_offset(second).
