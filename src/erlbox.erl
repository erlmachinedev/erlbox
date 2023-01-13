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
-export([behaviours/1]).
-export([optional_callback/3, optional_callback/4]).
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

-spec get_key(Key::atom()) -> 'undefined' | success(term()).
get_key(Key) ->
    get_key(?MODULE, Key).

-spec get_key(App::module(), Key::atom()) -> 'undefined' | success(term()).
get_key(App, Key) ->
    application:get_key(App, Key).

-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec filename(Path::list()) -> list().
filename(Path) ->
    filename:join(priv_dir(), Path).

-spec modules() -> [module()].
modules() ->
    {ok, Modules} = get_key(?MODULE, 'modules'),
    Modules.

-spec vsn() -> binary().
vsn() ->
    {ok, Vsn} = get_key(?MODULE, 'vsn'),
    Vsn.

-spec description() ->  binary().
description() ->
    {ok, Desc} = get_key(?MODULE, 'description'),
    Desc.

%%% Response API

-spec failure() -> failure().
failure() ->
    error.

-spec failure(E::term()) -> failure(term()).
failure(E) ->
    {error, E}.

-spec failure(E::term(), R::term()) ->
                     failure(term(), term()).
failure(E, R) ->
    {error, {E, R}}.

-spec failure(E::term(), R::term(), S::term()) -> failure(term(), term(), term()).
failure(E, R, S) ->
    {error, {E, R}, S}.

-spec success() -> success().
success() ->
    ok.

-spec success(Res::term()) -> success(term()).
success(Res) ->
    {ok, Res}.

-spec success(Res::term(), S::term()) -> success(term(), term()).
success(Res, S) ->
    {ok, Res, S}.

-spec success(Res::term(), S::term(), A::[term()]) -> success(term(), term(), [term()]).
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

-spec attributes(Module::module()) -> [{atom(), term()}].
attributes(Module) ->
    Module:module_info(attributes).

-spec behaviours(Module::module()) -> [atom()].
behaviours(Module) ->
    [Name|| {behaviour, [Name]} <- attributes(Module)].

-spec optional_callback(Module::module(), Fun::atom(), Args::list()) -> 
          term().
optional_callback(Module, Fun, Args) ->
    optional_callback(Module, Fun, Args, success()).

-spec optional_callback(Module::module(), Fun::atom(), Args::[term()], Def::term()) ->
          term().
optional_callback(Module, Fun, Args, Def) ->
    case erlang:function_exported(Module, Fun, length(Args)) of
        true ->
            erlang:apply(Module, Fun, Args);
        _  -> 
            Def 
    end.

-spec vsn(Module::module()) -> binary() | integer().
vsn(Module) ->
    {_, [Vsn]} = lists:keyfind('vsn', 1, attributes(Module)),
    Vsn.

%% Timestamp API

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(second) + erlang:time_offset(second).
