-module(erlbox).
-export([]).

-export([failure/0, failure/1, failure/2, failure/3]).
-export([success/0, success/1, success/2, success/3]).

-export([is_success/1, is_failure/1]).

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
