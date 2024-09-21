-module(erlbox_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").

erlbox_test_() ->
  [ {"The format (failure)", fun () -> R0 = erlbox:failure(),
                                        R1 = erlbox:failure(error),
                                        R2 = erlbox:failure(error, reason),
                                        R3 = erlbox:failure(error, reason, _State = #{}),
                                        
                                        Fun = fun erlbox:is_failure/1,
                                        Res = [R0, R1, R2, R3],
                                        
                                        _ = inspect(Fun, Res)
                              end },

    {"The format (success)", fun () -> R0 = erlbox:success(),
                                        R1 = erlbox:success(ok),
                                        R2 = erlbox:success(ok, _State0 = #{}),
                                        R3 = erlbox:success(ok, _State1 = #{}, _Acc = #{}),

                                        Fun = fun erlbox:is_success/1,
                                        Res = [R0, R1, R2, R3],
                                        
                                        _ = inspect(Fun, Res)
                              end 
    }
  ].


inspect(Fun, Res) ->
  [true = Fun(X) || X <- Res].
 
