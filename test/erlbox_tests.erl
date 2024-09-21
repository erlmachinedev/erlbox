-module(erlbox_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").

erlbox_test_() ->
  State = #{},
  Reason = {test, _Stack = []},
  
  Acc = #{},
  
  [ {"Format (failure)", fun () -> R0 = erlbox:failure(),
                                   R1 = erlbox:failure(error),
                                   R2 = erlbox:failure(error, Reason),
                                   R3 = erlbox:failure(error, Reason, State),
                                        
                                   Fun = fun erlbox:is_failure/1,
                                        
                                   _ = inspect(Fun, _Res = [R0, R1, R2, R3])
                         end 
    },

    {"Format (success)", fun () -> R0 = erlbox:success(),
                                   R1 = erlbox:success(ok),
                                   R2 = erlbox:success(ok, State),
                                   R3 = erlbox:success(ok, State, Acc),

                                   Fun = fun erlbox:is_success/1,

                                   _ = inspect(Fun, _Res = [R0, R1, R2, R3])
                         end 
    }
  ].


inspect(Fun, Res) ->
  [true = Fun(X) || X <- Res].
 
