-module(erlbox_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").

erlbox_test_() ->
    { foreach,
      fun() -> ok
      end,

      fun (_) -> ok
      end,
      [ {"The format of failure", fun () -> R0 = erlbox:failure(),
                                            R1 = erlbox:failure(error),
                                            R2 = erlbox:failure(error, reason),
                                            R3 = erlbox:failure(error, reason, _State = #{}),

                                            [true = erlbox:is_failure(R) || R <- [R0, R1, R2, R3]]
                                  end },

        {"The format of success", fun () -> R0 = erlbox:success(),
                                            R1 = erlbox:success(ok),
                                            R2 = erlbox:success(ok, _State0 = #{}),
                                            R3 = erlbox:success(ok, _State1 = #{}, _Acc = #{}),

                                            [true = erlbox:is_success(R) || R <- [R0, R1, R2, R3]]
                                  end }
      ]
    }.
