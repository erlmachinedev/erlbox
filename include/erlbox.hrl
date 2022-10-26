-type failure() :: erlbox:failure().
-type failure(E) :: erlbox:failure(E).
-type failure(E, R) :: erlbox:failure(E, R).
-type failure(E, R, S) :: erlbox:failure(E, R, S).

-type success(Res) :: erlbox:success(Res).
-type success(Res, S) :: erlbox:successs(Res, S).
-type success(Res, S, A) :: erlbox:successs(Res, S, A).
-type success() ::  erlbox:successs().

