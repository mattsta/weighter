-module(weighter_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
weighter_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Create new user at epoch 1",
       fun create_user_1/0},
     {"Reply new user at epoch 3000",
       fun create_user_3000/0},
     {"Sign up new user with parent of 1",
       fun create_user_parent_1/0},
     {"Check weight of 1",
       fun weight_check_1/0},
     {"Check weight of 3000",
       fun weight_check_3000/0},
     {"Check weight user with parent of 1",
       fun weight_check_parent_1/0},
     {"Use half weight of 3000 user",
       fun use_half_of_user_3000s_weight/0},
     {"Use more than all weight of 3000 user",
       fun use_more_than_remaining_user_3000s_weight/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_user_1() ->
  Result = weighter:setup_user(user1, vampire, 1, none),
  ResultBad = weighter:setup_user(user1, vampire, 1, user1),
  ResultExists = weighter:setup_user(user1, vampire, 1, none),
  ?assertEqual(707106, Result),
  ?assertEqual(cant_inherit_from_self, ResultBad),
  ?assertEqual(user_already_setup, ResultExists).

create_user_3000() ->
  Result = weighter:setup_user(user3000, vampire, 3000, none),
  ?assertEqual(64024, Result).

create_user_parent_1() ->
  Result = weighter:setup_user(userParent1, vampire, 3000, user1),
  ?assertEqual(64024, Result).

weight_check_1() ->
  Result = weighter:weight_for_user(user1, vampire),
  ?assertEqual(707106, Result).

weight_check_3000() ->
  Result = weighter:weight_for_user(user3000, vampire),
  ?assertEqual(64024, Result).

weight_check_parent_1() ->
  Result = weighter:weight_for_user(userParent1, vampire),
  ?assertEqual(417577, Result).

use_half_of_user_3000s_weight() ->
  Half = trunc(64024/2),
  Result = weighter:use_weight(user3000, vampire, Half),
  ?assertEqual(Half, Result).

use_more_than_remaining_user_3000s_weight() ->
  Half = trunc(64024/2),
  Result = weighter:use_weight(user3000, vampire, 9999999999),
  ?assertEqual(Half, Result).

%%%----------------------------------------------------------------------
%%% Set it up, tear it down
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(redis_weighter, "127.0.0.1", 6389),
  er:flushall(redis_weighter).

teardown(_) ->
  application:stop(er).
