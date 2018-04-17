-module('API_SUITE').

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([
  export_all,
  nowarn_export_all
]).

all() ->
  [
    %% Registration
    success_registration_test,
    double_registration_test,
    invalid_json_registration_test,
    %% Authorization
    success_authorization_test,
    no_user_authorization_test,
    wrong_password_authorization_test,
    invalid_json_authorization_test,
    %% Password changing
    success_password_changing_test,
    unauthorized_password_changing_test,
    other_user_password_changing_test,
    wrong_old_password_changing_test,
    invalid_json_password_changing_test,
    %% Receive users list
    success_receive_users_list_test,
    unauthorized_receive_users_list_test
  ].

%% Preparing

init_per_testcase(_, Config) ->
  application:ensure_all_started(erlang_aaa),
  application:ensure_all_started(gun),
  Config.

end_per_testcase(_, Config) ->
  application:stop(erlang_aaa),
  application:stop(gun),
  Config.

%% Registration

success_registration_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>).

double_registration_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  StreamRef_2 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"This username is already registered">>}),
  common_compare(ConnPid, StreamRef_2, 409, ExpectedData).


invalid_json_registration_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = <<"{\"123\"">>,
  StreamRef = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"Invalid json">>}),
  common_compare(ConnPid, StreamRef, 400, ExpectedData).

%% Authorization

success_authorization_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
  {<<"content-type">>, "application/json"}
  ], BodyJson),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  StreamRef_2 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  common_compare(ConnPid, StreamRef_2, 200, <<"token">>,
    fun(X) ->
      [K] = maps:keys(jiffy:decode(X, [return_maps])),
      K
    end).

no_user_authorization_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"User is doesn't registered now">>}),
  common_compare(ConnPid, StreamRef, 403, ExpectedData).

wrong_password_authorization_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson_1 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  BodyJson_2 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"wrongpassword">>}),
  StreamRef_2 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_2),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"Wrong password">>}),
  common_compare(ConnPid, StreamRef_2, 403, ExpectedData).

invalid_json_authorization_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = <<"{\"123\"">>,
  StreamRef = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"Invalid json">>}),
  common_compare(ConnPid, StreamRef, 400, ExpectedData).

%% Password changing

success_password_changing_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson_1 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  StreamRef_2 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  Self = self(),
  common_compare(ConnPid, StreamRef_2, 200, <<"token">>,
    fun(X) ->
      #{<<"token">> := Token} = jiffy:decode(X, [return_maps]),
      Self ! {token, Token},
      <<"token">>
    end),
  Token = receive {token, A} -> A end,
  BodyJson_2 = jiffy:encode(#{<<"old_password">> => <<"password">>, <<"new_password">> => <<"pass">>}),
  StreamRef_3 = gun:post(ConnPid, "/user/login", [
    {<<"content-type">>, "application/json"},
    {<<"authorization">>, "Bearer " ++ binary_to_list(Token)}
  ], BodyJson_2),
  common_compare(ConnPid, StreamRef_3, 200, <<"">>).

unauthorized_password_changing_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson = jiffy:encode(#{<<"old_password">> => <<"password">>, <<"new_password">> => <<"pass">>}),
  StreamRef = gun:post(ConnPid, "/user/login1", [
    {<<"content-type">>, "application/json"}
  ], BodyJson),
  common_compare(ConnPid, StreamRef, 401, <<"">>).

other_user_password_changing_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson_1 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  StreamRef_2 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  Self = self(),
  common_compare(ConnPid, StreamRef_2, 200, <<"token">>,
    fun(X) ->
      #{<<"token">> := Token} = jiffy:decode(X, [return_maps]),
      Self ! {token, Token},
      <<"token">>
    end),
  Token = receive {token, A} -> A end,
  BodyJson_2 = jiffy:encode(#{<<"old_password">> => <<"password">>, <<"new_password">> => <<"pass">>}),
  StreamRef_3 = gun:post(ConnPid, "/user/login1", [
    {<<"content-type">>, "application/json"},
    {<<"authorization">>, "Bearer " ++ binary_to_list(Token)}
  ], BodyJson_2),
  common_compare(ConnPid, StreamRef_3, 401, <<"">>).

wrong_old_password_changing_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson_1 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  StreamRef_2 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  Self = self(),
  common_compare(ConnPid, StreamRef_2, 200, <<"token">>,
    fun(X) ->
      #{<<"token">> := Token} = jiffy:decode(X, [return_maps]),
      Self ! {token, Token},
      <<"token">>
    end),
  Token = receive {token, A} -> A end,
  BodyJson_2 = jiffy:encode(#{<<"old_password">> => <<"wrongpassword">>, <<"new_password">> => <<"pass">>}),
  StreamRef_3 = gun:post(ConnPid, "/user/login", [
    {<<"content-type">>, "application/json"},
    {<<"authorization">>, "Bearer " ++ binary_to_list(Token)}
  ], BodyJson_2),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"Wrong old password">>}),
  common_compare(ConnPid, StreamRef_3, 403, ExpectedData).


invalid_json_password_changing_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson_1 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  StreamRef_2 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  Self = self(),
  common_compare(ConnPid, StreamRef_2, 200, <<"token">>,
    fun(X) ->
      #{<<"token">> := Token} = jiffy:decode(X, [return_maps]),
      Self ! {token, Token},
      <<"token">>
    end),
  Token = receive {token, A} -> A end,
  BodyJson_2 = <<"{\"123\"">>,
  StreamRef_3 = gun:post(ConnPid, "/user/login", [
    {<<"content-type">>, "application/json"},
    {<<"authorization">>, "Bearer " ++ binary_to_list(Token)}
  ], BodyJson_2),
  ExpectedData = jiffy:encode(#{<<"error">> => <<"Invalid json">>}),
  common_compare(ConnPid, StreamRef_3, 400, ExpectedData).

%% Receive users list

success_receive_users_list_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  BodyJson_1 = jiffy:encode(#{<<"user">> => <<"login">>, <<"password">> => <<"password">>}),
  StreamRef_1 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  common_compare(ConnPid, StreamRef_1, 200, <<"">>),
  BodyJson_2 = jiffy:encode(#{<<"user">> => <<"login1">>, <<"password">> => <<"password">>}),
  StreamRef_2 = gun:post(ConnPid, "/user/registration/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_2),
  common_compare(ConnPid, StreamRef_2, 200, <<"">>),
  StreamRef_3 = gun:post(ConnPid, "/user/auth/", [
    {<<"content-type">>, "application/json"}
  ], BodyJson_1),
  Self = self(),
  common_compare(ConnPid, StreamRef_3, 200, <<"token">>,
    fun(X) ->
      #{<<"token">> := Token} = jiffy:decode(X, [return_maps]),
      Self ! {token, Token},
      <<"token">>
    end),
  Token = receive {token, A} -> A end,
  StreamRef_4 = gun:get(ConnPid, "/user", [
    {<<"accept">>, "application/json"},
    {<<"authorization">>, "Bearer " ++ binary_to_list(Token)}]),
  ExpectedData = jiffy:encode(#{<<"users">> => [<<"login">>, <<"login1">>]}),
  common_compare(ConnPid, StreamRef_4, 200, ExpectedData).

unauthorized_receive_users_list_test(_Config) ->
  {ok, ConnPid} = gun:open("localhost", 8080, #{connect_timeout => infinity, retry => 0}),
  StreamRef = gun:get(ConnPid, "/user", [
    {<<"accept">>, "application/json"}]),
  common_compare(ConnPid, StreamRef, 401, <<"">>).


%% Utils

common_compare(ConnPid, StreamRef, ExpectedStatus, ExpectedData, ActualDataMutator) ->
  receive
    {gun_up, ConnPid, _} -> common_compare(ConnPid, StreamRef, ExpectedStatus, ExpectedData, ActualDataMutator);
    {gun_response, ConnPid, StreamRef, fin, ActualStatus, _Headers} ->
      ?assertEqual(ExpectedStatus, ActualStatus);
    {gun_response, ConnPid, StreamRef, nofin, ActualStatus, _Headers} ->
      ?assertEqual(ExpectedStatus, ActualStatus),
      ?assertEqual(ExpectedData, ActualDataMutator(data_loop(ConnPid, StreamRef, <<"">>)));
    A -> error({nomatch, A})
  end.

common_compare(ConnPid, StreamRef, ExpectedStatus, ExpectedData) ->
  receive
    {gun_up, ConnPid, _} -> common_compare(ConnPid, StreamRef, ExpectedStatus, ExpectedData);
    {gun_response, ConnPid, StreamRef, fin, ActualStatus, Headers} ->
      case ActualStatus of
        405 ->
          error(Headers);
        _ -> ok
      end,
      ?assertEqual(ExpectedStatus, ActualStatus);
    {gun_response, ConnPid, StreamRef, nofin, ActualStatus, _Headers} ->
      ?assertEqual(ExpectedStatus, ActualStatus),
      ?assertEqual(ExpectedData, data_loop(ConnPid, StreamRef, <<"">>));
    A -> error({nomatch, A})
  end.

data_loop(Pid, Ref, Acc) ->
  receive
    {gun_data, Pid, Ref, nofin, Data} ->
      data_loop(Pid, Ref, <<Acc/binary, Data/binary>>);
    {gun_data, Pid, Ref, fin, Data} ->
      <<Acc/binary, Data/binary>>
  after 5000 ->
    error(timeout)
  end.