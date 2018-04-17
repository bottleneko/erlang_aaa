-module(users_utils).

-include("users.hrl").

%% API
-export([
  reply_by_json_parse_result/2,
  parse_account/1,
  is_alphanumeric/1]).

-spec reply_by_json_parse_result(ParseResult :: atom(), Req :: cowboy_req:req()) ->
  NewReq :: cowboy_req:req().
reply_by_json_parse_result(ParseResult, Req) ->
  case ParseResult of
    invalid_json ->
      ErrorJson = jiffy:encode(#{<<"error">> => <<"Invalid json">>}),
      cowboy_req:reply(400, #{}, ErrorJson, Req);
    unexpected_data ->
      ErrorJson = jiffy:encode(#{<<"error">> => <<"Data was in an unexpected format">>}),
      cowboy_req:reply(400, #{}, ErrorJson, Req);
    not_is_alphanumeric ->
      ErrorJson = jiffy:encode(#{<<"error">> => <<"Values must be alphanumeric">>}),
      cowboy_req:reply(400, #{}, ErrorJson, Req)
  end.

-spec parse_account(Json :: binary()) ->
  #account{} | atom().
parse_account(Json) ->
  try jiffy:decode(Json) of
    {[{<<"user">>, Username}, {<<"password">>, Password}]} ->
      validate_account(#account{username = Username, password = Password});
    _ ->
      unexpected_data
  catch
    _ -> invalid_json
  end.

-spec validate_account(Account :: #account{}) ->
  #account{} | not_is_alphanumeric.
validate_account(Account) ->
  ValidateResult =
    is_alphanumeric(Account#account.username) andalso
      is_alphanumeric(Account#account.password),
  case ValidateResult of
    true ->
      Account;
    false ->
      not_is_alphanumeric
  end.

-spec is_alphanumeric(Binary :: binary()) ->
  boolean().
is_alphanumeric(Binary) ->
  Binary =:= re:replace(Binary, "[^A-Za-z0-9]", "", [global, {return, binary}]).
