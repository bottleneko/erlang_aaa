-module(auth_handler).

%% Cowboy REST callbacks
-export([
  init/2,
  allowed_methods/2,
  content_types_accepted/2,
  authorization_from_json/2]).

-include("users.hrl").

-spec init(Req :: cowboy_req:req(), Opts :: [any()]) ->
  {cowboy_rest, cowboy_req:req(), [any()]}.
init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) ->
  {[binary()], cowboy_req:req(), any()}.
allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) ->
  {[{binary(), atom()}],
    cowboy_req:req(), any()}.
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, authorization_from_json}
  ], Req, State}.

-spec authorization_from_json(Req :: cowboy_req:req(), State :: any()) ->
  {stop, cowboy_req:req(), any()}.
authorization_from_json(Req, State) ->
  {ok, Json, Req2} = cowboy_req:read_body(Req),
  Req3 =
    case users_utils:parse_account(Json) of
      ParseStatus when is_atom(ParseStatus) ->
        users_utils:reply_by_json_parse_result(ParseStatus, Req2);
      Account when is_tuple(Account) ->
        reply_by_is_registered_result(Account, Req2)
    end,
  {stop, Req3, State}.

-spec reply_by_is_registered_result(Account :: #account{}, Req :: cowboy_req:req()) ->
  cowboy_req:req().
reply_by_is_registered_result(Account, Req) ->
  Response = users_db:check_account(Account),
  case Response of
    user_not_exist ->
      ErrorJson = jiffy:encode(#{<<"error">> => <<"User is doesn't registered now">>}),
      cowboy_req:reply(403, #{}, ErrorJson, Req);
    wrong_password ->
      ErrorJson = jiffy:encode(#{<<"error">> => <<"Wrong password">>}),
      cowboy_req:reply(403, #{}, ErrorJson, Req);
    ok ->
      {token, Token} = users_db:new_session(Account#account.username),
      cowboy_req:reply(200, #{}, jiffy:encode(#{<<"token">> => Token}), Req)
  end.