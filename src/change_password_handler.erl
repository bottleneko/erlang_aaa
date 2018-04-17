-module(change_password_handler).

-export([
  init/2,
  content_types_accepted/2,
  allowed_methods/2,
  change_password_from_json/2,
  is_authorized/2]).

-spec init(Req :: cowboy_req:req(), Opts :: [any()]) ->
  {cowboy_rest, cowboy_req:req(), [any()]}.
init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) ->
  {[binary()], cowboy_req:req(), any()}.
allowed_methods(Req, State) ->
  {[<<"PUT">>], Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: any()) ->
  {[{binary(), atom()}],
  cowboy_req:req(), any()}.
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, change_password_from_json}
  ], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: any()) ->
  {true, cowboy_req:req(), any()} | {{false, binary()}, cowboy_req:req(), any()}.
  is_authorized(Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {bearer, Token} ->
      reply_by_token_access(Token, Req, State);
    _ ->
      {{false, <<"Basic realm=\"localhost\"">>}, Req, State}
  end.

-spec reply_by_token_access(Token :: binary(), Req :: cowboy_req:req(), State :: any()) ->
  {true, cowboy_req:req(), any()} | {{false, binary()}, cowboy_req:req(), any()}.
reply_by_token_access(Token, Req, State) ->
  ActualUsername = cowboy_req:binding(login, Req),
  {session, ExpectedUsername} = users_db:get_user_by_token(Token),
  case ActualUsername =:= ExpectedUsername of
    true ->
      {true, Req, State};
    false ->
      {{false, <<"Basic realm=\"localhost\"">>}, Req, State}
  end.

-spec change_password_from_json(Req :: cowboy_req:req(), State :: any()) ->
  {stop, cowboy_req:req(), any()}.
change_password_from_json(Req, State) ->
  {ok, Json, Req2} = cowboy_req:read_body(Req),
  Req3 =
    case parse_change_password(Json) of
      ParseStatus when is_atom(ParseStatus) ->
        users_utils:reply_by_json_parse_result(ParseStatus, Req2);
      Passwords when is_tuple(Passwords) ->
        reply_by_password_change_result(Passwords, Req2)
    end,
  {stop, Req3, State}.

-spec reply_by_password_change_result(Passwords :: {binary(), binary()}, Req :: cowboy_req:req()) ->
  cowboy_req:req().
reply_by_password_change_result(Passwords, Req) ->
  Username = cowboy_req:binding(login, Req),
  Response = users_db:change_password(Username, Passwords),
  case Response of
    wrong_password ->
      ErrorJson = jiffy:encode(#{<<"error">> => <<"Wrong old password">>}),
      cowboy_req:reply(403, #{}, ErrorJson, Req);
    ok ->
      cowboy_req:reply(200, Req)
  end.

-spec parse_change_password(Json :: binary()) ->
  {binary(), binary()} | unexpected_data | invalid_json | not_is_alphanumeric.
parse_change_password(Json) ->
  try jiffy:decode(Json) of
    {[{<<"old_password">>, Username}, {<<"new_password">>, Password}]} ->
      validate_user_data({Username, Password});
    _ -> unexpected_data
  catch
    _ -> invalid_json
  end.

-spec validate_user_data(Passwords :: {binary(), binary()}) ->
  {binary(), binary()} | not_is_alphanumeric.
validate_user_data(Passwords = {OldPassword, NewPassword}) ->
  ValidateResult =
    users_utils:is_alphanumeric(OldPassword) andalso
      users_utils:is_alphanumeric(NewPassword),
  case ValidateResult of
    true ->
      Passwords;
    false ->
      not_is_alphanumeric
  end.