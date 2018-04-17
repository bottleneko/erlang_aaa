-module(show_users_handler).

%% API
-export([
  init/2,
  content_types_provided/2,
  users_to_json/2,
  is_authorized/2]).

-spec init(Req :: cowboy_req:req(), Opts :: [any()]) ->
  {cowboy_rest, cowboy_req:req(), [any()]}.
init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) ->
  {[{binary(), atom()}], cowboy_req:req(), any()}.
content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, users_to_json}
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
  {true, cowboy_req:req(), any()}.
reply_by_token_access(Token, Req, State) ->
  {session, _ExpectedUsername} = users_db:get_user_by_token(Token),
  {true, Req, State}.

-spec users_to_json(Req :: cowboy_req:req(), State :: any()) ->
  {[binary()], cowboy_req:req(), any()}.
users_to_json(Req, State) ->
  Users = users_db:get_users_list(),
  UsersJson = jiffy:encode(#{<<"users">> => Users}),
  {UsersJson, Req, State}.
