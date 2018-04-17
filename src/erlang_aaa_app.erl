-module(erlang_aaa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(StartType :: atom(), StartArgs :: [term()]) ->
  {ok, pid()}.
start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/user", show_users_handler, []},
      {"/user/registration", registration_handler, []},
      {"/user/auth", auth_handler, []},
      {"/user/[:login]", change_password_handler, []}
    ]}
  ]),
  Sup = erlang_aaa_sup:start_link(),
  Port = application:get_env(erlang_aaa, cowboy_port, 8080),
  {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
    env => #{dispatch => Dispatch}
  }),
  Sup.

-spec stop(_State :: any()) ->
  ok.
stop(_State) ->
  cowboy:stop_listener(http).
