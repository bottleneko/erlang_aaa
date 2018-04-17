-module(erlang_aaa_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-dialyzer({nowarn_function, init/1}).
-dialyzer(no_improper_lists).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) ->
  {ok,{
  #{intensity => non_neg_integer(),
    period => pos_integer(),
    strategy => one_for_all | one_for_one | rest_for_one | simple_one_for_one},
    [#{
      id := _,
      start := {atom(),atom(),undefined | [any()]},
      modules => dynamic | [atom()],
      restart => permanent | temporary | transient,
      shutdown => brutal_kill | infinity | non_neg_integer(),
      type => supervisor | worker}
    ]
  }}.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{
        id => users_db,
        start => {users_db, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [users_db]
        }],
    {ok, {SupFlags, ChildSpecs}}.
