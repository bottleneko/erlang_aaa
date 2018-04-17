-module(users_db).

-behaviour(gen_server).

-include("users.hrl").

%% API
-export([
  start_link/0,
  user_registration/1,
  get_users_list/0,
  check_account/1,
  change_password/2,
  new_session/1,
  get_user_by_token/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {
  accounts_tid,
  sessions_tid
}).


%%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec user_registration(Account :: #account{}) ->
  ok | already_exist.
user_registration(Account) ->
  gen_server:call(?SERVER, {register, Account}).

-spec get_users_list() ->
  [binary()].
get_users_list() ->
  gen_server:call(?SERVER, get_users_list).

-spec check_account(Account :: #account{}) ->
  user_not_exist | ok | wrong_password.
check_account(Account) ->
  gen_server:call(?SERVER, {check_account, Account}).

-spec change_password(Username :: binary(), Passwords :: {binary(), binary()}) ->
  ok | wrong_password.
change_password(Username, Passwords) ->
  gen_server:call(?SERVER, {change_password, Username, Passwords}).

-spec new_session(Username :: binary()) ->
  {token, binary()}.
new_session(Username) ->
  gen_server:call(?SERVER, {new_session, Username}).

-spec get_user_by_token(Token :: binary()) ->
  {session, binary()} | no_session.
get_user_by_token(Token) ->
  gen_server:call(?SERVER, {get_user_by_token, Token}).

%%% Callbacks

-spec init([]) ->
  {ok, #state{}}.
init([]) ->
  {ok, #state{
    accounts_tid = ets:new(accounts, [ordered_set, private, {keypos, 2}]),
    sessions_tid = ets:new(sessions, [ordered_set, private, {keypos, 2}])
    }}.

-spec handle_call(Request, _From :: {pid(), term()}, State :: #state{}) -> Reply when
  Request ::
    {register, #account{}} |
    get_users_list |
    {check_account, #account{}} |
    {change_password, binary(), tuple()} |
    {new_session, binary()} |
    {get_user_by_session, binary()},
  Reply ::
    {reply, Data, State} |
    {noreply, State},
  Data ::
    ok |
    already_exist |
    user_not_exist |
    wrong_password |
    no_session |
    {session, binary()} |
    {token, binary()} |
    [binary()].
handle_call({register, Account}, _From, State = #state{accounts_tid = Tid}) ->
  Reply =
    case ets:insert_new(Tid, Account) of
      true -> ok;
      false -> already_exist
    end,
  {reply, Reply, State};
handle_call(get_users_list, _From, State = #state{accounts_tid = Tid}) ->
  UserAccounts = ets:tab2list(Tid),
  Usernames = lists:map(fun(X) -> X#account.username end, UserAccounts),
  {reply, Usernames, State};
handle_call({check_account, #account{
  username = Username,
  password = Password}},
    _From,
    State = #state{accounts_tid = Tid}) ->
  Reply =
    case ets:lookup(Tid, Username) of
      [] ->
        user_not_exist;
      [#account{username = Username, password = Password}] ->
        ok;
      [#account{username = Username, password = _}] ->
        wrong_password
    end,
  {reply, Reply, State};
handle_call({change_password, Username, {OldPassword, NewPassword}}, _From, State = #state{accounts_tid = Tid}) ->
  Reply =
    case ets:lookup(Tid, Username) of
      [#account{username = Username, password = OldPassword}] ->
        ets:insert(Tid, {Username, NewPassword}),
        ok;
      [#account{username = Username, password = _}] ->
        wrong_password
    end,
  {reply, Reply, State};
handle_call({new_session, Username}, _From, State = #state{sessions_tid = Tid}) ->
  Token = generate_token(),
  ets:insert(Tid, #session{token = Token, username = Username}),
  {reply, {token, Token}, State};
handle_call({get_user_by_token, Token}, _From, State = #state{sessions_tid = Tid}) ->
  Reply =
    case ets:lookup(Tid, Token) of
      [] ->
        no_session;
      [#session{token = Token, username = Username}] ->
        {session, Username}
    end,
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

-spec handle_cast(Request :: any(), State :: #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(Info :: any(), State :: #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(Reason :: atom(), State :: #state{}) -> any().
terminate(_Reason, #state{
  accounts_tid = AccountsTid,
  sessions_tid = SessionsTid}) ->
  ets:delete(AccountsTid),
  ets:delete(SessionsTid).

%%% Utils

-spec generate_token() -> binary().
generate_token()->
  <<Tail:10/binary, _/binary>> = base64:encode(crypto:strong_rand_bytes(10)),
  binary:replace(Tail, [<<"/">>, <<"+">>], <<"A">>, [global]).