%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 处理加密、签名的进程
%%%
%%% @end
%%% Created : 22. Aug 2016 21:36
%%%-------------------------------------------------------------------
-module(enc_worker).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-author("simonxu").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
				 handle_call/3,
				 handle_cast/2,
				 handle_info/2,
				 terminate/2,
				 code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, xf_encryptor).
-define(KEYS_ROOT, "keys").

-type platform() :: up | wx_scan_citic.
-type mcht_id() :: non_neg_integer().
-type txn_type() :: req | resp.
-type keys() :: {platform(), mcht_id(), txn_type()}.
-type v_keys() :: binary().
-type m_keys() :: #{keys() => v_keys()}.
-record(state, {m_keys}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #state{}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
									State :: #state{}) ->
									 {reply, Reply :: term(), NewState :: #state{}} |
									 {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
									 {noreply, NewState :: #state{}} |
									 {noreply, NewState :: #state{}, timeout() | hibernate} |
									 {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
									 {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, State) ->
	%%% load keystore from keys root dir
	RootDir = keys_dir_root(),
	KeyStore = load_keystore(RootDir),
	{noreply, #state{m_keys = KeyStore}};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
								State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
									Extra :: term()) ->
									 {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec priv_dir() -> file:filename().
priv_dir() ->
	Application = case application:get_application() of
									undefined -> ?APP;
									{ok, App} -> App
								end,
	lager:debug("Application=~p", [Application]),
	code:priv_dir(Application).


%%--------------------------------------------------------------------
-spec keys_dir_root() -> file:filename().
keys_dir_root() ->
	KeysDir = case application:get_env(keys_dir_root) of
							undefined -> [priv_dir(), "/", ?KEYS_ROOT];
							{ok, Dir} -> Dir
						end,
	lager:info("The root keystore directory = ~p", [KeysDir]),
	KeysDir.


%%--------------------------------------------------------------------
-spec load_keystore(RootDir) -> KeyStore when
	RootDir :: string(),
	KeyStore :: m_keys().

load_keystore(RootDir) ->
	PlatformList = get_platform_list(RootDir),
	ok.


%%--------------------------------------------------------------------
foldl_dir(Dir, Fun) ->
	{ok, DirList} = file:list_dir_all(Dir),
	[Fun(Dir, Item)
	 || Item <- DirList, lists:nth(1, Item) =/= $.].

%%--------------------------------------------------------------------
get_platform_list(RootDir) ->
	F = fun(Dir, Platform) ->
		{[Dir, "/", Platform], list_to_existing_atom(Platform)}
			end,

	foldl_dir(RootDir, F).

%%%===================================================================
%%% test functions
%%%===================================================================
