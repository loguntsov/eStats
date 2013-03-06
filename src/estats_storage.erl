%% Copyright
-module(estats_storage).

-behaviour(gen_server).

%% API
-export([start_link/0, put/2, get/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link(?MODULE, {}, []).

%% gen_server callbacks
-record(state, {
  set :: atom()
}).


init({}) ->
  gproc:add_local_name(estats_date_manager),
  {ok, Table} = dets:open_file(dets_date_manager, [{type, set}, {file , "data/storage/set"},{auto_save, 180*1000}, {repair, true}, {ram_file, true}]),
  {ok, #state{
    set = Table
  }}.

-spec put(Key :: term(), Value :: term() ) -> ok | {error, Reason :: term()}.
put(Key, Value) ->
  call({set, Key, Value }).

-spec get(Key :: term()) -> Value :: term() | undefined.
get(Key) ->
  call({get, Key}).

-spec call(Msg :: term()) -> term().
call(Msg) ->
  gen_server:call(pid(), Msg).

-spec pid() -> pid().
pid() ->
  gproc:lookup_local_name(estats_date_manager).

handle_call({set, Key, Value}, _From, State) ->
  Is_new = dets:insert(State#state.set, {Key, Value}),
  dets:sync(State#state.set),
  {reply, Is_new, State};

handle_call({get, Key}, _From, State) ->
  { reply,
    case dets:lookup(State#state.set, Key) of
    [ ] -> undefined;
    [ { _, Value } ] ->  Value
    end,
    State };

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
