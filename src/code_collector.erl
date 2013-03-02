%% Copyright
-module(code_collector).

%% API
-export([start/2, start/1, start/3, start_ok/2, start_ok/3]).

-record(state, {
  timeout = 5000,
  ref :: reference(),
  pid :: pid(),
  answers = [],
  counters = 0
}).

start(Fun) ->
  start(Fun, 5000).

-spec start(Fun :: [ { Key :: term(), Fun :: fun((term()) -> term()) , Data :: tuple() } ], Timeout :: integer()) ->
    {ok, [ { Key :: term(), Answer :: term() } ] } | % Полное завершение получены все ответы
    { error, timeout, Answers :: [ Answer :: term() ] } | % Получены ответы частично
    { error, timeout }. % Ответы не получены вообще
start(Fun, Timeout) when is_integer(Timeout) ->
  Pid = spawn_link(fun loop_1_init/0),
  Ref = make_ref(),
  Pid ! {init, Ref, self(), Fun, Timeout},
  receive
    { answer, Ref, Answer } -> {ok, Answer}
  after Timeout ->
    Pid ! { state, Ref },
    receive
      { answer, Ref, Answers} -> {error, timeout, Answers}
    after Timeout ->
      { error, timeout }
    end
  end;

start(Fun, Data) when is_list(Data) ->
  start(Fun, Data, 5000).

start(Fun, Data, Timeout) ->
  start([
    case Item of
      { Key, Value } -> { Key, Fun, Value };
      Value -> { Fun, Value }
    end || Item <- Data], Timeout).

start_ok(Fun, Data, Timeout) ->
  Result = case start(Fun, Data, Timeout) of
    {ok, Res} -> Res;
    {error, timeout, Res } -> Res;
    _ -> timeout
  end,
  case Result of
    timeout -> timeout;
    A -> [ X || X<-[
        case Item of
          { Key, { ok , R } } -> { Key, R };
          { ok, R } -> R;
          _ -> none
        end
        || Item <- A ], X =/= none ]
  end.

start_ok(Fun, Data) ->
  start_ok(Fun, Data,5000).


loop_1_init() ->
  loop_1(#state{
     timeout = 5000
  }).

loop_1(State) ->
  receive
    { init, Ref, Pid, Fun, Timeout } ->
      lists:map(fun(Item) ->
        Pid_item = spawn_link(fun loop_2/0),
        Pid_item ! {init,{self(), Ref,  Item } }
      end, Fun),
      loop_1(State#state{
        timeout = Timeout,
        ref = Ref,
        pid = Pid,
        answers = [],
        counters = length(Fun)
      });
    { answer, Ref , Result } when Ref == State#state.ref->
      NewState = State#state{
        answers = [ Result | State#state.answers ],
        counters = State#state.counters - 1
      },
      if
        NewState#state.counters > 0 -> loop_1(NewState);
        true -> NewState#state.pid ! { answer, NewState#state.ref, NewState#state.answers }
      end;
    { state, Ref } when Ref == State#state.ref -> State#state.pid ! { answer, State#state.ref, State#state.answers }
  after State#state.timeout ->
    State#state.pid ! { answer, State#state.ref, State#state.answers }
  end.

loop_2() ->
  receive
    {init, { Pid, Ref, { Key, Func, Data } } } ->
      Pid ! { answer, Ref, {Key, Func(Data) } };
    {init, { Pid, Ref, { Func, Data } } } ->
      Pid ! { answer, Ref, Func(Data) };
    _ -> error
  after 10000 ->
    error
  end.


