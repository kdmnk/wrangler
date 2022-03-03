
%% Code Actions: Behaviour and API
%%==============================================================================

-module(wls_code_actions).

%%==============================================================================
%% Callback Functions
%%==============================================================================

-callback is_default() -> boolean().
-callback title() -> binary().
-callback id() -> binary().

-callback init(els_core:uri(), els_core:range()) -> state().
-callback precondition(els_core:uri(), els_core:range()) -> boolean().

-callback command_args(els_core:uri(), els_core:range(), state()) -> map().
-callback execute_command([any()]) -> [map()].

-optional_callbacks([ init/2
                    , precondition/2
                    ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ enabled_actions/0
        , get_actions/2 
        , execute_command/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================

-include("wls_core.hrl").

%%==============================================================================
%% API
%%==============================================================================


-spec get_actions(els_core:uri(), els_core:range()) -> [code_action()].
get_actions(Doc, Range) ->
  Pred = fun (Action) -> Action /= null end,
  lists:filter(Pred, [actions(Id, Doc, Range) || Id <- enabled_actions()]).

-spec available_actions() -> [action_id()].
available_actions() ->
  [ <<"generalise-fun">>
  ].

-spec default_actions() ->  [action_id()].
default_actions() ->
  [Id || Id <- available_actions(), (cb_module(Id)):is_default()].

-spec enabled_actions() ->  [action_id()].
enabled_actions() ->
  %%TODO
  %Config = els_config:get(actions),
  %Default = default_actions(),
  %Enabled = maps:get("enabled", Config, []),
  %Disabled = maps:get("disabled", Config, []),
  %lists:usort((Default ++ valid(Enabled)) -- valid(Disabled)).
  default_actions().

-spec actions(action_id(), els_core:uri(), els_core:range()) ->  code_action() | null.
actions(Id, Uri, Range) ->
  CbModule = cb_module(Id),
  case precondition(CbModule, Uri, Range) of
    true ->
      State = case erlang:function_exported(CbModule, init, 1) of
                true ->
                  CbModule:init(Uri, Range);
                false ->
                  'state_not_initialized'
              end,
      make_action(CbModule, Uri, Range, State);
    false ->
      null
  end.

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(Command, Arguments) -> 
  CbModule = cb_module(Command),
  CbModule:execute_command(Arguments).


%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_action(module(), els_core:uri(), els_core:range(), state()) -> code_action().
make_action(CbModule, Uri, Range, State) ->
  #{ title       => CbModule:title()
   , kind        => ?CODE_ACTION_KIND_REFACTOR
   , command     => els_command:make_command(CbModule:title(), CbModule:id(), CbModule:command_args(Uri, Range, State))
  }.


%% @doc Return the callback module for a given Code Action Identifier
-spec cb_module(action_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"wls_code_action_", Id/binary>>, utf8).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec precondition(atom(), els_core:uri(), wls_utils:range()) -> boolean().
precondition(CbModule, Uri, Range) ->
  case erlang:function_exported(CbModule, precondition, 2) of
    true ->
      CbModule:precondition(Uri, Range);
    false ->
      true
  end.
