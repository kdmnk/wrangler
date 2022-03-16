
%% Code Actions: Behaviour and API
%%==============================================================================

-module(wls_code_actions).

%%==============================================================================
%% Callback Functions
%%==============================================================================

-callback is_default() -> boolean().

-callback id() -> action_id().

%% @doc Return the title that is displayed in the code action menu.
-callback title() -> binary().

%% @doc Precondition check + return a state.
-callback init(path(), range(), syntaxTree()) -> {true, state()} | false.

%% @doc Indicate additional user input, if needed. Not used yet.
-callback user_input() -> #{syntax_type() => description()}.

%% @doc Get the arguments for the code action`s command.
-callback command_args(path(), range(), state()) -> command_args().

%% @doc Execute the command.
-callback execute_command([command_args()]) -> [map()].

-optional_callbacks([ user_input/0 ]).

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
-include("wrangler_internal.hrl").

%%==============================================================================
%% API
%%==============================================================================

-spec get_actions(els_core:uri(), els_core:range()) -> [code_action()].
get_actions(Uri, Range) ->
  Path = wls_utils:path(Uri),
  WLS_Range = wls_utils:range(Range),
  case wrangler_ast_server:parse_annotate_file(Path, true) of
    {ok, {AnnAST, _Info}} ->
      Pred = fun (Action) -> Action /= null end,
      lists:filter(Pred, [ actions(Id, Path, WLS_Range, AnnAST) || Id <- enabled_actions()]);
    _ -> []
  end.

-spec available_actions() -> [action_id()].
available_actions() ->
  [% <<"generalise-fun">>
  <<"extract-fun">>
, <<"test">>
  % , <<"new-macro">>
  % , <<"new-var">>
  % , <<"rename-fun">>
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

-spec actions(action_id(), path(), range(), syntaxTree()) ->  code_action() | null.
actions(Id, Path, Range, AST) ->
  CbModule = cb_module(Id),
  case CbModule:init(Path, Range, AST) of
    {true, State} ->
      make_action(CbModule, Path, Range, State);
    false -> null
  end.

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(Command, Arguments) -> 
  CbModule = cb_module(Command),
  CbModule:execute_command(Arguments).


%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_action(module(), path(), range(), state()) -> code_action().
make_action(CbModule, Path, Range, State) ->
  #{ title       => CbModule:title()
   , kind        => ?CODE_ACTION_KIND_REFACTOR
   , command     => els_command:make_command(CbModule:title(), CbModule:id(), [CbModule:command_args(Path, Range, State)])
  }.


%% @doc Return the callback module for a given Code Action Identifier
-spec cb_module(action_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"wls_code_action_", Id/binary>>, utf8).
