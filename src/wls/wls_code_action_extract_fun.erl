-module(wls_code_action_extract_fun).

-behaviour(wls_code_actions).

-export([ is_default/0
        , id/0
        , title/0
        , user_input/0
        , init/3
        , command_args/3
        , execute_command/1
        ]).

-include_lib("wls_core.hrl").
-include_lib("wrangler_internal.hrl").
-include_lib("kernel/include/logger.hrl").

-spec is_default() -> boolean().
is_default() -> false.

-spec id() -> action_id().
id() -> <<"extract-fun">>.

-spec title() -> binary().
title() -> <<"Extract Function">>.

-spec user_input() -> #{syntax_type() => description()}.
user_input() -> #{'atom' => <<"New function name">>}.

-spec init(path(), range(), syntaxTree()) -> {true, state()} | false.
init(_Path, {StartPos, EndPos}, AST) ->
  case api_interface:pos_to_expr_list(AST, StartPos, EndPos) of
    [] -> 
      false;
    ExpList ->
      {true, #{}}
  end.


-spec command_args(path(), range(), state()) -> command_args().
command_args(Path, Range, _State) ->
  #{ 'range' => Range
    ,'path'  => Path
  }.



-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := {StartPos, EndPos}
                  , <<"path">>  := Path 
                }]) ->
  ?LOG_INFO("Using default variable name: NewName"),
  extract(binary_to_list(Path), StartPos, EndPos, "NewName"),
  [];
execute_command([#{ <<"range">> := {StartPos, EndPos}
                  , <<"path">>  := Path 
                  }, NewName]) ->
  extract(binary_to_list(Path), StartPos, EndPos, binary_to_list(NewName)),
  [].


%%==============================================================================
%% Private Functions
%%==============================================================================

extract(Path, StartPos, EndPos, NewName) ->
  try refac_new_fun:fun_extraction(Path, StartPos, EndPos, NewName, wls, ?DEFAULT_TABWIDTH) of
    {ok, [{OldPath, _NewPath, Text}]} ->
      Edit = #{
        documentChanges => [
            wls_utils:text_document_edit(OldPath, Text)
          ]
        },
      wls_utils:apply_edit(Edit);
    Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
       ?LOG_INFO("Error extracting fun: ~p", [Err])
  catch
    _:{error, Message} -> wls_utils:send_warning(Message);
    _:{warning, Message} -> wls_utils:send_warning(Message);
    _:E -> ?LOG_INFO("Error extracting fun: ~p", [E])
  end.
