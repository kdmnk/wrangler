-module(wls_utils).

-include_lib("wls_core.hrl").

-export([ range/1
        , pos/1
        , path/1
        , root_folder/0
        %, preview_candidates/3
        , create_file/1
        , rename_file/2
        , apply_edit/1
        , text_document_edit/2
        , text_edit/1
        , send_info/1
        , send_warning/1
        , send_error/1]).

%%==============================================================================
%% ELS to WLS representation
%%==============================================================================

-spec range(els_core:range()) -> range().
range(Range) ->
  #{<<"start">> := StartPos
  , <<"end">>   := EndPos} = Range,
  {pos(StartPos), pos(EndPos)}.


-spec pos(els_core:position()) -> position().
pos(Pos) ->
  #{<<"character">> := Col, <<"line">> := Line} = Pos,
  {Line+1, Col+1}.


-spec path(els_core:uri()) -> string().
path(Uri) ->
  binary_to_list(els_uri:path(Uri)).

%%==============================================================================
%% General utilities
%%==============================================================================

-spec root_folder() -> string().
root_folder() ->
  binary_to_list(els_uri:path(els_config:get(root_uri))).



%%==============================================================================
%% Wrangler refactoring form
%%==============================================================================

% preview_candidates([{{{StartLine, _StartCol}, {_EndLine, _EndCol}}, _IDK1, _IDK2} | _Tail] = Candidate, [Line | Lines], LineNum) when LineNum < StartLine ->
%   Next = preview_candidates(Candidate, Lines, LineNum + 1),
%   <<Line/binary, "\n", Next/binary>>;

% preview_candidates([{{{StartLine, _StartCol}, {_EndLine, _EndCol}}, _IDK1, _IDK2} | _Tail] = Candidate, [Line | Lines], LineNum) when LineNum == StartLine ->
%   Next = preview_candidates(Candidate, Lines, LineNum + 1),
%   <<"%vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n", Line/binary, "\n", Next/binary>>;

% preview_candidates([{{{_StartLine, _StartCol}, {EndLine, _EndCol}}, _IDK1, _IDK2} | _Tail] = Candidate, [Line | Lines], LineNum) when LineNum < EndLine + 1 ->
%   Next = preview_candidates(Candidate, Lines, LineNum + 1),
%   <<Line/binary, "\n", Next/binary>>;

% preview_candidates([{{{_StartLine, _StartCol}, {EndLine, _EndCol}}, _IDK1, _IDK2} | Tail], [Line | Lines], LineNum) when LineNum == EndLine + 1 ->
%   Next = preview_candidates(Tail, Lines, LineNum + 1),
%   <<"%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n", Line/binary, "\n", Next/binary>>;

% preview_candidates([{{{_StartLine, _StartCol}, {_EndLine, _EndCol}}, _IDK1, _IDK2} | _Tail] = Candidate, [Line | Lines], LineNum) ->
%   Next = preview_candidates(Candidate, Lines, LineNum + 1),
%   <<"?", Line/binary, "\n", Next/binary>>;

% preview_candidates([], [Line | Lines], LineNum) ->
%   Next = preview_candidates([], Lines, LineNum+1),
%   <<Line/binary, "\n", Next/binary>>;
% preview_candidates(_, [], _) ->
%   <<"">>.


%%==============================================================================
%% LSP workspace edit constructors
%%==============================================================================

-spec apply_edit(workspaceEdit()) -> ok.
apply_edit(Body) ->
  Method = <<"workspace/applyEdit">>,
  Params = #{
    edit => Body
  },
  els_server:send_request(Method, Params).

-spec create_file(path()) -> createFile().
create_file(Name) ->
  #{
    kind => <<"create">>,
    uri => els_uri:uri(list_to_binary(Name))
  }.

-spec rename_file(path(), path()) -> renameFile().
rename_file(OldName, NewName) ->
  #{
    kind => <<"rename">>,
    oldUri => els_uri:uri(list_to_binary(OldName)),
    newUri => els_uri:uri(list_to_binary(NewName))
  }.

-spec text_document_edit(path(), binary()) -> textDocumentEdit().
text_document_edit(Name, Text) ->
  #{
  textDocument =>
    #{
      uri => els_uri:uri(list_to_binary(Name)),
      version => null
    },
  edits =>
    [
      text_edit(Text)
    ]
  }.

-spec text_edit(binary()) ->  textEdit().
text_edit(Text) -> #{
  range =>
    #{ start => #{ line => 0, character => 0},
      'end' => #{ line => ?MAX_FILE_LENGTH, character => 0}
    },
  newText => els_utils:to_binary(Text)}.

%%==============================================================================
%% LSP notifications
%%==============================================================================

send_info(Message) ->
  els_server:send_notification(<<"window/showMessage">>,
    #{ type => ?LSP_MESSAGE_TYPE_INFO,
      message => els_utils:to_binary(Message++ " (Wrangler)")
    }).

send_warning(Message) ->
  els_server:send_notification(<<"window/showMessage">>,
    #{ type => ?LSP_MESSAGE_TYPE_WARNING,
      message => els_utils:to_binary(Message++ " (Wrangler)")
    }).

send_error(Message) -> 
  els_server:send_notification(<<"window/showMessage">>,
    #{ type => ?LSP_MESSAGE_TYPE_ERROR,
      message => els_utils:to_binary(Message ++ " (Wrangler)")
    }).