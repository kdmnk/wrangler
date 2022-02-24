
-module(wls_execute_command_provider).

-export([enabled_commands/0, execute_command/2]).
-include_lib("kernel/include/logger.hrl").


-spec enabled_commands() -> [els_command:command_id()].
enabled_commands() -> wls_code_actions:enabled_actions().

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(Command, Arguments) -> 
    case lists:member(Command, wls_code_actions:enabled_actions()) of
        true -> wls_code_actions:execute_command(Command, Arguments);
        false -> ?LOG_INFO("Unsupported command: ~p", [Command])
    end.