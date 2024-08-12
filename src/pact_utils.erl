-module(pact_utils).

-export([
    run_executable_async/1
]).

-spec run_executable_async(string()) -> {integer(), string()}.
run_executable_async(Cmd) ->
    EscapedCmd = escape_special_chars(Cmd),
    Port = erlang:open_port({spawn, EscapedCmd}, [stream, in, eof, hide, exit_status]),
    get_data_from_executable(Port, []).

escape_special_chars(Cmd) ->
    %% Split the command into parts and escape them
    Parts = string:tokens(Cmd, " "),
    EscapedParts = lists:map(fun escape_part/1, Parts),
    string:join(EscapedParts, " ").

escape_part(Part) ->
    %% Check if the part contains special characters and escape it
    case re:run(Part, "^[\\w\\d\\-]+$") of
        {match, _} -> Part; %% If it matches a simple word, return as is
        nomatch -> "'" ++ re:replace(Part, "'", "'\\''", [global, {return, list}]) ++ "'"
    end.
get_data_from_executable(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data_from_executable(Port, [Sofar | Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} -> true
            end,
            receive
                {'EXIT', Port, _} -> ok
            after
                % force context switch
                1 -> ok
            end,
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, lists:flatten(Sofar)}
    end.
