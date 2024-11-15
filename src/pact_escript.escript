#!/usr/bin/env escript


main([Module, Function | Args]) ->
    {ok, Dir} = file:get_cwd(),
    MixEnv = os:getenv("MIX_ENV", "test"),
    code:add_pathz(Dir ++ "/_build/" ++ MixEnv ++ "/lib/pact_erlang/ebin"),
    ModuleAtom = list_to_atom(Module),
    FunctionAtom = list_to_atom(Function),
    ArgsList =
    case FunctionAtom of
        verify_file_pacts ->
            {AList, _} =
            lists:foldl(
                fun(Arg, {Acc, CountAcc}) ->
                    A =
                    case CountAcc of
                        3 ->
                            list_to_integer(Arg);
                        _ ->
                            list_to_binary(Arg)
                    end,
                    {Acc ++ [A], CountAcc + 1}
                end,
                {[], 0},
                Args
            ),
            case length(AList) < 10 of
                true ->
                    AList ++ [<<"">>];
                false ->
                    AList
            end;
        verify_url_pacts ->
            {AList, _} =
            lists:foldl(
                fun(Arg, {Acc, CountAcc}) ->
                    A =
                    case CountAcc of
                        3 ->
                            list_to_integer(Arg);
                        _ ->
                            list_to_binary(Arg)
                    end,
                    {Acc ++ [A], CountAcc + 1}
                end,
                {[], 0},
                Args
            ),
            case length(AList) < 13 of
                true ->
                    AList ++ [<<"">>];
                false ->
                    AList
            end;
        verify_broker_pacts ->
            {AList1, _} =
            lists:foldl(
                fun(Arg, {Acc, CountAcc}) ->
                    A =
                    case CountAcc of
                        Num when Num == 3 orelse Num == 10 orelse Num == 14 ->
                            list_to_integer(Arg);
                        _ ->
                            list_to_binary(Arg)
                    end,
                    {Acc ++ [A], CountAcc + 1}
                end,
                {[], 0},
                Args
            ),
            case length(AList1) < 16 of
                true ->
                    AList1 ++ [<<"">>];
                false ->
                    AList1
            end
    end,
    pact:enable_logging(info),
    Result = erlang:apply(ModuleAtom, FunctionAtom, ArgsList),
    case Result of
        ReturnValue when is_integer(ReturnValue) ->
            halt(ReturnValue);
        {badrpc, Reason} ->
            io:format("Error executing function: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    io:format("Usage: ./script.erl Module Function Arg1 Arg2 ...~n"),
    halt(1).
