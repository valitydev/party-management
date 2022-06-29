-module(pm_claim_committer_handler).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_claimmgmt_thrift.hrl").

-behaviour(pm_woody_wrapper).

-export([handle_function/3]).

-spec handle_function(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        claimmgmt,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function_(Fun, {PartyID, _Claim} = Args, _Opts) when Fun == 'Accept'; Fun == 'Commit' ->
    call(PartyID, Fun, Args).

call(PartyID, FunctionName, Args) ->
    ok = scoper:add_meta(#{party_id => PartyID}),
    try
        pm_party_machine:call(PartyID, claim_committer, {'ClaimCommitter', FunctionName}, Args)
    catch
        throw:#payproc_PartyNotFound{} ->
            erlang:throw(#claimmgmt_PartyNotFound{})
    end.
