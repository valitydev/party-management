-module(pm_proto).

-export([get_service/1]).

-export([get_service_spec/1]).
-export([get_service_spec/2]).

-export_type([service/0]).
-export_type([service_spec/0]).

%%

-define(VERSION_PREFIX, "/v1").

-type service() :: woody:service().
-type service_spec() :: {Path :: string(), service()}.

-spec get_service(Name :: atom()) -> service().
get_service(claim_committer) ->
    {dmsl_claimmgmt_thrift, 'ClaimCommitter'};
get_service(party_management) ->
    {dmsl_payproc_thrift, 'PartyManagement'};
get_service(party_config) ->
    {dmsl_payproc_thrift, 'PartyConfigManagement'};
get_service(accounter) ->
    {dmsl_accounter_thrift, 'Accounter'}.

-spec get_service_spec(Name :: atom()) -> service_spec().
get_service_spec(Name) ->
    get_service_spec(Name, #{}).

-spec get_service_spec(Name :: atom(), Opts :: #{namespace => binary()}) -> service_spec().
get_service_spec(Name = claim_committer, #{}) ->
    {?VERSION_PREFIX ++ "/processing/claim_committer", get_service(Name)};
get_service_spec(Name = party_management, #{}) ->
    {?VERSION_PREFIX ++ "/processing/partymgmt", get_service(Name)};
get_service_spec(Name = party_config, #{}) ->
    {?VERSION_PREFIX ++ "/processing/partycfg", get_service(Name)};
get_service_spec(Name = processor, #{namespace := Ns}) when is_binary(Ns) ->
    {?VERSION_PREFIX ++ "/stateproc/" ++ binary_to_list(Ns), get_service(Name)}.
