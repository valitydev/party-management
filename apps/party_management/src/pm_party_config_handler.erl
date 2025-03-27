-module(pm_party_config_handler).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Woody handler called by pm_woody_wrapper

-behaviour(pm_woody_wrapper).

-export([handle_function/3]).

%%

-spec handle_function(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        partycfg,
        fun() ->
            handle_function_(Func, Args, Opts)
        end
    ).

-spec handle_function_(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function_('ComputeTerms', Args, _Opts) ->
    {Ref, Revision, Varset} = Args,
    VS = pm_varset:decode_varset(Varset),
    Terms = get_terms(Ref, Revision),
    pm_party:reduce_terms(Terms, VS, Revision).

%%

get_terms(Ref, DomainRevision) ->
    pm_domain:get(DomainRevision, {term_set_hierarchy, Ref}).
