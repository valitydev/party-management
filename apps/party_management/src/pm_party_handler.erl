-module(pm_party_handler).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Woody handler called by pm_woody_wrapper

-behaviour(pm_woody_wrapper).

-export([handle_function/3]).

%%

-spec handle_function(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(partymgmt, fun() ->
        handle_function_(Func, Args, Opts)
    end).

-spec handle_function_(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
%% Accounts
handle_function_('GetShopAccount', {PartyID, ShopID}, _Opts) ->
    _ = set_party_mgmt_meta(PartyID),
    Party = pm_party_machine:get_party(PartyID),
    pm_party:get_shop_account(ShopID, Party);
handle_function_('GetWalletAccount', {PartyID, WalletID}, _Opts) ->
    _ = set_party_mgmt_meta(PartyID),
    Party = pm_party_machine:get_party(PartyID),
    %% TODO Implement
    pm_party:get_wallet_account(WalletID, Party);
handle_function_('GetAccountState', {PartyID, AccountID}, _Opts) ->
    _ = set_party_mgmt_meta(PartyID),
    Party = pm_party_machine:get_party(PartyID),
    pm_party:get_account_state(AccountID, Party);
%% Providers
handle_function_('ComputeProvider', Args, _Opts) ->
    {ProviderRef, DomainRevision, Varset} = Args,
    Provider = get_provider(ProviderRef, DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    ComputedProvider = pm_provider:reduce_provider(Provider, VS, DomainRevision),
    _ = assert_provider_reduced(ComputedProvider),
    ComputedProvider;
handle_function_('ComputeProviderTerminalTerms', Args, _Opts) ->
    {ProviderRef, TerminalRef, DomainRevision, Varset} = Args,
    Provider = get_provider(ProviderRef, DomainRevision),
    Terminal = get_terminal(TerminalRef, DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    Terms = pm_provider:reduce_provider_terminal_terms(Provider, Terminal, VS, DomainRevision),
    _ = assert_provider_terms_reduced(Terms),
    Terms;
handle_function_('ComputeProviderTerminal', {TerminalRef, DomainRevision, VarsetIn}, _Opts) ->
    Terminal = get_terminal(TerminalRef, DomainRevision),
    ProviderRef = Terminal#domain_Terminal.provider_ref,
    Provider = get_provider(ProviderRef, DomainRevision),
    Proxy = pm_provider:compute_proxy(Provider, Terminal, DomainRevision),
    ComputedTerms = pm_maybe:apply(
        fun(Varset) ->
            VS = pm_varset:decode_varset(Varset),
            pm_provider:reduce_provider_terminal_terms(Provider, Terminal, VS, DomainRevision)
        end,
        VarsetIn
    ),
    #payproc_ProviderTerminal{
        ref = TerminalRef,
        name = Terminal#domain_Terminal.name,
        description = Terminal#domain_Terminal.description,
        proxy = Proxy,
        provider = #payproc_ProviderDetails{
            ref = ProviderRef,
            name = Provider#domain_Provider.name,
            description = Provider#domain_Provider.description
        },
        terms = ComputedTerms
    };
%% Globals
handle_function_('ComputeGlobals', Args, _Opts) ->
    {DomainRevision, Varset} = Args,
    Globals = get_globals(DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    pm_globals:reduce_globals(Globals, VS, DomainRevision);
%% RuleSets
handle_function_('ComputeRoutingRuleset', Args, _Opts) ->
    {RuleSetRef, DomainRevision, Varset} = Args,
    RuleSet = get_payment_routing_ruleset(RuleSetRef, DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    pm_ruleset:reduce_payment_routing_ruleset(RuleSet, VS, DomainRevision);
%% Payment Institutions
handle_function_('ComputePaymentInstitutionTerms', {PaymentInstitutionRef, Varset}, _Opts) ->
    Revision = pm_domain:head(),
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, Revision),
    VS = pm_varset:decode_varset(Varset),
    ContractTemplate = get_default_contract_template(PaymentInstitution, VS, Revision),
    Terms = pm_party:get_terms(ContractTemplate, pm_datetime:format_now(), Revision),
    pm_party:reduce_terms(Terms, VS, Revision);
handle_function_('ComputePaymentInstitution', Args, _Opts) ->
    {PaymentInstitutionRef, DomainRevision, Varset} = Args,
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    pm_payment_institution:reduce_payment_institution(PaymentInstitution, VS, DomainRevision);
handle_function_('ComputeTerms', Args, _Opts) ->
    {Ref, Revision, Varset} = Args,
    VS = pm_varset:decode_varset(Varset),
    Terms = pm_party:get_term_set(Ref, pm_datetime:format_now(), Revision),
    pm_party:reduce_terms(Terms, VS, Revision).

%%

call(PartyID, FunctionName, Args) ->
    pm_party_machine:call(
        PartyID,
        party_management,
        {'PartyManagement', FunctionName},
        Args
    ).

%%

assert_provider_reduced(#domain_Provider{terms = Terms}) ->
    assert_provider_terms_reduced(Terms).

assert_provider_terms_reduced(#domain_ProvisionTermSet{}) ->
    ok;
assert_provider_terms_reduced(undefined) ->
    throw(#payproc_ProvisionTermSetUndefined{}).

set_party_mgmt_meta(PartyID) ->
    scoper:add_meta(#{party_id => PartyID}).

checkout_party(PartyID, RevisionParam) ->
    checkout_party(PartyID, RevisionParam, #payproc_PartyNotExistsYet{}).

checkout_party(PartyID, RevisionParam, Exception) ->
    try
        pm_party_machine:checkout(PartyID, RevisionParam)
    catch
        error:revision_not_found ->
            throw(Exception)
    end.

ensure_contract(#domain_Contract{} = Contract) ->
    Contract;
ensure_contract(undefined) ->
    throw(#payproc_ContractNotFound{}).

ensure_shop(#domain_Shop{} = Shop) ->
    Shop;
ensure_shop(undefined) ->
    throw(#payproc_ShopNotFound{}).

get_payment_institution(PaymentInstitutionRef, Revision) ->
    case pm_domain:find(Revision, {payment_institution, PaymentInstitutionRef}) of
        #domain_PaymentInstitution{} = P ->
            P;
        notfound ->
            throw(#payproc_PaymentInstitutionNotFound{})
    end.

get_provider(ProviderRef, DomainRevision) ->
    try
        pm_domain:get(DomainRevision, {provider, ProviderRef})
    catch
        error:{object_not_found, {DomainRevision, {provider, ProviderRef}}} ->
            throw(#payproc_ProviderNotFound{})
    end.

get_terminal(TerminalRef, DomainRevision) ->
    try
        pm_domain:get(DomainRevision, {terminal, TerminalRef})
    catch
        error:{object_not_found, {DomainRevision, {terminal, TerminalRef}}} ->
            throw(#payproc_TerminalNotFound{})
    end.

get_globals(DomainRevision) ->
    Globals = {globals, #domain_GlobalsRef{}},
    try
        pm_domain:get(DomainRevision, Globals)
    catch
        error:{object_not_found, {DomainRevision, Globals}} ->
            throw(#payproc_GlobalsNotFound{})
    end.

get_payment_routing_ruleset(RuleSetRef, DomainRevision) ->
    try
        pm_domain:get(DomainRevision, {routing_rules, RuleSetRef})
    catch
        error:{object_not_found, {DomainRevision, {routing_rules, RuleSetRef}}} ->
            throw(#payproc_RuleSetNotFound{})
    end.

get_default_contract_template(#domain_PaymentInstitution{default_contract_template = ContractSelector}, VS, Revision) ->
    ContractTemplateRef = pm_selector:reduce_to_value(ContractSelector, VS, Revision),
    pm_domain:get(Revision, {contract_template, ContractTemplateRef}).

get_identification_level(#domain_Contract{contractor_id = undefined, contractor = Contractor}, _) ->
    %% TODO legacy, remove after migration
    case Contractor of
        {legal_entity, _} ->
            full;
        _ ->
            none
    end;
get_identification_level(#domain_Contract{contractor_id = ContractorID}, Party) ->
    Contractor = pm_party:get_contractor(ContractorID, Party),
    Contractor#domain_PartyContractor.status.
