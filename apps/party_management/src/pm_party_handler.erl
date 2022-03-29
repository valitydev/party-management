-module(pm_party_handler).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

%% Woody handler called by pm_woody_wrapper

-behaviour(pm_woody_wrapper).

-export([handle_function/3]).

%%

-spec handle_function(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args0, Opts) ->
    scoper:scope(
        partymgmt,
        fun() ->
            handle_function_(Func, remove_user_info_arg(Func, Args0), Opts)
        end
    ).

-spec handle_function_(woody:func(), woody:args(), pm_woody_wrapper:handler_opts()) -> term() | no_return().
%% Party
handle_function_('Create', {PartyID, PartyParams}, _Opts) ->
    pm_party_machine:start(PartyID, PartyParams);
handle_function_('Checkout', {PartyID, RevisionParam}, _Opts) ->
    checkout_party(PartyID, RevisionParam, #payproc_InvalidPartyRevision{});
handle_function_('Get', {PartyID}, _Opts) ->
    pm_party_machine:get_party(PartyID);
handle_function_('GetRevision', {PartyID}, _Opts) ->
    pm_party_machine:get_last_revision(PartyID);
handle_function_('GetStatus', {PartyID}, _Opts) ->
    pm_party_machine:get_status(PartyID);
handle_function_(Fun, Args, _Opts) when
    Fun =:= 'Block' orelse
        Fun =:= 'Unblock' orelse
        Fun =:= 'Suspend' orelse
        Fun =:= 'Activate'
->
    PartyID = erlang:element(1, Args),
    call(PartyID, Fun, Args);
%% Contract
handle_function_('GetContract', {PartyID, ContractID}, _Opts) ->
    Party = pm_party_machine:get_party(PartyID),
    ensure_contract(pm_party:get_contract(ContractID, Party));
handle_function_('ComputeContractTerms', Args, _Opts) ->
    {PartyID, ContractID, Timestamp, PartyRevisionParams, DomainRevision, Varset} = Args,
    Party = checkout_party(PartyID, PartyRevisionParams),
    Contract = ensure_contract(pm_party:get_contract(ContractID, Party)),
    VS =
        case pm_varset:decode_varset(Varset) of
            #{shop_id := ShopID} = VS0 ->
                Shop = ensure_shop(pm_party:get_shop(ShopID, Party)),
                Currency = maps:get(currency, VS0, (Shop#domain_Shop.account)#domain_ShopAccount.currency),
                VS0#{
                    category => Shop#domain_Shop.category,
                    currency => Currency
                };
            #{} = VS0 ->
                VS0
        end,
    DecodedVS = VS#{
        party_id => PartyID,
        identification_level => get_identification_level(Contract, Party)
    },
    Terms = pm_party:get_terms(Contract, Timestamp, DomainRevision),
    pm_party:reduce_terms(Terms, DecodedVS, DomainRevision);
%% Shop
handle_function_('GetShop', {PartyID, ID}, _Opts) ->
    Party = pm_party_machine:get_party(PartyID),
    ensure_shop(pm_party:get_shop(ID, Party));
handle_function_('GetShopContract', {PartyID, ID}, _Opts) ->
    Party = pm_party_machine:get_party(PartyID),
    Shop = ensure_shop(pm_party:get_shop(ID, Party)),
    Contract = pm_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Contractor = pm_party:get_contractor(Contract#domain_Contract.contractor_id, Party),
    #payproc_ShopContract{shop = Shop, contract = Contract, contractor = Contractor};
handle_function_('ComputeShopTerms', {PartyID, ShopID, Timestamp, PartyRevision, Varset}, _Opts) ->
    Party = checkout_party(PartyID, PartyRevision),
    Shop = ensure_shop(pm_party:get_shop(ShopID, Party)),
    Contract = pm_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Revision = pm_domain:head(),
    VS0 = pm_varset:decode_varset(Varset),
    DecodedVS = VS0#{
        party_id => PartyID,
        shop_id => ShopID,
        category => Shop#domain_Shop.category,
        currency => (Shop#domain_Shop.account)#domain_ShopAccount.currency,
        identification_level => get_identification_level(Contract, Party)
    },
    pm_party:reduce_terms(pm_party:get_terms(Contract, Timestamp, Revision), DecodedVS, Revision);
handle_function_(Fun, Args, _Opts) when
    Fun =:= 'BlockShop' orelse
        Fun =:= 'UnblockShop' orelse
        Fun =:= 'SuspendShop' orelse
        Fun =:= 'ActivateShop'
->
    PartyID = erlang:element(1, Args),
    call(PartyID, Fun, Args);
%% Claim
handle_function_('GetClaim', {PartyID, ID}, _Opts) ->
    pm_party_machine:get_claim(ID, PartyID);
handle_function_('GetClaims', {PartyID}, _Opts) ->
    pm_party_machine:get_claims(PartyID);
handle_function_(Fun, Args, _Opts) when
    Fun =:= 'CreateClaim' orelse
        Fun =:= 'AcceptClaim' orelse
        Fun =:= 'UpdateClaim' orelse
        Fun =:= 'DenyClaim' orelse
        Fun =:= 'RevokeClaim'
->
    PartyID = erlang:element(1, Args),
    call(PartyID, Fun, Args);
%% Event
handle_function_('GetEvents', {PartyID, Range}, _Opts) ->
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    pm_party_machine:get_public_history(PartyID, AfterID, Limit);
%% ShopAccount
handle_function_('GetAccountState', {PartyID, AccountID}, _Opts) ->
    Party = pm_party_machine:get_party(PartyID),
    pm_party:get_account_state(AccountID, Party);
handle_function_('GetShopAccount', {PartyID, ShopID}, _Opts) ->
    Party = pm_party_machine:get_party(PartyID),
    pm_party:get_shop_account(ShopID, Party);
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
%% Deprecated, will be replaced by 'ComputeRoutingRuleset'
handle_function_('ComputePaymentRoutingRuleset', Args, _Opts) ->
    {RuleSetRef, DomainRevision, Varset} = Args,
    RuleSet = get_payment_routing_ruleset(RuleSetRef, DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    pm_ruleset:reduce_payment_routing_ruleset(RuleSet, VS, DomainRevision);
handle_function_('ComputeRoutingRuleset', Args, _Opts) ->
    {RuleSetRef, DomainRevision, Varset} = Args,
    RuleSet = get_payment_routing_ruleset(RuleSetRef, DomainRevision),
    VS = pm_varset:decode_varset(Varset),
    pm_ruleset:reduce_payment_routing_ruleset(RuleSet, VS, DomainRevision);
%% PartyMeta

handle_function_('GetMeta', {PartyID}, _Opts) ->
    pm_party_machine:get_meta(PartyID);
handle_function_('GetMetaData', {PartyID, NS}, _Opts) ->
    pm_party_machine:get_metadata(NS, PartyID);
handle_function_(Fun, Args, _Opts) when
    Fun =:= 'SetMetaData' orelse
        Fun =:= 'RemoveMetaData'
->
    PartyID = erlang:element(1, Args),
    call(PartyID, Fun, Args);
%% Payment Institutions

handle_function_(
    'ComputePaymentInstitutionTerms',
    {PaymentInstitutionRef, Varset},
    _Opts
) ->
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
%% Payouts adhocs

handle_function_(
    'ComputePayoutCashFlow',
    {PartyID, #payproc_PayoutParams{id = ShopID, amount = Amount, timestamp = Timestamp} = PayoutParams},
    _Opts
) ->
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Shop = ensure_shop(pm_party:get_shop(ShopID, Party)),
    Contract = pm_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Currency = Amount#domain_Cash.currency,
    ok = pm_currency:validate_currency(Currency, Shop),
    PayoutTool = get_payout_tool(Shop, Contract, PayoutParams),
    VS = #{
        party_id => PartyID,
        shop_id => ShopID,
        category => Shop#domain_Shop.category,
        currency => Currency,
        cost => Amount,
        payout_method => pm_payout_tool:get_method(PayoutTool)
    },
    Revision = pm_domain:head(),
    case pm_party:get_terms(Contract, Timestamp, Revision) of
        #domain_TermSet{payouts = PayoutsTerms} when PayoutsTerms /= undefined ->
            compute_payout_cash_flow(Amount, PayoutsTerms, Shop, Contract, VS, Revision);
        #domain_TermSet{payouts = undefined} ->
            throw(#payproc_OperationNotPermitted{})
    end.

%%

%% @TODO Delete after protocol migration
%% This is a migration measure to make sure we can accept both old and new (with no userinfo) protocol here
remove_user_info_arg('ComputeProviderTerminal', Args0) ->
    Args0;
remove_user_info_arg(_Func, Args0) ->
    erlang:delete_element(1, Args0).

add_user_info_arg('ComputeProviderTerminal', Args0) ->
    Args0;
add_user_info_arg(_Func, Args0) ->
    erlang:insert_element(1, Args0, undefined).

%%

call(PartyID, FunctionName, Args) ->
    pm_party_machine:call(
        PartyID,
        party_management,
        {'PartyManagement', FunctionName},
        add_user_info_arg(FunctionName, Args)
    ).

%%

get_payout_tool(_Shop, Contract, #payproc_PayoutParams{payout_tool_id = ToolID}) when ToolID =/= undefined ->
    case pm_contract:get_payout_tool(ToolID, Contract) of
        undefined ->
            throw(#payproc_PayoutToolNotFound{});
        PayoutTool ->
            PayoutTool
    end;
get_payout_tool(Shop, Contract, _PayoutParams) ->
    pm_contract:get_payout_tool(Shop#domain_Shop.payout_tool_id, Contract).

assert_provider_reduced(#domain_Provider{terms = Terms}) ->
    assert_provider_terms_reduced(Terms).

assert_provider_terms_reduced(#domain_ProvisionTermSet{}) ->
    ok;
assert_provider_terms_reduced(undefined) ->
    throw(#payproc_ProvisionTermSetUndefined{}).

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

compute_payout_cash_flow(
    Amount,
    #domain_PayoutsServiceTerms{fees = CashFlowSelector},
    Shop,
    Contract,
    VS,
    Revision
) ->
    Cashflow = pm_selector:reduce_to_value(CashFlowSelector, VS, Revision),
    CashFlowContext = #{operation_amount => Amount},
    Currency = Amount#domain_Cash.currency,
    AccountMap = collect_payout_account_map(Currency, Shop, Contract, VS, Revision),
    pm_cashflow:finalize(Cashflow, CashFlowContext, AccountMap).

collect_payout_account_map(
    Currency,
    #domain_Shop{account = ShopAccount},
    #domain_Contract{payment_institution = PaymentInstitutionRef},
    VS,
    Revision
) ->
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, Revision),
    SystemAccount = pm_payment_institution:get_system_account(Currency, VS, Revision, PaymentInstitution),
    #{
        {merchant, settlement} => ShopAccount#domain_ShopAccount.settlement,
        {merchant, guarantee} => ShopAccount#domain_ShopAccount.guarantee,
        {merchant, payout} => ShopAccount#domain_ShopAccount.payout,
        {system, settlement} => SystemAccount#domain_SystemAccount.settlement,
        {system, subagent} => SystemAccount#domain_SystemAccount.subagent
    }.

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
