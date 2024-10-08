-module(pm_client_party).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-export([start/2]).
-export([stop/1]).

-export([create/2]).
-export([get/1]).
-export([get_revision/1]).
-export([checkout/2]).
-export([block/2]).
-export([unblock/2]).
-export([suspend/1]).
-export([activate/1]).
-export([get_status/1]).

-export([get_meta/1]).
-export([get_metadata/2]).
-export([set_metadata/3]).
-export([remove_metadata/2]).

-export([get_contract/2]).
-export([compute_contract_terms/6]).
-export([get_shop/2]).
-export([get_shop_contract/2]).
-export([compute_shop_terms/5]).
-export([compute_payment_institution_terms/3]).
-export([compute_payment_institution/4]).

-export([block_shop/3]).
-export([unblock_shop/3]).
-export([suspend_shop/2]).
-export([activate_shop/2]).

-export([get_claim/2]).
-export([get_claims/1]).
-export([create_claim/2]).
-export([update_claim/4]).
-export([accept_claim/3]).
-export([deny_claim/4]).
-export([revoke_claim/4]).

-export([get_account_state/2]).
-export([get_shop_account/2]).
-export([pull_event/1]).
-export([pull_event/2]).
-export([get_events/3]).

-export([compute_provider/4]).
-export([compute_provider_terminal/4]).
-export([compute_provider_terminal_terms/5]).
-export([compute_globals/3]).
-export([compute_routing_ruleset/4]).

%% GenServer

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

%%

-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type party_params() :: dmsl_payproc_thrift:'PartyParams'().
-type domain_revision() :: dmsl_domain_thrift:'DataRevision'().
-type contract_id() :: dmsl_domain_thrift:'ContractID'().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type claim_id() :: dmsl_payproc_thrift:'ClaimID'().
-type claim() :: dmsl_payproc_thrift:'Claim'().
-type claim_revision() :: dmsl_payproc_thrift:'ClaimRevision'().
-type changeset() :: dmsl_payproc_thrift:'PartyChangeset'().
-type shop_account_id() :: dmsl_domain_thrift:'AccountID'().
-type meta() :: dmsl_domain_thrift:'PartyMeta'().
-type meta_ns() :: dmsl_domain_thrift:'PartyMetaNamespace'().
-type meta_data() :: dmsl_domain_thrift:'PartyMetaData'().
-type timestamp() :: dmsl_base_thrift:'Timestamp'().

-type party_revision_param() :: dmsl_payproc_thrift:'PartyRevisionParam'().
-type payment_intitution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type varset() :: dmsl_payproc_thrift:'Varset'().
-type contract_terms_varset() :: dmsl_payproc_thrift:'ComputeContractTermsVarset'().
-type shop_terms_varset() :: dmsl_payproc_thrift:'ComputeShopTermsVarset'().

-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type routing_ruleset_ref() :: dmsl_domain_thrift:'RoutingRulesetRef'().

-spec start(party_id(), pm_client_api:t()) -> pid().
start(PartyID, ApiClient) ->
    {ok, Pid} = gen_server:start(?MODULE, {PartyID, ApiClient}, []),
    Pid.

-spec stop(pid()) -> ok.
stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create(party_params(), pid()) -> ok | woody_error:business_error().
create(PartyParams, Client) ->
    call(Client, 'Create', with_party_id([PartyParams])).

-spec get(pid()) -> dmsl_domain_thrift:'Party'() | woody_error:business_error().
get(Client) ->
    call(Client, 'Get', with_party_id([])).

-spec get_revision(pid()) -> dmsl_domain_thrift:'Party'() | woody_error:business_error().
get_revision(Client) ->
    call(Client, 'GetRevision', with_party_id([])).

-spec get_status(pid()) -> dmsl_domain_thrift:'PartyStatus'() | woody_error:business_error().
get_status(Client) ->
    call(Client, 'GetStatus', with_party_id([])).

-spec checkout(party_revision_param(), pid()) -> dmsl_domain_thrift:'Party'() | woody_error:business_error().
checkout(PartyRevisionParam, Client) ->
    call(Client, 'Checkout', with_party_id([PartyRevisionParam])).

-spec block(binary(), pid()) -> ok | woody_error:business_error().
block(Reason, Client) ->
    call(Client, 'Block', with_party_id([Reason])).

-spec unblock(binary(), pid()) -> ok | woody_error:business_error().
unblock(Reason, Client) ->
    call(Client, 'Unblock', with_party_id([Reason])).

-spec suspend(pid()) -> ok | woody_error:business_error().
suspend(Client) ->
    call(Client, 'Suspend', with_party_id([])).

-spec activate(pid()) -> ok | woody_error:business_error().
activate(Client) ->
    call(Client, 'Activate', with_party_id([])).

-spec get_meta(pid()) -> meta() | woody_error:business_error().
get_meta(Client) ->
    call(Client, 'GetMeta', with_party_id([])).

-spec get_metadata(meta_ns(), pid()) -> meta_data() | woody_error:business_error().
get_metadata(NS, Client) ->
    call(Client, 'GetMetaData', with_party_id([NS])).

-spec set_metadata(meta_ns(), meta_data(), pid()) -> ok | woody_error:business_error().
set_metadata(NS, Data, Client) ->
    call(Client, 'SetMetaData', with_party_id([NS, Data])).

-spec remove_metadata(meta_ns(), pid()) -> ok | woody_error:business_error().
remove_metadata(NS, Client) ->
    call(Client, 'RemoveMetaData', with_party_id([NS])).

-spec get_contract(contract_id(), pid()) -> dmsl_domain_thrift:'Contract'() | woody_error:business_error().
get_contract(ID, Client) ->
    call(Client, 'GetContract', with_party_id([ID])).

-spec compute_contract_terms(
    contract_id(),
    timestamp(),
    party_revision_param(),
    domain_revision(),
    contract_terms_varset(),
    pid()
) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().
compute_contract_terms(ID, Timestamp, PartyRevision, DomainRevision, Varset, Client) ->
    Args = with_party_id([ID, Timestamp, PartyRevision, DomainRevision, Varset]),
    call(Client, 'ComputeContractTerms', Args).

-spec compute_payment_institution_terms(payment_intitution_ref(), varset(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().
compute_payment_institution_terms(Ref, Varset, Client) ->
    call(Client, 'ComputePaymentInstitutionTerms', [Ref, Varset]).

-spec compute_payment_institution(payment_intitution_ref(), domain_revision(), varset(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().
compute_payment_institution(Ref, DomainRevision, Varset, Client) ->
    call(Client, 'ComputePaymentInstitution', [Ref, DomainRevision, Varset]).

-spec get_shop(shop_id(), pid()) -> dmsl_domain_thrift:'Shop'() | woody_error:business_error().
get_shop(ID, Client) ->
    call(Client, 'GetShop', with_party_id([ID])).

-spec get_shop_contract(shop_id(), pid()) ->
    dmsl_payproc_thrift:'ShopContract'() | woody_error:business_error().
get_shop_contract(ID, Client) ->
    call(Client, 'GetShopContract', with_party_id([ID])).

-spec block_shop(shop_id(), binary(), pid()) -> ok | woody_error:business_error().
block_shop(ID, Reason, Client) ->
    call(Client, 'BlockShop', with_party_id([ID, Reason])).

-spec unblock_shop(shop_id(), binary(), pid()) -> ok | woody_error:business_error().
unblock_shop(ID, Reason, Client) ->
    call(Client, 'UnblockShop', with_party_id([ID, Reason])).

-spec suspend_shop(shop_id(), pid()) -> ok | woody_error:business_error().
suspend_shop(ID, Client) ->
    call(Client, 'SuspendShop', with_party_id([ID])).

-spec activate_shop(shop_id(), pid()) -> ok | woody_error:business_error().
activate_shop(ID, Client) ->
    call(Client, 'ActivateShop', with_party_id([ID])).

-spec compute_shop_terms(shop_id(), timestamp(), party_revision_param(), shop_terms_varset(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().
compute_shop_terms(ID, Timestamp, PartyRevision, VS, Client) ->
    call(Client, 'ComputeShopTerms', with_party_id([ID, Timestamp, PartyRevision, VS])).

-spec get_claim(claim_id(), pid()) -> claim() | woody_error:business_error().
get_claim(ID, Client) ->
    call(Client, 'GetClaim', with_party_id([ID])).

-spec get_claims(pid()) -> [claim()] | woody_error:business_error().
get_claims(Client) ->
    call(Client, 'GetClaims', with_party_id([])).

-spec create_claim(changeset(), pid()) -> claim() | woody_error:business_error().
create_claim(Changeset, Client) ->
    call(Client, 'CreateClaim', with_party_id([Changeset])).

-spec update_claim(claim_id(), claim_revision(), changeset(), pid()) -> ok | woody_error:business_error().
update_claim(ID, Revision, Changeset, Client) ->
    call(Client, 'UpdateClaim', with_party_id([ID, Revision, Changeset])).

-spec accept_claim(claim_id(), claim_revision(), pid()) -> ok | woody_error:business_error().
accept_claim(ID, Revision, Client) ->
    call(Client, 'AcceptClaim', with_party_id([ID, Revision])).

-spec deny_claim(claim_id(), claim_revision(), binary() | undefined, pid()) -> ok | woody_error:business_error().
deny_claim(ID, Revision, Reason, Client) ->
    call(Client, 'DenyClaim', with_party_id([ID, Revision, Reason])).

-spec revoke_claim(claim_id(), claim_revision(), binary() | undefined, pid()) -> ok | woody_error:business_error().
revoke_claim(ID, Revision, Reason, Client) ->
    call(Client, 'RevokeClaim', with_party_id([ID, Revision, Reason])).

-spec get_account_state(shop_account_id(), pid()) ->
    dmsl_payproc_thrift:'AccountState'() | woody_error:business_error().
get_account_state(AccountID, Client) ->
    call(Client, 'GetAccountState', with_party_id([AccountID])).

-spec get_shop_account(shop_id(), pid()) -> dmsl_domain_thrift:'ShopAccount'() | woody_error:business_error().
get_shop_account(ShopID, Client) ->
    call(Client, 'GetShopAccount', with_party_id([ShopID])).

-spec compute_provider(provider_ref(), domain_revision(), varset(), pid()) ->
    dmsl_domain_thrift:'Provider'() | woody_error:business_error().
compute_provider(ProviderRef, Revision, Varset, Client) ->
    call(Client, 'ComputeProvider', [ProviderRef, Revision, Varset]).

-spec compute_provider_terminal(
    terminal_ref(),
    domain_revision(),
    varset() | undefined,
    pid()
) -> dmsl_payproc_thrift:'ProviderTerminal'() | woody_error:business_error().
compute_provider_terminal(TerminalRef, Revision, Varset, Client) ->
    call(Client, 'ComputeProviderTerminal', [TerminalRef, Revision, Varset]).

-spec compute_provider_terminal_terms(
    provider_ref(),
    terminal_ref(),
    domain_revision(),
    varset(),
    pid()
) -> dmsl_domain_thrift:'ProvisionTermSet'() | woody_error:business_error().
compute_provider_terminal_terms(ProviderRef, TerminalRef, Revision, Varset, Client) ->
    Args = [ProviderRef, TerminalRef, Revision, Varset],
    call(Client, 'ComputeProviderTerminalTerms', Args).

-spec compute_globals(domain_revision(), varset(), pid()) ->
    dmsl_domain_thrift:'Globals'() | woody_error:business_error().
compute_globals(Revision, Varset, Client) ->
    call(Client, 'ComputeGlobals', [Revision, Varset]).

-spec compute_routing_ruleset(routing_ruleset_ref(), domain_revision(), varset(), pid()) ->
    dmsl_domain_thrift:'RoutingRuleset'() | woody_error:business_error().
compute_routing_ruleset(RoutingRuleSetRef, Revision, Varset, Client) ->
    call(Client, 'ComputeRoutingRuleset', [RoutingRuleSetRef, Revision, Varset]).

-define(DEFAULT_NEXT_EVENT_TIMEOUT, 5000).

-spec pull_event(pid()) -> tuple() | timeout | woody_error:business_error().
pull_event(Client) ->
    pull_event(?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_event(timeout(), pid()) -> tuple() | timeout | woody_error:business_error().
pull_event(Timeout, Client) ->
    gen_server:call(Client, {pull_event, Timeout}, infinity).

-spec get_events(non_neg_integer() | undefined, pos_integer() | undefined, pid()) ->
    [tuple()] | woody_error:business_error().
get_events(After, Limit, Client) ->
    call(Client, 'GetEvents', with_party_id([]) ++ [#payproc_EventRange{'after' = After, limit = Limit}]).

call(Client, Function, Args) ->
    map_result_error(gen_server:call(Client, {call, Function, Args})).

map_result_error({ok, Result}) ->
    Result;
map_result_error({exception, _} = Exception) ->
    Exception;
map_result_error({error, Error}) ->
    error(Error).

%%

-type event() :: dmsl_payproc_thrift:'Event'().

-record(state, {
    party_id :: party_id(),
    poller :: pm_client_event_poller:st(event()),
    client :: pm_client_api:t()
}).

-type state() :: #state{}.
-type callref() :: {pid(), Tag :: reference()}.

-spec init({party_id(), pm_client_api:t()}) -> {ok, state()}.
init({PartyID, ApiClient}) ->
    {ok, #state{
        party_id = PartyID,
        client = ApiClient,
        poller = pm_client_event_poller:new(
            {party_management, 'GetEvents', [undefined, PartyID]},
            fun(Event) -> Event#payproc_Event.id end
        )
    }}.

-spec handle_call(term(), callref(), state()) -> {reply, term(), state()} | {noreply, state()}.
handle_call({call, Function, ArgsIn}, _From, St = #state{client = Client}) ->
    Args = lists:map(
        fun
            (Fun) when is_function(Fun, 1) -> Fun(St);
            (Arg) -> Arg
        end,
        ArgsIn
    ),
    Result = pm_client_api:call(party_management, Function, Args, Client),
    {reply, Result, St};
handle_call({pull_event, Timeout}, _From, St = #state{poller = Poller, client = Client}) ->
    {Result, PollerNext} = pm_client_event_poller:poll(1, Timeout, Client, Poller),
    StNext = St#state{poller = PollerNext},
    case Result of
        [] ->
            {reply, timeout, StNext};
        [#payproc_Event{payload = Payload}] ->
            {reply, Payload, StNext};
        Error ->
            {reply, Error, StNext}
    end;
handle_call(Call, _From, State) ->
    _ = logger:warning("unexpected call received: ~tp", [Call]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(Cast, State) ->
    _ = logger:warning("unexpected cast received: ~tp", [Cast]),
    {noreply, State}.

with_party_id(Args) ->
    [fun(St) -> St#state.party_id end | Args].
