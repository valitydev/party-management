-module(pm_client_party).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-export([start/2]).
-export([stop/1]).

-export([compute_terms/4]).
-export([compute_payment_institution/4]).

-export([get_shop_account/3]).
-export([get_wallet_account/3]).
-export([get_account_state/3]).

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
-type domain_revision() :: dmsl_domain_thrift:'DataRevision'().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type wallet_id() :: dmsl_domain_thrift:'WalletID'().
-type shop_account_id() :: dmsl_domain_thrift:'AccountID'().

-type termset_hierarchy_ref() :: dmsl_domain_thrift:'TermSetHierarchyRef'().
-type payment_intitution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type varset() :: dmsl_payproc_thrift:'Varset'().

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

-spec compute_terms(termset_hierarchy_ref(), domain_revision(), varset(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().
compute_terms(Ref, DomainRevision, Varset, Client) ->
    call(Client, 'ComputeTerms', [Ref, DomainRevision, Varset]).

-spec compute_payment_institution(payment_intitution_ref(), domain_revision(), varset(), pid()) ->
    dmsl_domain_thrift:'PaymentInstitution'() | woody_error:business_error().
compute_payment_institution(Ref, DomainRevision, Varset, Client) ->
    call(Client, 'ComputePaymentInstitution', [Ref, DomainRevision, Varset]).

-spec get_account_state(shop_account_id(), domain_revision(), pid()) ->
    dmsl_payproc_thrift:'AccountState'() | woody_error:business_error().
get_account_state(AccountID, DomainRevision, Client) ->
    call(Client, 'GetAccountState', with_party_id([AccountID, DomainRevision])).

-spec get_shop_account(shop_id(), domain_revision(), pid()) ->
    dmsl_domain_thrift:'ShopAccount'() | woody_error:business_error().
get_shop_account(ShopID, DomainRevision, Client) ->
    call(Client, 'GetShopAccount', with_party_id([ShopID, DomainRevision])).

-spec get_wallet_account(wallet_id(), domain_revision(), pid()) ->
    dmsl_domain_thrift:'WalletAccount'() | woody_error:business_error().
get_wallet_account(ShopID, DomainRevision, Client) ->
    call(Client, 'GetWalletAccount', with_party_id([ShopID, DomainRevision])).

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
