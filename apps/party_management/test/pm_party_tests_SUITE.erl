-module(pm_party_tests_SUITE).

-include_lib("party_management/test/pm_ct_domain.hrl").
-include_lib("party_management/include/party_events.hrl").
-include_lib("party_management/include/domain.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([party_creation/1]).
-export([party_not_found_on_retrieval/1]).
-export([party_already_exists/1]).
-export([party_retrieval/1]).

-export([claim_already_accepted_on_accept/1]).
-export([claim_already_accepted_on_deny/1]).
-export([claim_already_accepted_on_revoke/1]).
-export([claim_acceptance/1]).
-export([claim_denial/1]).
-export([claim_revocation/1]).
-export([claim_not_found_on_retrieval/1]).
-export([no_pending_claims/1]).
-export([complex_claim_acceptance/1]).

-export([party_revisioning/1]).
-export([party_get_initial_revision/1]).
-export([party_get_revision/1]).
-export([party_blocking/1]).
-export([party_unblocking/1]).
-export([party_already_blocked/1]).
-export([party_already_unblocked/1]).
-export([party_blocked_on_suspend/1]).
-export([party_suspension/1]).
-export([party_activation/1]).
-export([party_already_suspended/1]).
-export([party_already_active/1]).
-export([party_get_status/1]).

-export([party_meta_retrieval/1]).
-export([party_metadata_setting/1]).
-export([party_metadata_retrieval/1]).
-export([party_metadata_removing/1]).

-export([shop_not_found_on_retrieval/1]).
-export([shop_creation/1]).
-export([shop_aggregation/1]).
-export([shop_terms_retrieval/1]).
-export([shop_already_exists/1]).
-export([shop_update/1]).
-export([shop_update_before_confirm/1]).
-export([shop_update_with_bad_params/1]).
-export([shop_blocking/1]).
-export([shop_unblocking/1]).
-export([shop_already_blocked/1]).
-export([shop_already_unblocked/1]).
-export([shop_blocked_on_suspend/1]).
-export([shop_suspension/1]).
-export([shop_activation/1]).
-export([shop_already_suspended/1]).
-export([shop_already_active/1]).

-export([shop_account_set_retrieval/1]).
-export([shop_account_retrieval/1]).
-export([get_account_state_not_found/1]).

-export([contract_not_found/1]).
-export([contract_creation/1]).
-export([contract_terms_retrieval/1]).
-export([contract_already_exists/1]).
-export([contract_termination/1]).
-export([contract_already_terminated/1]).
-export([contract_expiration/1]).
-export([contract_legal_agreement_binding/1]).
-export([contract_report_preferences_modification/1]).
-export([contract_adjustment_creation/1]).
-export([contract_adjustment_expiration/1]).
-export([contract_w2w_terms/1]).

-export([compute_payment_institution_terms/1]).
-export([compute_payment_institution/1]).

-export([contractor_creation/1]).
-export([contractor_modification/1]).
-export([contract_w_contractor_creation/1]).

-export([compute_provider_ok/1]).
-export([compute_provider_not_found/1]).
-export([compute_provider_terminal_terms_ok/1]).
-export([compute_provider_terminal_terms_global_allow_ok/1]).
-export([compute_provider_terminal_terms_not_found/1]).
-export([compute_provider_terminal_terms_undefined_terms/1]).
-export([compute_provider_terminal_ok/1]).
-export([compute_provider_terminal_empty_varset_ok/1]).
-export([compute_provider_terminal_not_found/1]).
-export([compute_globals_ok/1]).
-export([compute_payment_routing_ruleset_ok/1]).
-export([compute_payment_routing_ruleset_irreducible/1]).
-export([compute_payment_routing_ruleset_not_found/1]).

-export([compute_pred_w_partial_all_of/1]).
-export([compute_pred_w_irreducible_criterion/1]).
-export([compute_pred_w_partially_irreducible_criterion/1]).
-export([compute_terms_w_criteria/1]).
-export([check_all_payment_methods/1]).
-export([check_all_withdrawal_methods/1]).

%% tests descriptions

-type config() :: pm_ct_helper:config().
-type test_case_name() :: pm_ct_helper:test_case_name().
-type group_name() :: pm_ct_helper:group_name().

-define(assert_different_term_sets(T1, T2),
    case T1 =:= T2 of
        true -> error({equal_term_sets, T1, T2});
        false -> ok
    end
).

cfg(Key, C) ->
    pm_ct_helper:cfg(Key, C).

-spec all() -> [{group, group_name()}].
all() ->
    [
        {group, party_creation},
        {group, party_revisioning},
        {group, party_blocking_suspension},
        {group, party_meta},
        {group, party_status},
        {group, contract_management},
        {group, shop_management},
        {group, shop_account_lazy_creation},
        {group, contractor_management},

        {group, claim_management},
        {group, compute},
        {group, terms}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {party_creation, [sequence], [
            party_not_found_on_retrieval,
            party_creation,
            party_already_exists,
            party_retrieval
        ]},
        {party_revisioning, [sequence], [
            party_creation,
            party_get_initial_revision,
            party_revisioning,
            party_get_revision
        ]},
        {party_blocking_suspension, [sequence], [
            party_creation,
            party_blocking,
            party_already_blocked,
            party_blocked_on_suspend,
            party_unblocking,
            party_already_unblocked,
            party_suspension,
            party_already_suspended,
            party_blocking,
            party_unblocking,
            party_activation,
            party_already_active
        ]},
        {party_meta, [sequence], [
            party_creation,
            party_metadata_setting,
            party_metadata_retrieval,
            party_metadata_removing,
            party_meta_retrieval
        ]},
        {party_status, [sequence], [
            party_creation,
            party_get_status
        ]},
        {contract_management, [sequence], [
            party_creation,
            contract_not_found,
            contract_creation,
            contract_terms_retrieval,
            contract_already_exists,
            contract_termination,
            contract_already_terminated,
            contract_expiration,
            contract_legal_agreement_binding,
            contract_report_preferences_modification,
            contract_adjustment_creation,
            contract_adjustment_expiration,
            compute_payment_institution_terms,
            compute_payment_institution,
            contract_w2w_terms
        ]},
        {shop_management, [sequence], [
            party_creation,
            contract_creation,
            shop_not_found_on_retrieval,
            shop_update_before_confirm,
            shop_update_with_bad_params,
            shop_creation,
            shop_aggregation,
            shop_terms_retrieval,
            shop_already_exists,
            shop_update,
            {group, shop_blocking_suspension}
        ]},
        {shop_blocking_suspension, [sequence], [
            shop_blocking,
            shop_already_blocked,
            shop_blocked_on_suspend,
            shop_unblocking,
            shop_already_unblocked,
            shop_suspension,
            shop_already_suspended,
            shop_activation,
            shop_already_active
        ]},
        {contractor_management, [sequence], [
            party_creation,
            contractor_creation,
            contractor_modification,
            contract_w_contractor_creation
        ]},
        {shop_account_lazy_creation, [sequence], [
            party_creation,
            contract_creation,
            shop_creation,
            shop_account_set_retrieval,
            shop_account_retrieval,
            get_account_state_not_found
        ]},
        {claim_management, [sequence], [
            party_creation,
            contract_creation,
            claim_not_found_on_retrieval,
            claim_already_accepted_on_revoke,
            claim_already_accepted_on_accept,
            claim_already_accepted_on_deny,
            shop_creation,
            claim_acceptance,
            claim_denial,
            claim_revocation,
            no_pending_claims,
            complex_claim_acceptance,
            no_pending_claims
        ]},
        {compute, [parallel], [
            compute_provider_ok,
            compute_provider_not_found,
            compute_provider_terminal_terms_ok,
            compute_provider_terminal_terms_global_allow_ok,
            compute_provider_terminal_terms_not_found,
            compute_provider_terminal_terms_undefined_terms,
            compute_provider_terminal_ok,
            compute_provider_terminal_empty_varset_ok,
            compute_provider_terminal_not_found,
            compute_globals_ok,
            compute_payment_routing_ruleset_ok,
            compute_payment_routing_ruleset_irreducible,
            compute_payment_routing_ruleset_not_found
        ]},
        {terms, [sequence], [
            party_creation,
            compute_pred_w_partial_all_of,
            compute_pred_w_irreducible_criterion,
            compute_pred_w_partially_irreducible_criterion,
            compute_terms_w_criteria,
            check_all_payment_methods,
            check_all_withdrawal_methods
        ]}
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    {Apps, _Ret} = pm_ct_helper:start_apps([woody, scoper, dmt_client, epg_connector, progressor, party_management]),
    {_Rev, ObjIds} = pm_domain:insert(construct_domain_fixture()),
    [{apps, Apps}, {objects_ids, ObjIds} | C].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = pm_domain:cleanup(cfg(objects_ids, C)),
    [application:stop(App) || App <- cfg(apps, C)].

%% tests

-spec init_per_group(group_name(), config()) -> config().
init_per_group(shop_blocking_suspension, C) ->
    C;
init_per_group(Group, C) ->
    PartyID = list_to_binary(lists:concat([Group, ".", erlang:system_time()])),
    ApiClient = pm_ct_helper:create_client(),
    Client = pm_client_party:start(PartyID, ApiClient),
    [{party_id, PartyID}, {client, Client} | C].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    pm_client_party:stop(cfg(client, C)).

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok.

%%

-define(party_w_status(ID, Blocking, Suspension), #domain_Party{
    id = ID,
    blocking = Blocking,
    suspension = Suspension
}).

-define(shop_w_status(ID, Blocking, Suspension), #domain_Shop{
    id = ID,
    blocking = Blocking,
    suspension = Suspension
}).

-define(party_not_found(),
    {exception, #payproc_PartyNotFound{}}
).

-define(party_exists(),
    {exception, #payproc_PartyExists{}}
).

-define(invalid_party_revision(),
    {exception, #payproc_InvalidPartyRevision{}}
).

-define(party_blocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?blocked(Reason, _)}}}
).

-define(party_unblocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?unblocked(Reason, _)}}}
).

-define(party_suspended(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?suspended(_)}}}
).

-define(party_active(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?active(_)}}}
).

-define(namespace_not_found(),
    {exception, #payproc_PartyMetaNamespaceNotFound{}}
).

-define(contract_not_found(),
    {exception, #payproc_ContractNotFound{}}
).

-define(shop_not_found(),
    {exception, #payproc_ShopNotFound{}}
).

-define(shop_blocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?blocked(Reason, _)}}}
).

-define(shop_unblocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?unblocked(Reason, _)}}}
).

-define(shop_suspended(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?suspended(_)}}}
).

-define(shop_active(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?active(_)}}}
).

-define(claim(ID), #payproc_Claim{id = ID}).
-define(claim(ID, Status), #payproc_Claim{id = ID, status = Status}).
-define(claim(ID, Status, Changeset), #payproc_Claim{
    id = ID, status = Status, changeset = Changeset
}).

-define(claim_not_found(),
    {exception, #payproc_ClaimNotFound{}}
).

-define(invalid_claim_status(Status),
    {exception, #payproc_InvalidClaimStatus{status = Status}}
).

-define(invalid_changeset(Reason),
    {exception, #payproc_InvalidChangeset{reason = Reason}}
).

-define(REAL_SHOP_ID, <<"SHOP1">>).
-define(REAL_CONTRACTOR_ID, <<"CONTRACTOR1">>).
-define(REAL_CONTRACT_ID, <<"CONTRACT1">>).
-define(REAL_PARTY_PAYMENT_METHODS, [
    ?pmt(bank_card, ?bank_card(<<"maestro">>)),
    ?pmt(bank_card, ?bank_card(<<"mastercard">>)),
    ?pmt(bank_card, ?bank_card(<<"visa">>))
]).

-define(WRONG_DMT_OBJ_ID, 99999).

-spec party_creation(config()) -> _ | no_return().
-spec party_not_found_on_retrieval(config()) -> _ | no_return().
-spec party_already_exists(config()) -> _ | no_return().
-spec party_retrieval(config()) -> _ | no_return().

-spec shop_not_found_on_retrieval(config()) -> _ | no_return().
-spec shop_creation(config()) -> _ | no_return().
-spec shop_aggregation(config()) -> _ | no_return().
-spec shop_terms_retrieval(config()) -> _ | no_return().
-spec shop_already_exists(config()) -> _ | no_return().
-spec shop_update(config()) -> _ | no_return().
-spec shop_update_before_confirm(config()) -> _ | no_return().
-spec shop_update_with_bad_params(config()) -> _ | no_return().

-spec party_get_initial_revision(config()) -> _ | no_return().
-spec party_revisioning(config()) -> _ | no_return().
-spec party_get_revision(config()) -> _ | no_return().

-spec claim_already_accepted_on_revoke(config()) -> _ | no_return().
-spec claim_already_accepted_on_accept(config()) -> _ | no_return().
-spec claim_already_accepted_on_deny(config()) -> _ | no_return().
-spec claim_acceptance(config()) -> _ | no_return().
-spec claim_denial(config()) -> _ | no_return().
-spec claim_revocation(config()) -> _ | no_return().
-spec claim_not_found_on_retrieval(config()) -> _ | no_return().
-spec no_pending_claims(config()) -> _ | no_return().
-spec complex_claim_acceptance(config()) -> _ | no_return().

-spec party_blocking(config()) -> _ | no_return().
-spec party_unblocking(config()) -> _ | no_return().
-spec party_already_blocked(config()) -> _ | no_return().
-spec party_already_unblocked(config()) -> _ | no_return().
-spec party_blocked_on_suspend(config()) -> _ | no_return().
-spec party_suspension(config()) -> _ | no_return().
-spec party_activation(config()) -> _ | no_return().
-spec party_already_suspended(config()) -> _ | no_return().
-spec party_already_active(config()) -> _ | no_return().
-spec party_get_status(config()) -> _ | no_return().

-spec party_meta_retrieval(config()) -> _ | no_return().
-spec party_metadata_setting(config()) -> _ | no_return().
-spec party_metadata_retrieval(config()) -> _ | no_return().
-spec party_metadata_removing(config()) -> _ | no_return().

-spec shop_blocking(config()) -> _ | no_return().
-spec shop_unblocking(config()) -> _ | no_return().
-spec shop_already_blocked(config()) -> _ | no_return().
-spec shop_already_unblocked(config()) -> _ | no_return().
-spec shop_blocked_on_suspend(config()) -> _ | no_return().
-spec shop_suspension(config()) -> _ | no_return().
-spec shop_activation(config()) -> _ | no_return().
-spec shop_already_suspended(config()) -> _ | no_return().
-spec shop_already_active(config()) -> _ | no_return().
-spec shop_account_set_retrieval(config()) -> _ | no_return().
-spec shop_account_retrieval(config()) -> _ | no_return().
-spec get_account_state_not_found(config()) -> _ | no_return().

-spec contract_not_found(config()) -> _ | no_return().
-spec contract_creation(config()) -> _ | no_return().
-spec contract_terms_retrieval(config()) -> _ | no_return().
-spec contract_already_exists(config()) -> _ | no_return().
-spec contract_termination(config()) -> _ | no_return().
-spec contract_already_terminated(config()) -> _ | no_return().
-spec contract_expiration(config()) -> _ | no_return().
-spec contract_legal_agreement_binding(config()) -> _ | no_return().
-spec contract_report_preferences_modification(config()) -> _ | no_return().
-spec contract_adjustment_creation(config()) -> _ | no_return().
-spec contract_adjustment_expiration(config()) -> _ | no_return().
-spec compute_payment_institution_terms(config()) -> _ | no_return().
-spec compute_payment_institution(config()) -> _ | no_return().
-spec contract_w2w_terms(config()) -> _ | no_return().
-spec contractor_creation(config()) -> _ | no_return().
-spec contractor_modification(config()) -> _ | no_return().
-spec contract_w_contractor_creation(config()) -> _ | no_return().

-spec compute_provider_ok(config()) -> _ | no_return().
-spec compute_provider_not_found(config()) -> _ | no_return().
-spec compute_provider_terminal_terms_ok(config()) -> _ | no_return().
-spec compute_provider_terminal_terms_global_allow_ok(config()) -> _ | no_return().
-spec compute_provider_terminal_terms_not_found(config()) -> _ | no_return().
-spec compute_provider_terminal_terms_undefined_terms(config()) -> _ | no_return().
-spec compute_provider_terminal_ok(config()) -> _ | no_return().
-spec compute_provider_terminal_empty_varset_ok(config()) -> _ | no_return().
-spec compute_provider_terminal_not_found(config()) -> _ | no_return().
-spec compute_globals_ok(config()) -> _ | no_return().
-spec compute_payment_routing_ruleset_ok(config()) -> _ | no_return().
-spec compute_payment_routing_ruleset_irreducible(config()) -> _ | no_return().
-spec compute_payment_routing_ruleset_not_found(config()) -> _ | no_return().

-spec compute_pred_w_partial_all_of(config()) -> _ | no_return().
-spec compute_pred_w_irreducible_criterion(config()) -> _ | no_return().
-spec compute_pred_w_partially_irreducible_criterion(config()) -> _ | no_return().
-spec compute_terms_w_criteria(config()) -> _ | no_return().

party_creation(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ContactInfo = #domain_PartyContactInfo{registration_email = <<?MODULE_STRING>>},
    ok = pm_client_party:create(make_party_params(ContactInfo), Client),
    [
        ?party_created(PartyID, ContactInfo, _),
        ?revision_changed(_, 0)
    ] = next_event(Client),
    Party = pm_client_party:get(Client),
    ?party_w_status(PartyID, ?unblocked(_, _), ?active(_)) = Party,
    #domain_Party{contact_info = ContactInfo, shops = Shops, contracts = Contracts} = Party,
    0 = maps:size(Shops),
    0 = maps:size(Contracts).

party_already_exists(C) ->
    ?party_exists() = pm_client_party:create(make_party_params(), cfg(client, C)).

party_not_found_on_retrieval(C) ->
    ?party_not_found() = pm_client_party:get(cfg(client, C)).

party_retrieval(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    #domain_Party{id = PartyID} = pm_client_party:get(Client).

party_get_initial_revision(C) ->
    % NOTE
    % This triggers `pm_party_machine:get_last_revision_old_way/1` codepath.
    Client = cfg(client, C),
    0 = pm_client_party:get_revision(Client).

party_revisioning(C) ->
    Client = cfg(client, C),
    % yesterday
    T0 = pm_datetime:add_interval(pm_datetime:format_now(), {undefined, undefined, -1}),
    ?invalid_party_revision() = pm_client_party:checkout({timestamp, T0}, Client),
    Party1 = pm_client_party:get(Client),
    R1 = Party1#domain_Party.revision,
    T1 = pm_datetime:format_now(),
    Party2 = party_suspension(C),
    R2 = Party2#domain_Party.revision,
    Party1 = pm_client_party:checkout({timestamp, T1}, Client),
    Party1 = pm_client_party:checkout({revision, R1}, Client),
    T2 = pm_datetime:format_now(),
    _ = party_activation(C),
    Party2 = pm_client_party:checkout({timestamp, T2}, Client),
    Party2 = pm_client_party:checkout({revision, R2}, Client),
    Party3 = pm_client_party:get(Client),
    R3 = Party3#domain_Party.revision,
    % tomorrow
    T3 = pm_datetime:add_interval(T2, {undefined, undefined, 1}),
    Party3 = pm_client_party:checkout({timestamp, T3}, Client),
    Party3 = pm_client_party:checkout({revision, R3}, Client),
    ?invalid_party_revision() = pm_client_party:checkout({revision, R3 + 1}, Client).

party_get_revision(C) ->
    Client = cfg(client, C),
    Party1 = pm_client_party:get(Client),
    R1 = Party1#domain_Party.revision,
    R1 = pm_client_party:get_revision(Client),
    Party1 = #domain_Party{revision = R1} = pm_client_party:checkout({revision, R1}, Client),
    Changeset = create_change_set(0),
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    R1 = pm_client_party:get_revision(Client),
    ok = accept_claim(Claim, Client),
    R2 = pm_client_party:get_revision(Client),
    R2 = R1 + 1,
    Party2 = #domain_Party{revision = R2} = pm_client_party:checkout({revision, R2}, Client),
    % some more
    Max = 7,
    Claims = [
        assert_claim_pending(pm_client_party:create_claim(create_change_set(Num), Client), Client)
     || Num <- lists:seq(1, Max)
    ],
    R2 = pm_client_party:get_revision(Client),
    Party2 = pm_client_party:checkout({revision, R2}, Client),
    _Oks = [accept_claim(Cl, Client) || Cl <- Claims],
    R3 = pm_client_party:get_revision(Client),
    R3 = R2 + Max,
    #domain_Party{revision = R3} = pm_client_party:checkout({revision, R3}, Client).

create_change_set(ID) ->
    ContractParams = make_contract_params(),
    BinaryID = erlang:integer_to_binary(ID),
    ContractID = <<?REAL_CONTRACT_ID/binary, BinaryID/binary>>,
    [
        ?contract_modification(ContractID, {creation, ContractParams})
    ].

contract_not_found(C) ->
    ?contract_not_found() = pm_client_party:get_contract(<<"666">>, cfg(client, C)).

contract_creation(C) ->
    Client = cfg(client, C),
    ContractParams = make_contract_params(),
    ContractID = ?REAL_CONTRACT_ID,
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{id = ContractID} = pm_client_party:get_contract(ContractID, Client).

contract_terms_retrieval(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ContractID = ?REAL_CONTRACT_ID,
    Varset = #payproc_ComputeContractTermsVarset{},
    PartyRevision = pm_client_party:get_revision(Client),
    DomainRevision1 = pm_domain:head(),
    Timstamp1 = pm_datetime:format_now(),
    TermSet1 = pm_client_party:compute_contract_terms(
        ContractID,
        Timstamp1,
        {revision, PartyRevision},
        DomainRevision1,
        Varset,
        Client
    ),
    ?assertMatch(
        #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = {value, [?pmt(bank_card, ?bank_card(<<"visa">>))]}
            }
        },
        TermSet1
    ),
    _ = pm_domain:update(construct_term_set_for_party(PartyID, undefined)),
    DomainRevision2 = pm_domain:head(),
    Timstamp2 = pm_datetime:format_now(),
    TermSet2 = pm_client_party:compute_contract_terms(
        ContractID,
        Timstamp2,
        {revision, PartyRevision},
        DomainRevision2,
        Varset,
        Client
    ),
    ?assertMatch(
        #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = {value, ?REAL_PARTY_PAYMENT_METHODS}
            }
        },
        TermSet2
    ).

contract_already_exists(C) ->
    Client = cfg(client, C),
    ContractParams = make_contract_params(),
    ContractID = ?REAL_CONTRACT_ID,
    Changeset = [?contract_modification(ContractID, {creation, ContractParams})],
    ?invalid_changeset(
        ?invalid_contract(
            ContractID,
            {already_exists, ContractID}
        )
    ) = pm_client_party:create_claim(Changeset, Client).

contract_termination(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    Changeset = [?contract_modification(ContractID, ?contract_termination(<<"WHY NOT?!">>))],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {terminated, _}
    } = pm_client_party:get_contract(ContractID, Client).

contract_already_terminated(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    Changeset = [
        ?contract_modification(ContractID, ?contract_termination(<<"JUST TO BE SURE.">>))
    ],
    ?invalid_changeset(
        ?invalid_contract(
            ContractID,
            {invalid_status, _}
        )
    ) = pm_client_party:create_claim(Changeset, Client).

contract_expiration(C) ->
    Client = cfg(client, C),
    ContractParams = make_contract_params(?tmpl(3)),
    ContractID = <<"CONTRACT_EXPIRED">>,
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {expired, _}
    } = pm_client_party:get_contract(ContractID, Client).

contract_legal_agreement_binding(C) ->
    % FIXME how about already terminated contract?
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    LA = #domain_LegalAgreement{
        signed_at = pm_datetime:format_now(),
        legal_agreement_id = <<"20160123-0031235-OGM/GDM">>
    },
    Changeset = [?contract_modification(ContractID, {legal_agreement_binding, LA})],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        legal_agreement = LA
    } = pm_client_party:get_contract(ContractID, Client).

contract_report_preferences_modification(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    Pref1 = #domain_ReportPreferences{},
    Pref2 = #domain_ReportPreferences{
        service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
            schedule = ?bussched(1),
            signer = #domain_Representative{
                position = <<"69">>,
                full_name = <<"Generic Name">>,
                document = {articles_of_association, #domain_ArticlesOfAssociation{}}
            }
        }
    },
    Changeset = [
        ?contract_modification(ContractID, {report_preferences_modification, Pref1}),
        ?contract_modification(ContractID, {report_preferences_modification, Pref2})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        report_preferences = Pref2
    } = pm_client_party:get_contract(ContractID, Client).

contract_adjustment_creation(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    ID = <<"ADJ1">>,
    AdjustmentParams = #payproc_ContractAdjustmentParams{
        template = #domain_ContractTemplateRef{id = 2}
    },
    Changeset = [?contract_modification(ContractID, ?adjustment_creation(ID, AdjustmentParams))],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    } = pm_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments).

contract_adjustment_expiration(C) ->
    Client = cfg(client, C),
    ok = pm_context:save(pm_context:create()),
    ContractID = ?REAL_CONTRACT_ID,
    ID = <<"ADJ2">>,
    Revision = pm_domain:head(),
    Terms = pm_party:get_terms(
        pm_client_party:get_contract(ContractID, Client),
        pm_datetime:format_now(),
        Revision
    ),
    AdjustmentParams = #payproc_ContractAdjustmentParams{
        template = #domain_ContractTemplateRef{id = 4}
    },
    Changeset = [?contract_modification(ContractID, ?adjustment_creation(ID, AdjustmentParams))],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    } = pm_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments),
    true =
        Terms /=
            pm_party:get_terms(
                pm_client_party:get_contract(ContractID, Client),
                pm_datetime:format_now(),
                Revision
            ),
    AfterExpiration = pm_datetime:add_interval(pm_datetime:format_now(), {0, 1, 1}),
    Terms = pm_party:get_terms(
        pm_client_party:get_contract(ContractID, Client), AfterExpiration, Revision
    ),
    pm_context:cleanup().

compute_payment_institution_terms(C) ->
    Client = cfg(client, C),
    TermsFun = fun(Type, Object) ->
        #domain_TermSet{} =
            pm_client_party:compute_payment_institution_terms(
                ?pinst(2),
                #payproc_Varset{payment_method = ?pmt(Type, Object)},
                Client
            )
    end,
    T1 =
        #domain_TermSet{} =
        pm_client_party:compute_payment_institution_terms(
            ?pinst(2),
            #payproc_Varset{},
            Client
        ),
    T2 = TermsFun(bank_card, ?bank_card(<<"visa">>)),
    T3 = TermsFun(payment_terminal, ?pmt_srv(<<"euroset">>)),
    T4 = TermsFun(bank_card, ?bank_card_no_cvv(<<"visa">>)),

    ?assert_different_term_sets(T1, T2),
    ?assert_different_term_sets(T1, T3),
    ?assert_different_term_sets(T1, T4),
    ?assert_different_term_sets(T2, T3),
    ?assert_different_term_sets(T2, T4),
    ?assert_different_term_sets(T3, T4).

compute_payment_institution(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    TermsFun = fun(PartyID) ->
        #domain_PaymentInstitution{} =
            pm_client_party:compute_payment_institution(
                ?pinst(4),
                DomainRevision,
                #payproc_Varset{party_id = PartyID},
                Client
            )
    end,
    T1 = TermsFun(<<"12345">>),
    T2 = TermsFun(<<"67890">>),
    ?assert_different_term_sets(T1, T2).

-spec check_all_payment_methods(config()) -> _.
check_all_payment_methods(C) ->
    Client = cfg(client, C),
    TermsFun0 = fun(Type, Object) ->
        ?assertMatch(
            #domain_TermSet{
                payments = #domain_PaymentsServiceTerms{
                    payment_methods =
                        {value, [_]}
                }
            },
            pm_client_party:compute_payment_institution_terms(
                ?pinst(5),
                #payproc_Varset{payment_method = ?pmt(Type, Object)},
                Client
            )
        ),
        ok
    end,

    TermsFun1 = fun(Type, Object, PaymentTool) ->
        ?assertMatch(
            #domain_TermSet{
                payments = #domain_PaymentsServiceTerms{
                    payment_methods =
                        {value, [_]}
                }
            },
            pm_client_party:compute_payment_institution_terms(
                ?pinst(5),
                #payproc_Varset{payment_method = ?pmt(Type, Object), payment_tool = PaymentTool},
                Client
            )
        ),
        ok
    end,
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{payment_methods = {value, []}}} =
        pm_client_party:compute_payment_institution_terms(
            ?pinst(5),
            #payproc_Varset{payment_method = ?pmt(digital_wallet, ?pmt_srv(<<"wrong-ref">>))},
            Client
        ),

    TermsFun0(bank_card, ?bank_card(<<"visa">>)),
    TermsFun0(payment_terminal, ?pmt_srv(<<"alipay">>)),
    TermsFun0(digital_wallet, ?pmt_srv(<<"qiwi">>)),
    TermsFun0(mobile, ?mob(<<"mts">>)),
    TermsFun0(crypto_currency, ?crypta(<<"bitcoin">>)),
    TermsFun0(bank_card, ?token_bank_card(<<"visa">>, <<"applepay">>)),
    TermsFun0(bank_card, ?bank_card_no_cvv(<<"visa">>)),
    TermsFun0(generic, ?gnrc(?pmt_srv(<<"generic">>))),
    TermsFun1(
        generic,
        ?gnrc(?pmt_srv(<<"generic1">>)),
        {generic,
            ?gnrc_tool(?pmt_srv(<<"generic1">>), #base_Content{
                type = <<"application/json">>,
                data = jsx:encode(#{<<"some_path">> => <<"some_value">>})
            })}
    ).

contract_w2w_terms(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    PartyRevision = pm_client_party:get_revision(Client),
    DomainRevision1 = pm_domain:head(),
    Timstamp1 = pm_datetime:format_now(),
    Varset = #payproc_ComputeContractTermsVarset{
        currency = ?cur(<<"RUB">>),
        amount = ?cash(2500, <<"RUB">>)
    },
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            w2w = W2WServiceTerms
        }
    } = pm_client_party:compute_contract_terms(
        ContractID,
        Timstamp1,
        {revision, PartyRevision},
        DomainRevision1,
        Varset,
        Client
    ),
    #domain_W2WServiceTerms{fees = Fees} = W2WServiceTerms,
    {value, #domain_Fees{
        fees = #{surplus := {fixed, #domain_CashVolumeFixed{cash = ?cash(50, <<"RUB">>)}}}
    }} = Fees.

-spec check_all_withdrawal_methods(config()) -> _.
check_all_withdrawal_methods(C) ->
    Client = cfg(client, C),
    TermsFun = fun(Type, Object) ->
        ?assertMatch(
            #domain_TermSet{
                wallets = #domain_WalletServiceTerms{
                    withdrawals = #domain_WithdrawalServiceTerms{
                        methods = {value, [?pmt(bank_card, ?bank_card(<<"visa">>))]}
                    }
                }
            },
            pm_client_party:compute_payment_institution_terms(
                ?pinst(2),
                #payproc_Varset{payment_method = ?pmt(Type, Object)},
                Client
            )
        ),
        ok
    end,

    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            withdrawals = #domain_WithdrawalServiceTerms{methods = {value, []}}
        }
    } =
        pm_client_party:compute_payment_institution_terms(
            ?pinst(2),
            #payproc_Varset{payment_method = ?pmt(bank_card, ?bank_card(<<"wrong-ref">>))},
            Client
        ),

    TermsFun(bank_card, ?bank_card(<<"visa">>)),
    TermsFun(digital_wallet, ?pmt_srv(<<"qiwi">>)),
    TermsFun(mobile, ?mob(<<"mts">>)),
    TermsFun(crypto_currency, ?crypta(<<"bitcoin">>)).

shop_not_found_on_retrieval(C) ->
    Client = cfg(client, C),
    ?shop_not_found() = pm_client_party:get_shop(<<"666">>, Client).

shop_creation(C) ->
    Client = cfg(client, C),
    Details = pm_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
    ContractID = ?REAL_CONTRACT_ID,
    ShopID = ?REAL_SHOP_ID,
    Params = #payproc_ShopParams{
        category = ?cat(2),
        location = {url, <<"https://somename.somedomain/p/123?redirect=1">>},
        details = Details,
        contract_id = ContractID
    },
    ShopAccountParams = #payproc_ShopAccountParams{currency = ?cur(<<"RUB">>)},
    Changeset = [
        ?shop_modification(ShopID, {creation, Params}),
        ?shop_modification(ShopID, {shop_account_creation, ShopAccountParams})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ?claim(_, _, Changeset) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Shop{
        id = ShopID,
        details = Details,
        account = #domain_ShopAccount{currency = ?cur(<<"RUB">>)}
    } = pm_client_party:get_shop(ShopID, Client).

shop_aggregation(C) ->
    Client = cfg(client, C),
    #payproc_ShopContract{
        shop = #domain_Shop{id = ?REAL_SHOP_ID},
        contract = #domain_Contract{id = ?REAL_CONTRACT_ID}
    } = pm_client_party:get_shop_contract(?REAL_SHOP_ID, Client).

shop_terms_retrieval(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = ?REAL_SHOP_ID,
    Timestamp = pm_datetime:format_now(),
    VS = #payproc_ComputeShopTermsVarset{},
    TermSet1 = pm_client_party:compute_shop_terms(
        ShopID, Timestamp, {timestamp, Timestamp}, VS, Client
    ),
    ?assertMatch(
        #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = {value, [?pmt(bank_card, ?bank_card(<<"visa">>))]}
            }
        },
        TermSet1
    ),
    _ = pm_domain:update(construct_term_set_for_party(PartyID, {shop_is, ShopID})),
    TermSet2 = pm_client_party:compute_shop_terms(
        ShopID, pm_datetime:format_now(), {timestamp, Timestamp}, VS, Client
    ),
    ?assertMatch(
        #domain_TermSet{
            payments = #domain_PaymentsServiceTerms{
                payment_methods = {value, ?REAL_PARTY_PAYMENT_METHODS}
            }
        },
        TermSet2
    ).

shop_already_exists(C) ->
    Client = cfg(client, C),
    Details = pm_ct_helper:make_shop_details(
        <<"THRlFT SHOP">>, <<"Hot. Fancy. Almost like thrift.">>
    ),
    ContractID = ?REAL_CONTRACT_ID,
    ShopID = ?REAL_SHOP_ID,
    Params = #payproc_ShopParams{
        category = ?cat(2),
        location = {url, <<"https://s0mename.s0med0main">>},
        details = Details,
        contract_id = ContractID
    },
    Changeset = [?shop_modification(ShopID, {creation, Params})],
    ?invalid_changeset(?invalid_shop(ShopID, {already_exists, _})) = pm_client_party:create_claim(
        Changeset, Client
    ).

shop_update(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Details = pm_ct_helper:make_shop_details(<<"BARBER SHOP">>, <<"Nice. Short. Clean.">>),
    Changeset1 = [?shop_modification(ShopID, {details_modification, Details})],
    Claim1 = assert_claim_pending(pm_client_party:create_claim(Changeset1, Client), Client),
    ok = accept_claim(Claim1, Client),
    #domain_Shop{details = Details} = pm_client_party:get_shop(ShopID, Client),

    Location = {url, <<"suspicious_url">>},
    Changeset2 = [?shop_modification(ShopID, {location_modification, Location})],
    Claim2 = assert_claim_pending(pm_client_party:create_claim(Changeset2, Client), Client),
    ok = accept_claim(Claim2, Client),
    #domain_Shop{location = Location, details = Details} = pm_client_party:get_shop(ShopID, Client),

    ContractID = <<"CONTRACT_IN_DIFFERENT_PAYMENT_INST">>,
    Changeset3 = [
        ?contract_modification(ContractID, {creation, make_contract_params(?tmpl(2), ?pinst(3))}),
        ?shop_modification(ShopID, ?shop_contract_modification(ContractID))
    ],
    Claim3 = assert_claim_pending(pm_client_party:create_claim(Changeset3, Client), Client),
    ok = accept_claim(Claim3, Client),
    #domain_Shop{
        location = Location,
        details = Details,
        contract_id = ContractID
    } = pm_client_party:get_shop(ShopID, Client).

shop_update_before_confirm(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    ShopID = <<"SHOP2">>,
    Params = #payproc_ShopParams{
        location = {url, <<"">>},
        details = pm_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
        contract_id = ContractID
    },
    Changeset1 = [?shop_modification(ShopID, {creation, Params})],
    Claim0 = assert_claim_pending(pm_client_party:create_claim(Changeset1, Client), Client),
    ?shop_not_found() = pm_client_party:get_shop(ShopID, Client),
    NewCategory = ?cat(3),
    NewDetails = pm_ct_helper:make_shop_details(<<"BARBIES SHOP">>, <<"Hot. Short. Clean.">>),
    ShopAccountParams = #payproc_ShopAccountParams{currency = ?cur(<<"RUB">>)},
    Changeset2 = [
        ?shop_modification(ShopID, {category_modification, NewCategory}),
        ?shop_modification(ShopID, {details_modification, NewDetails}),
        ?shop_modification(ShopID, {shop_account_creation, ShopAccountParams})
    ],
    ok = update_claim(Claim0, Changeset2, Client),
    Claim1 = pm_client_party:get_claim(pm_claim:get_id(Claim0), Client),
    ok = accept_claim(Claim1, Client),
    #domain_Shop{category = NewCategory, details = NewDetails} = pm_client_party:get_shop(
        ShopID, Client
    ).

shop_update_with_bad_params(C) ->
    % FIXME add more invalid params checks
    Client = cfg(client, C),
    ShopID = <<"SHOP2">>,
    ContractID = <<"CONTRACT3">>,
    ContractParams = make_contract_params(#domain_ContractTemplateRef{id = 5}),
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),

    Claim1 =
        #payproc_Claim{id = ID1, revision = Rev1} = assert_claim_pending(
            pm_client_party:create_claim(
                [?shop_modification(ShopID, {category_modification, ?cat(1)})],
                Client
            ),
            Client
        ),
    ?invalid_changeset(_CategoryError) = pm_client_party:accept_claim(ID1, Rev1, Client),
    ok = revoke_claim(Claim1, Client).

claim_acceptance(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Details = pm_ct_helper:make_shop_details(<<"McDolan">>),
    Location = {url, <<"very_suspicious_url">>},
    Changeset = [
        ?shop_modification(ShopID, {details_modification, Details}),
        ?shop_modification(ShopID, {location_modification, Location})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{location = Location, details = Details} = pm_client_party:get_shop(ShopID, Client).

claim_denial(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Shop = pm_client_party:get_shop(ShopID, Client),
    Location = {url, <<"Pr0nHub">>},
    Changeset = [?shop_modification(ShopID, {location_modification, Location})],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = deny_claim(Claim, Client),
    Shop = pm_client_party:get_shop(ShopID, Client).

claim_revocation(C) ->
    Client = cfg(client, C),
    Party = pm_client_party:get(Client),
    ShopID = <<"SHOP3">>,
    ContractID = ?REAL_CONTRACT_ID,
    Params = #payproc_ShopParams{
        location = {url, <<"https://url3">>},
        details = pm_ct_helper:make_shop_details(<<"OOPS">>),
        contract_id = ContractID
    },
    Changeset = [?shop_modification(ShopID, {creation, Params})],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = revoke_claim(Claim, Client),
    Party = pm_client_party:get(Client),
    ?shop_not_found() = pm_client_party:get_shop(ShopID, Client).

complex_claim_acceptance(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    ShopID1 = <<"SHOP4">>,
    Params1 = #payproc_ShopParams{
        location = {url, <<"https://url4">>},
        category = ?cat(2),
        details = Details1 = pm_ct_helper:make_shop_details(<<"SHOP4">>),
        contract_id = ContractID
    },
    ShopID2 = <<"SHOP5">>,
    Params2 = #payproc_ShopParams{
        location = {url, <<"http://url5">>},
        category = ?cat(3),
        details = Details2 = pm_ct_helper:make_shop_details(<<"SHOP5">>),
        contract_id = ContractID
    },
    PartyName = <<"PartyName">>,
    PartyComment = <<"PartyComment">>,
    Emails = [],
    ShopAccountParams = #payproc_ShopAccountParams{currency = ?cur(<<"RUB">>)},
    Claim1 = assert_claim_pending(
        pm_client_party:create_claim(
            [
                ?shop_modification(ShopID1, {creation, Params1}),
                ?shop_modification(ShopID1, {shop_account_creation, ShopAccountParams}),
                ?additional_info_modification(PartyName, PartyComment, Emails)
            ],
            Client
        ),
        Client
    ),
    ok = pm_client_party:suspend(Client),
    [?party_suspension(?suspended(_)), ?revision_changed(_, _)] = next_event(Client),
    ok = pm_client_party:activate(Client),
    [?party_suspension(?active(_)), ?revision_changed(_, _)] = next_event(Client),
    Claim1 = pm_client_party:get_claim(pm_claim:get_id(Claim1), Client),

    Claim2 = assert_claim_pending(
        pm_client_party:create_claim(
            [
                ?shop_modification(ShopID2, {creation, Params2}),
                ?shop_modification(ShopID2, {shop_account_creation, ShopAccountParams}),
                ?additional_info_modification(PartyName, PartyComment, Emails)
            ],
            Client
        ),
        Client
    ),
    ok = update_claim(
        Claim1, [?shop_modification(ShopID1, {category_modification, ?cat(3)})], Client
    ),
    Claim1_1 = pm_client_party:get_claim(pm_claim:get_id(Claim1), Client),
    true = Claim1#payproc_Claim.changeset =/= Claim1_1#payproc_Claim.changeset,
    true = Claim1#payproc_Claim.revision =/= Claim1_1#payproc_Claim.revision,
    ok = accept_claim(Claim2, Client),
    ok = accept_claim(Claim1_1, Client),
    #domain_Party{
        party_name = PartyName,
        comment = PartyComment,
        contact_info = #domain_PartyContactInfo{manager_contact_emails = Emails}
    } = pm_client_party:get(Client),
    #domain_Shop{details = Details1, category = ?cat(3)} = pm_client_party:get_shop(
        ShopID1, Client
    ),
    #domain_Shop{details = Details2} = pm_client_party:get_shop(ShopID2, Client).

claim_already_accepted_on_revoke(C) ->
    Client = cfg(client, C),
    Reason = <<"The End is near">>,
    Claim = get_first_accepted_claim(Client),
    ?invalid_claim_status(?accepted(_)) = pm_client_party:revoke_claim(
        pm_claim:get_id(Claim),
        pm_claim:get_revision(Claim),
        Reason,
        Client
    ).

claim_already_accepted_on_accept(C) ->
    Client = cfg(client, C),
    Claim = get_first_accepted_claim(Client),
    ?invalid_claim_status(?accepted(_)) = pm_client_party:accept_claim(
        pm_claim:get_id(Claim),
        pm_claim:get_revision(Claim),
        Client
    ).

claim_already_accepted_on_deny(C) ->
    Client = cfg(client, C),
    Reason = <<"I am about to destroy them">>,
    Claim = get_first_accepted_claim(Client),
    ?invalid_claim_status(?accepted(_)) = pm_client_party:deny_claim(
        pm_claim:get_id(Claim),
        pm_claim:get_revision(Claim),
        Reason,
        Client
    ).

get_first_accepted_claim(Client) ->
    Claims = lists:filter(
        fun(?claim(_, Status)) ->
            case Status of
                ?accepted(_) ->
                    true;
                _ ->
                    false
            end
        end,
        pm_client_party:get_claims(Client)
    ),
    case Claims of
        [Claim | _] ->
            Claim;
        [] ->
            error(accepted_claim_not_found)
    end.

claim_not_found_on_retrieval(C) ->
    Client = cfg(client, C),
    ?claim_not_found() = pm_client_party:get_claim(-666, Client).

no_pending_claims(C) ->
    Client = cfg(client, C),
    Claims = pm_client_party:get_claims(Client),
    [] = lists:filter(
        fun
            (?claim(_, ?pending())) ->
                true;
            (_) ->
                false
        end,
        Claims
    ),
    ok.

party_blocking(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    Reason = <<"i said so">>,
    ok = pm_client_party:block(Reason, Client),
    [?party_blocking(?blocked(Reason, _)), ?revision_changed(_, _)] = next_event(Client),
    ?party_w_status(PartyID, ?blocked(Reason, _), _) = pm_client_party:get(Client).

party_unblocking(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    Reason = <<"enough">>,
    ok = pm_client_party:unblock(Reason, Client),
    [?party_blocking(?unblocked(Reason, _)), ?revision_changed(_, _)] = next_event(Client),
    ?party_w_status(PartyID, ?unblocked(Reason, _), _) = pm_client_party:get(Client).

party_already_blocked(C) ->
    Client = cfg(client, C),
    ?party_blocked(_) = pm_client_party:block(<<"too much">>, Client).

party_already_unblocked(C) ->
    Client = cfg(client, C),
    ?party_unblocked(_) = pm_client_party:unblock(<<"too free">>, Client).

party_blocked_on_suspend(C) ->
    Client = cfg(client, C),
    ?party_blocked(_) = pm_client_party:suspend(Client).

party_suspension(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ok = pm_client_party:suspend(Client),
    [?party_suspension(?suspended(_)), ?revision_changed(_, _)] = next_event(Client),
    ?party_w_status(PartyID, _, ?suspended(_)) = pm_client_party:get(Client).

party_activation(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ok = pm_client_party:activate(Client),
    [?party_suspension(?active(_)), ?revision_changed(_, _)] = next_event(Client),
    ?party_w_status(PartyID, _, ?active(_)) = pm_client_party:get(Client).

party_already_suspended(C) ->
    Client = cfg(client, C),
    ?party_suspended() = pm_client_party:suspend(Client).

party_already_active(C) ->
    Client = cfg(client, C),
    ?party_active() = pm_client_party:activate(Client).

party_metadata_setting(C) ->
    Client = cfg(client, C),
    NS = pm_ct_helper:make_meta_ns(),
    Data = pm_ct_helper:make_meta_data(NS),
    ok = pm_client_party:set_metadata(NS, Data, Client),
    % lets check for idempotency
    ok = pm_client_party:set_metadata(NS, Data, Client).

party_metadata_retrieval(C) ->
    Client = cfg(client, C),
    ?namespace_not_found() = pm_client_party:get_metadata(<<"NoSuchNamespace">>, Client),
    NS = pm_ct_helper:make_meta_ns(),
    Data0 = pm_ct_helper:make_meta_data(),
    ok = pm_client_party:set_metadata(NS, Data0, Client),
    Data0 = pm_client_party:get_metadata(NS, Client),
    % lets change it and check again
    Data1 = pm_ct_helper:make_meta_data(NS),
    ok = pm_client_party:set_metadata(NS, Data1, Client),
    Data1 = pm_client_party:get_metadata(NS, Client).

party_metadata_removing(C) ->
    Client = cfg(client, C),
    ?namespace_not_found() = pm_client_party:remove_metadata(<<"NoSuchNamespace">>, Client),
    NS = pm_ct_helper:make_meta_ns(),
    ok = pm_client_party:set_metadata(NS, pm_ct_helper:make_meta_data(), Client),
    ok = pm_client_party:remove_metadata(NS, Client),
    ?namespace_not_found() = pm_client_party:remove_metadata(NS, Client).

party_meta_retrieval(C) ->
    Client = cfg(client, C),
    Meta0 = pm_client_party:get_meta(Client),
    NS = pm_ct_helper:make_meta_ns(),
    ok = pm_client_party:set_metadata(NS, pm_ct_helper:make_meta_data(), Client),
    Meta1 = pm_client_party:get_meta(Client),
    Meta0 =/= Meta1.

party_get_status(C) ->
    Client = cfg(client, C),
    Status0 = pm_client_party:get_status(Client),
    ?active(_) = Status0#domain_PartyStatus.suspension,
    ?unblocked(_) = Status0#domain_PartyStatus.blocking,
    ok = pm_client_party:block(<<"too much">>, Client),
    Status1 = pm_client_party:get_status(Client),
    ?active(_) = Status1#domain_PartyStatus.suspension,
    ?blocked(<<"too much">>, _) = Status1#domain_PartyStatus.blocking,
    Status1 =/= Status0.

shop_blocking(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Reason = <<"i said so">>,
    ok = pm_client_party:block_shop(ShopID, Reason, Client),
    [?shop_blocking(ShopID, ?blocked(Reason, _)), ?revision_changed(_, _)] = next_event(Client),
    ?shop_w_status(ShopID, ?blocked(Reason, _), _) = pm_client_party:get_shop(ShopID, Client).

shop_unblocking(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Reason = <<"enough">>,
    ok = pm_client_party:unblock_shop(ShopID, Reason, Client),
    [?shop_blocking(ShopID, ?unblocked(Reason, _)), ?revision_changed(_, _)] = next_event(Client),
    ?shop_w_status(ShopID, ?unblocked(Reason, _), _) = pm_client_party:get_shop(ShopID, Client).

shop_already_blocked(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_blocked(_) = pm_client_party:block_shop(ShopID, <<"too much">>, Client).

shop_already_unblocked(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_unblocked(_) = pm_client_party:unblock_shop(ShopID, <<"too free">>, Client).

shop_blocked_on_suspend(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_blocked(_) = pm_client_party:suspend_shop(ShopID, Client).

shop_suspension(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ok = pm_client_party:suspend_shop(ShopID, Client),
    [?shop_suspension(ShopID, ?suspended(_)), ?revision_changed(_, _)] = next_event(Client),
    ?shop_w_status(ShopID, _, ?suspended(_)) = pm_client_party:get_shop(ShopID, Client).

shop_activation(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ok = pm_client_party:activate_shop(ShopID, Client),
    [?shop_suspension(ShopID, ?active(_)), ?revision_changed(_, _)] = next_event(Client),
    ?shop_w_status(ShopID, _, ?active(_)) = pm_client_party:get_shop(ShopID, Client).

shop_already_suspended(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_suspended() = pm_client_party:suspend_shop(ShopID, Client).

shop_already_active(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_active() = pm_client_party:activate_shop(ShopID, Client).

shop_account_set_retrieval(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    S = #domain_ShopAccount{} = pm_client_party:get_shop_account(ShopID, Client),
    {save_config, S}.

shop_account_retrieval(C) ->
    Client = cfg(client, C),
    {shop_account_set_retrieval, #domain_ShopAccount{guarantee = AccountID}} = ?config(
        saved_config, C
    ),
    #payproc_AccountState{account_id = AccountID} = pm_client_party:get_account_state(
        AccountID, Client
    ).

get_account_state_not_found(C) ->
    Client = cfg(client, C),
    {exception, #payproc_AccountNotFound{}} =
        (catch pm_client_party:get_account_state(420, Client)).

%%

contractor_creation(C) ->
    Client = cfg(client, C),
    ContractorParams = make_contractor_params(),
    ContractorID = ?REAL_CONTRACTOR_ID,
    Changeset = [
        ?contractor_modification(ContractorID, {creation, ContractorParams})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    Party = pm_client_party:get(Client),
    #domain_PartyContractor{} = pm_party:get_contractor(ContractorID, Party).

contractor_modification(C) ->
    Client = cfg(client, C),
    ContractorID = ?REAL_CONTRACTOR_ID,
    Party1 = pm_client_party:get(Client),
    #domain_PartyContractor{} = C1 = pm_party:get_contractor(ContractorID, Party1),
    Changeset = [
        ?contractor_modification(ContractorID, {identification_level_modification, full}),
        ?contractor_modification(
            ContractorID,
            {
                identity_documents_modification,
                #payproc_ContractorIdentityDocumentsModification{
                    identity_documents = [<<"some_binary">>, <<"and_even_more_binary">>]
                }
            }
        )
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    Party2 = pm_client_party:get(Client),
    #domain_PartyContractor{} = C2 = pm_party:get_contractor(ContractorID, Party2),
    C1 /= C2 orelse error(same_contractor).

contract_w_contractor_creation(C) ->
    Client = cfg(client, C),
    ContractorID = ?REAL_CONTRACTOR_ID,
    ContractParams = make_contract_w_contractor_params(ContractorID),
    ContractID = ?REAL_CONTRACT_ID,
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams})
    ],
    Claim = assert_claim_pending(pm_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{id = ContractID, contractor_id = ContractorID} = pm_client_party:get_contract(
        ContractID, Client
    ).

%% Compute providers

compute_provider_ok(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    Varset = #payproc_Varset{
        currency = ?cur(<<"RUB">>)
    },
    CashFlow = ?cfpost(
        {system, settlement},
        {provider, settlement},
        {product,
            {min_of,
                ?ordset([
                    ?fixed(10, <<"RUB">>),
                    ?share(5, 100, operation_amount, round_half_towards_zero)
                ])}}
    ),
    #domain_Provider{
        terms = #domain_ProvisionTermSet{
            payments = #domain_PaymentsProvisionTerms{
                allow = {constant, true},
                cash_flow = {value, [CashFlow]}
            },
            recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                cash_value = {value, ?cash(1000, <<"RUB">>)}
            }
        }
    } = pm_client_party:compute_provider(?prv(1), DomainRevision, Varset, Client).

compute_provider_not_found(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    {exception, #payproc_ProviderNotFound{}} =
        (catch pm_client_party:compute_provider(
            ?prv(?WRONG_DMT_OBJ_ID), DomainRevision, #payproc_Varset{}, Client
        )).

compute_provider_terminal_terms_ok(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    Varset = #payproc_Varset{
        payment_tool = {bank_card, #domain_BankCard{token = <<>>, bin = <<>>, last_digits = <<>>}},
        currency = ?cur(<<"RUB">>)
    },
    CashFlow = ?cfpost(
        {system, settlement},
        {provider, settlement},
        {product,
            {min_of,
                ?ordset([
                    ?fixed(10, <<"RUB">>),
                    ?share(5, 100, operation_amount, round_half_towards_zero)
                ])}}
    ),
    PaymentMethods = ?ordset([?pmt(bank_card, ?bank_card(<<"visa">>))]),
    #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_flow = {value, [CashFlow]},
            payment_methods = {value, PaymentMethods},
            turnover_limits =
                {value, [
                    %% In ordset fashion
                    #domain_TurnoverLimit{
                        id = <<"p_card_day_count">>,
                        upper_boundary = 1,
                        domain_revision = undefined
                    },
                    #domain_TurnoverLimit{
                        id = <<"payment_card_month_amount_rub">>,
                        upper_boundary = 7500000,
                        domain_revision = undefined
                    },
                    #domain_TurnoverLimit{
                        id = <<"payment_card_month_count">>,
                        upper_boundary = 10,
                        domain_revision = undefined
                    },
                    #domain_TurnoverLimit{
                        id = <<"payment_day_amount_rub">>,
                        upper_boundary = 5000000,
                        domain_revision = undefined
                    }
                ]}
        },
        recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
            cash_value = {value, ?cash(1000, <<"RUB">>)}
        }
    } = pm_client_party:compute_provider_terminal_terms(
        ?prv(1), ?trm(1), DomainRevision, Varset, Client
    ).

compute_provider_terminal_terms_global_allow_ok(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    Varset0 = #payproc_Varset{
        amount = ?cash(100, <<"RUB">>),
        party_id = <<"PARTYID1">>
    },
    ?assertEqual(
        #domain_ProvisionTermSet{
            payments = #domain_PaymentsProvisionTerms{
                allow = {constant, false},
                global_allow = {constant, false}
            }
        },
        pm_client_party:compute_provider_terminal_terms(
            ?prv(3), ?trm(5), DomainRevision, Varset0, Client
        )
    ),
    Varset1 = Varset0#payproc_Varset{party_id = <<"PARTYID2">>},
    ?assertEqual(
        #domain_ProvisionTermSet{
            payments = #domain_PaymentsProvisionTerms{
                allow = {constant, true},
                global_allow = {constant, false}
            }
        },
        pm_client_party:compute_provider_terminal_terms(
            ?prv(3), ?trm(5), DomainRevision, Varset1, Client
        )
    ),
    Varset2 = Varset0#payproc_Varset{amount = ?cash(101, <<"RUB">>)},
    ?assertEqual(
        #domain_ProvisionTermSet{
            payments = #domain_PaymentsProvisionTerms{
                allow = {constant, false},
                global_allow = {constant, true}
            }
        },
        pm_client_party:compute_provider_terminal_terms(
            ?prv(3), ?trm(5), DomainRevision, Varset2, Client
        )
    ).

compute_provider_terminal_terms_not_found(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    {exception, #payproc_TerminalNotFound{}} =
        pm_client_party:compute_provider_terminal_terms(
            ?prv(1),
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client
        ),
    {exception, #payproc_ProviderNotFound{}} =
        pm_client_party:compute_provider_terminal_terms(
            ?prv(?WRONG_DMT_OBJ_ID),
            ?trm(1),
            DomainRevision,
            #payproc_Varset{},
            Client
        ),
    {exception, #payproc_ProviderNotFound{}} =
        pm_client_party:compute_provider_terminal_terms(
            ?prv(?WRONG_DMT_OBJ_ID),
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client
        ).

compute_provider_terminal_terms_undefined_terms(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    ?assertMatch(
        {exception, #payproc_ProvisionTermSetUndefined{}},
        pm_client_party:compute_provider_terminal_terms(
            ?prv(2),
            ?trm(4),
            DomainRevision,
            #payproc_Varset{},
            Client
        )
    ).

compute_provider_terminal_ok(C) ->
    Client = cfg(client, C),
    Revision = pm_domain:head(),
    Varset = #payproc_Varset{
        currency = ?cur(<<"RUB">>)
    },
    ExpectedCashflow = ?cfpost(
        {system, settlement},
        {provider, settlement},
        {product,
            {min_of,
                ?ordset([
                    ?fixed(10, <<"RUB">>),
                    ?share(5, 100, operation_amount, round_half_towards_zero)
                ])}}
    ),
    ExpectedPaymentMethods = ?ordset([
        ?pmt(bank_card, ?bank_card(<<"visa">>))
    ]),
    ?assertMatch(
        #payproc_ProviderTerminal{
            ref = ?trm(1),
            name = <<"Brominal 1">>,
            description = <<"Brominal 1">>,
            provider = #payproc_ProviderDetails{
                ref = ?prv(1),
                name = <<"Brovider">>,
                description = <<"A provider but bro">>
            },
            proxy = #domain_ProxyDefinition{
                name = <<"Dummy proxy">>,
                url = <<"http://dummy.proxy/">>,
                options = #{
                    <<"proxy">> := <<"def">>,
                    <<"pro">> := <<"vader">>,
                    <<"term">> := <<"inal">>,
                    <<"override_proxy">> := <<"proxydef">>,
                    <<"override_provider">> := <<"provider">>,
                    <<"override_terminal">> := <<"terminal">>
                }
            },
            terms = #domain_ProvisionTermSet{
                payments = #domain_PaymentsProvisionTerms{
                    cash_flow = {value, [ExpectedCashflow]},
                    payment_methods = {value, ExpectedPaymentMethods}
                },
                recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                    cash_value = {value, ?cash(1000, <<"RUB">>)}
                }
            }
        },
        pm_client_party:compute_provider_terminal(?trm(1), Revision, Varset, Client)
    ).

compute_provider_terminal_empty_varset_ok(C) ->
    Client = cfg(client, C),
    Revision = pm_domain:head(),
    ?assertMatch(
        #payproc_ProviderTerminal{
            ref = ?trm(1),
            provider = #payproc_ProviderDetails{
                ref = ?prv(1)
            },
            proxy = #domain_ProxyDefinition{
                url = <<"http://dummy.proxy/">>,
                options = #{
                    <<"override_proxy">> := <<"proxydef">>,
                    <<"override_provider">> := <<"provider">>,
                    <<"override_terminal">> := <<"terminal">>
                }
            },
            terms = undefined
        },
        pm_client_party:compute_provider_terminal(?trm(1), Revision, undefined, Client)
    ).

compute_provider_terminal_not_found(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    ?assertMatch(
        {exception, #payproc_TerminalNotFound{}},
        pm_client_party:compute_provider_terminal(
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client
        )
    ).

compute_globals_ok(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    Varset = #payproc_Varset{},
    #domain_Globals{
        external_account_set = {value, ?eas(1)}
    } = pm_client_party:compute_globals(DomainRevision, Varset, Client).

compute_payment_routing_ruleset_ok(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    Varset = #payproc_Varset{
        party_id = <<"67890">>
    },
    #domain_RoutingRuleset{
        name = <<"Rule#1">>,
        decisions =
            {candidates, [
                #domain_RoutingCandidate{
                    terminal = ?trm(2),
                    allowed = {constant, true}
                },
                #domain_RoutingCandidate{
                    terminal = ?trm(3),
                    allowed = {constant, true}
                },
                #domain_RoutingCandidate{
                    terminal = ?trm(1),
                    allowed = {constant, true}
                }
            ]}
    } = pm_client_party:compute_routing_ruleset(?ruleset(1), DomainRevision, Varset, Client).

compute_payment_routing_ruleset_irreducible(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    Varset = #payproc_Varset{},
    #domain_RoutingRuleset{
        name = <<"Rule#1">>,
        decisions =
            {delegates, [
                #domain_RoutingDelegate{
                    allowed = {condition, {party, #domain_PartyCondition{id = <<"12345">>}}},
                    ruleset = ?ruleset(2)
                },
                #domain_RoutingDelegate{
                    allowed = {condition, {party, #domain_PartyCondition{id = <<"67890">>}}},
                    ruleset = ?ruleset(3)
                },
                #domain_RoutingDelegate{
                    allowed = {constant, true},
                    ruleset = ?ruleset(4)
                }
            ]}
    } = pm_client_party:compute_routing_ruleset(?ruleset(1), DomainRevision, Varset, Client).

compute_payment_routing_ruleset_not_found(C) ->
    Client = cfg(client, C),
    DomainRevision = pm_domain:head(),
    {exception, #payproc_RuleSetNotFound{}} =
        (catch pm_client_party:compute_routing_ruleset(
            ?ruleset(5), DomainRevision, #payproc_Varset{}, Client
        )).

%%

compute_pred_w_partial_all_of(_) ->
    Revision = pm_domain:head(),
    Predicate =
        {all_of, [
            {constant, true},
            {condition, {currency_is, ?cur(<<"CNY">>)}},
            Cond1 = {condition, {category_is, ?cat(42)}},
            Cond2 = {condition, {shop_location_is, {url, <<"https://thisiswhyimbroke.com">>}}}
        ]},
    ?assertMatch(
        {all_of, [Cond1, Cond2]},
        pm_selector:reduce_predicate(
            Predicate,
            #{currency => ?cur(<<"CNY">>)},
            Revision
        )
    ).

compute_pred_w_irreducible_criterion(_) ->
    CriterionRef = ?crit(1),
    pm_ct_domain:with(
        [
            pm_ct_fixture:construct_criterion(
                CriterionRef,
                <<"HAHA">>,
                {condition, {currency_is, ?cur(<<"KZT">>)}}
            )
        ],
        fun(Revision) ->
            ?assertMatch(
                {criterion, CriterionRef},
                pm_selector:reduce_predicate({criterion, CriterionRef}, #{}, Revision)
            )
        end
    ).

compute_pred_w_partially_irreducible_criterion(_) ->
    CriterionRef = ?crit(1),
    pm_ct_domain:with(
        [
            pm_ct_fixture:construct_criterion(
                CriterionRef,
                <<"HAHA GOT ME">>,
                {all_of, [
                    {constant, true},
                    {is_not, {condition, {currency_is, ?cur(<<"KZT">>)}}}
                ]}
            )
        ],
        fun(Revision) ->
            ?assertMatch(
                {is_not, {condition, {currency_is, ?cur(<<"KZT">>)}}},
                pm_selector:reduce_predicate({criterion, CriterionRef}, #{}, Revision)
            )
        end
    ).

compute_terms_w_criteria(C) ->
    Client = cfg(client, C),
    CritRef = ?crit(1),
    CritBase = ?crit(10),
    TemplateRef = ?tmpl(10),
    CashLimitHigh = ?cashrng(
        {inclusive, ?cash(10, <<"KZT">>)},
        {exclusive, ?cash(1000, <<"KZT">>)}
    ),
    CashLimitLow = ?cashrng(
        {inclusive, ?cash(10, <<"KZT">>)},
        {exclusive, ?cash(100, <<"KZT">>)}
    ),
    WasRevision = pm_domain:head(),
    % TODO it's a weak point for cleanup, as we don't update Config with new IDs
    {_, _NewIDs0} = pm_ct_domain:upsert(
        WasRevision,
        pm_ct_fixture:construct_criterion(
            CritBase,
            <<"Visas">>,
            {all_of,
                ?ordset([
                    {condition,
                        {payment_tool,
                            {bank_card, #domain_BankCardCondition{
                                definition =
                                    {payment_system, #domain_PaymentSystemCondition{
                                        payment_system_is = ?pmt_sys(<<"visa">>)
                                    }}
                            }}}},
                    {is_not,
                        {condition,
                            {payment_tool,
                                {bank_card, #domain_BankCardCondition{
                                    definition = {empty_cvv_is, true}
                                }}}}}
                ])}
        )
    ),
    {_, _NewIDs1} = pm_ct_domain:with(
        [
            pm_ct_fixture:construct_criterion(
                CritRef,
                <<"Kazakh Visas">>,
                {all_of,
                    ?ordset([
                        {condition, {currency_is, ?cur(<<"KZT">>)}},
                        {criterion, CritBase}
                    ])}
            ),
            pm_ct_fixture:construct_contract_template(
                TemplateRef,
                ?trms(10)
            ),
            pm_ct_fixture:construct_term_set_hierarchy(
                ?trms(10),
                ?trms(2),
                #domain_TermSet{
                    payments = #domain_PaymentsServiceTerms{
                        cash_limit =
                            {decisions, [
                                #domain_CashLimitDecision{
                                    if_ = {criterion, CritRef},
                                    then_ = {value, CashLimitHigh}
                                },
                                #domain_CashLimitDecision{
                                    if_ = {is_not, {criterion, CritRef}},
                                    then_ = {value, CashLimitLow}
                                }
                            ]}
                    }
                }
            )
        ],
        fun(Revision) ->
            ContractID = pm_ct_helper:create_contract(TemplateRef, ?pinst(1), Client),
            PartyRevision = pm_client_party:get_revision(Client),
            Timstamp = pm_datetime:format_now(),
            ?assertMatch(
                #domain_TermSet{
                    payments = #domain_PaymentsServiceTerms{cash_limit = {value, CashLimitHigh}}
                },
                pm_client_party:compute_contract_terms(
                    ContractID,
                    Timstamp,
                    {revision, PartyRevision},
                    Revision,
                    #payproc_ComputeContractTermsVarset{
                        currency = ?cur(<<"KZT">>),
                        payment_tool = ?bank_card_payment_tool(<<"bank">>)
                    },
                    Client
                )
            ),
            ?assertMatch(
                #domain_TermSet{
                    payments = #domain_PaymentsServiceTerms{cash_limit = {value, CashLimitLow}}
                },
                pm_client_party:compute_contract_terms(
                    ContractID,
                    Timstamp,
                    {revision, PartyRevision},
                    Revision,
                    #payproc_ComputeContractTermsVarset{
                        currency = ?cur(<<"KZT">>),
                        payment_tool = ?bank_card_payment_tool(<<"bank">>, true)
                    },
                    Client
                )
            ),
            ?assertMatch(
                #domain_TermSet{
                    payments = #domain_PaymentsServiceTerms{cash_limit = {value, CashLimitLow}}
                },
                pm_client_party:compute_contract_terms(
                    ContractID,
                    Timstamp,
                    {revision, PartyRevision},
                    Revision,
                    #payproc_ComputeContractTermsVarset{
                        currency = ?cur(<<"RUB">>),
                        payment_tool = ?bank_card_payment_tool(<<"bank">>)
                    },
                    Client
                )
            )
        end
    ).

%%

update_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Changeset, Client) ->
    ok = pm_client_party:update_claim(ClaimID, Revision, Changeset, Client),
    NextRevision = Revision + 1,
    [?claim_updated(ClaimID, Changeset, NextRevision, _)] = next_event(Client),
    ok.

accept_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = pm_client_party:accept_claim(ClaimID, Revision, Client),
    NextRevision = Revision + 1,
    [?claim_status_changed(ClaimID, ?accepted(_), NextRevision, _), ?revision_changed(_, _)] = next_event(
        Client
    ),
    ok.

deny_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = pm_client_party:deny_claim(ClaimID, Revision, Reason = <<"The Reason">>, Client),
    NextRevision = Revision + 1,
    [?claim_status_changed(ClaimID, ?denied(Reason), NextRevision, _)] = next_event(Client),
    ok.

revoke_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = pm_client_party:revoke_claim(ClaimID, Revision, undefined, Client),
    NextRevision = Revision + 1,
    [?claim_status_changed(ClaimID, ?revoked(undefined), NextRevision, _)] = next_event(Client),
    ok.

assert_claim_pending(?claim(ClaimID, ?pending()) = Claim, Client) ->
    [?claim_created(?claim(ClaimID))] = next_event(Client),
    Claim.

%%

next_event(Client) ->
    case pm_client_party:pull_event(Client) of
        ?party_ev(Event) ->
            Event;
        Result ->
            Result
    end.

%%

make_party_params() ->
    make_party_params(#domain_PartyContactInfo{registration_email = <<?MODULE_STRING>>}).

make_party_params(ContactInfo) ->
    #payproc_PartyParams{contact_info = ContactInfo}.

make_contract_params() ->
    make_contract_params(undefined).

make_contract_params(TemplateRef) ->
    make_contract_params(TemplateRef, ?pinst(2)).

make_contract_params(TemplateRef, PaymentInstitutionRef) ->
    pm_ct_helper:make_battle_ready_contract_params(TemplateRef, PaymentInstitutionRef).

make_contract_w_contractor_params(ContractorID) ->
    #payproc_ContractParams{
        contractor_id = ContractorID,
        template = undefined,
        payment_institution = ?pinst(2)
    }.

make_contractor_params() ->
    pm_ct_helper:make_battle_ready_contractor().

construct_term_set_for_party(PartyID, Def) ->
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies =
                {value,
                    ordsets:from_list([
                        ?cur(<<"RUB">>),
                        ?cur(<<"USD">>)
                    ])},
            categories =
                {value,
                    ordsets:from_list([
                        ?cat(2),
                        ?cat(3)
                    ])},
            payment_methods =
                {decisions, [
                    #domain_PaymentMethodDecision{
                        if_ = ?partycond(PartyID, Def),
                        then_ = {value, ordsets:from_list(?REAL_PARTY_PAYMENT_METHODS)}
                    },
                    #domain_PaymentMethodDecision{
                        if_ = {constant, true},
                        then_ =
                            {value,
                                ordsets:from_list([
                                    ?pmt(bank_card, ?bank_card(<<"visa">>))
                                ])}
                    }
                ]}
        }
    },
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = ?trms(2),
        data = #domain_TermSetHierarchy{
            parent_terms = undefined,
            term_sets = [
                #domain_TimedTermSet{
                    action_time = #base_TimestampInterval{},
                    terms = TermSet
                }
            ]
        }
    }}.

-spec construct_domain_fixture() -> [pm_domain:object()].
construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
            categories = {value, ordsets:from_list([?cat(1)])}
        }
    },
    DefaultTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies =
                {value,
                    ordsets:from_list([
                        ?cur(<<"RUB">>),
                        ?cur(<<"USD">>)
                    ])},
            categories =
                {value,
                    ordsets:from_list([
                        ?cat(2),
                        ?cat(3)
                    ])},
            payment_methods =
                {value,
                    ordsets:from_list([
                        ?pmt(bank_card, ?bank_card(<<"visa">>))
                    ])}
        }
    },

    AllMethodsTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            payment_methods =
                {decisions, [
                    %% For check_all_payment_methods
                    mk_payment_decision(
                        {bank_card, #domain_BankCardCondition{
                            definition = {
                                payment_system,
                                #domain_PaymentSystemCondition{
                                    payment_system_is = ?pmt_sys(<<"visa">>)
                                }
                            }
                        }},
                        [?pmt(bank_card, ?bank_card(<<"visa">>))]
                    ),
                    mk_payment_decision(
                        {payment_terminal, #domain_PaymentTerminalCondition{
                            definition = {
                                payment_service_is,
                                ?pmt_srv(<<"alipay">>)
                            }
                        }},
                        [?pmt(payment_terminal, ?pmt_srv(<<"alipay">>))]
                    ),
                    mk_payment_decision(
                        {digital_wallet, #domain_DigitalWalletCondition{
                            definition =
                                {payment_service_is, ?pmt_srv(<<"qiwi">>)}
                        }},
                        [?pmt(digital_wallet, ?pmt_srv(<<"qiwi">>))]
                    ),
                    mk_payment_decision(
                        {mobile_commerce, #domain_MobileCommerceCondition{
                            definition = {operator_is, ?mob(<<"mts">>)}
                        }},
                        [?pmt(mobile, ?mob(<<"mts">>))]
                    ),
                    mk_payment_decision(
                        {crypto_currency, #domain_CryptoCurrencyCondition{
                            definition = {crypto_currency_is, ?crypta(<<"bitcoin">>)}
                        }},
                        [?pmt(crypto_currency, ?crypta(<<"bitcoin">>))]
                    ),
                    mk_payment_decision(
                        {bank_card, #domain_BankCardCondition{
                            definition =
                                {payment_system, #domain_PaymentSystemCondition{
                                    token_service_is = ?token_srv(<<"applepay">>)
                                }}
                        }},
                        [?pmt(bank_card, ?token_bank_card(<<"visa">>, <<"applepay">>))]
                    ),
                    mk_payment_decision(
                        {generic, {payment_service_is, ?pmt_srv(<<"generic">>)}},
                        [?pmt(generic, ?gnrc(?pmt_srv(<<"generic">>)))]
                    ),
                    mk_payment_decision(
                        {generic, {resource_field_matches, ?gnrc_cond([<<"some_path">>], <<"some_value">>)}},
                        [?pmt(generic, ?gnrc(?pmt_srv(<<"generic1">>)))]
                    ),
                    #domain_PaymentMethodDecision{
                        if_ = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{}}}},
                        then_ =
                            {value, ordsets:from_list([?pmt(bank_card, ?bank_card(<<"mastercard">>))])}
                    },
                    #domain_PaymentMethodDecision{
                        if_ =
                            {condition, {payment_tool, {payment_terminal, #domain_PaymentTerminalCondition{}}}},
                        then_ =
                            {value, ordsets:from_list([?pmt(payment_terminal, ?pmt_srv(<<"euroset">>))])}
                    },
                    #domain_PaymentMethodDecision{
                        if_ = {constant, true},
                        then_ = {value, ordsets:from_list([])}
                    }
                ]},
            cash_limit =
                {value, #domain_CashRange{
                    lower = {inclusive, #domain_Cash{amount = 1000, currency = ?cur(<<"RUB">>)}},
                    upper = {exclusive, #domain_Cash{amount = 4200000, currency = ?cur(<<"RUB">>)}}
                }},
            fees =
                {value, [
                    ?cfpost(
                        {merchant, settlement},
                        {system, settlement},
                        ?share(45, 1000, operation_amount)
                    )
                ]}
        }
    },

    TermSet = #domain_TermSet{
        recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods =
                {decisions, [
                    mk_payment_decision(
                        {bank_card, #domain_BankCardCondition{
                            definition = {issuer_bank_is, ?bank(1)}
                        }},
                        [
                            ?pmt(bank_card, ?bank_card(<<"visa">>)),
                            ?pmt(crypto_currency, ?crypta(<<"bitcoin">>))
                        ]
                    ),
                    mk_payment_decision(
                        {bank_card, #domain_BankCardCondition{definition = {empty_cvv_is, true}}},
                        []
                    ),
                    mk_payment_decision(
                        {bank_card, #domain_BankCardCondition{}},
                        [?pmt(bank_card, ?bank_card(<<"visa">>))]
                    ),
                    mk_payment_decision(
                        {payment_terminal, #domain_PaymentTerminalCondition{}},
                        [?pmt(crypto_currency, ?crypta(<<"bitcoin">>))]
                    ),
                    #domain_PaymentMethodDecision{
                        if_ = {constant, true},
                        then_ = {value, ordsets:from_list([])}
                    }
                ]}
        },
        payments = #domain_PaymentsServiceTerms{
            cash_limit =
                {value, #domain_CashRange{
                    lower = {inclusive, #domain_Cash{amount = 1000, currency = ?cur(<<"RUB">>)}},
                    upper = {exclusive, #domain_Cash{amount = 4200000, currency = ?cur(<<"RUB">>)}}
                }},
            fees =
                {value, [
                    ?cfpost(
                        {merchant, settlement},
                        {system, settlement},
                        ?share(45, 1000, operation_amount)
                    )
                ]}
        },
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
            wallet_limit =
                {decisions, [
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"RUB">>)},
                                    {exclusive, ?cash(5000001, <<"RUB">>)}
                                )}
                    },
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(0, <<"USD">>)},
                                    {exclusive, ?cash(10000001, <<"USD">>)}
                                )}
                    }
                ]},
            withdrawals = #domain_WithdrawalServiceTerms{
                methods =
                    {decisions, [
                        mk_payment_decision(
                            {bank_card, #domain_BankCardCondition{
                                definition = {
                                    payment_system,
                                    #domain_PaymentSystemCondition{
                                        payment_system_is = ?pmt_sys(<<"visa">>)
                                    }
                                }
                            }},
                            [?pmt(bank_card, ?bank_card(<<"visa">>))]
                        ),
                        mk_payment_decision(
                            {digital_wallet, #domain_DigitalWalletCondition{
                                definition =
                                    {payment_service_is, ?pmt_srv(<<"qiwi">>)}
                            }},
                            [?pmt(bank_card, ?bank_card(<<"visa">>))]
                        ),
                        mk_payment_decision(
                            {mobile_commerce, #domain_MobileCommerceCondition{
                                definition = {operator_is, ?mob(<<"mts">>)}
                            }},
                            [?pmt(bank_card, ?bank_card(<<"visa">>))]
                        ),
                        mk_payment_decision(
                            {crypto_currency, #domain_CryptoCurrencyCondition{
                                definition = {crypto_currency_is, ?crypta(<<"bitcoin">>)}
                            }},
                            [?pmt(bank_card, ?bank_card(<<"visa">>))]
                        ),
                        #domain_PaymentMethodDecision{
                            if_ = {constant, true},
                            then_ = {value, ordsets:from_list([])}
                        }
                    ]}
            },
            w2w = #domain_W2WServiceTerms{
                currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                cash_limit =
                    {decisions, [
                        #domain_CashLimitDecision{
                            if_ =
                                {any_of,
                                    ordsets:from_list([
                                        {any_of,
                                            ordsets:from_list([
                                                {condition, {currency_is, ?cur(<<"RUB">>)}},
                                                {condition,
                                                    {payment_tool,
                                                        {bank_card, #domain_BankCardCondition{
                                                            definition =
                                                                {payment_system, #domain_PaymentSystemCondition{
                                                                    payment_system_is = #domain_PaymentSystemRef{
                                                                        id = <<"visa">>
                                                                    }
                                                                }}
                                                        }}}}
                                            ])},
                                        {all_of,
                                            ordsets:from_list([
                                                {condition,
                                                    {cost_in,
                                                        ?cashrng(
                                                            {inclusive, ?cash(424242, <<"USD">>)},
                                                            {inclusive, ?cash(424242, <<"USD">>)}
                                                        )}}
                                            ])}
                                    ])},
                            then_ =
                                {value,
                                    ?cashrng(
                                        {inclusive, ?cash(0, <<"RUB">>)},
                                        {exclusive, ?cash(10000001, <<"RUB">>)}
                                    )}
                        }
                    ]},
                cash_flow =
                    {decisions, [
                        #domain_CashFlowDecision{
                            if_ =
                                {condition,
                                    {cost_in,
                                        ?cashrng(
                                            {inclusive, ?cash(0, <<"RUB">>)},
                                            {exclusive, ?cash(3000, <<"RUB">>)}
                                        )}},
                            then_ = {
                                value,
                                [
                                    #domain_CashFlowPosting{
                                        source = {wallet, receiver_destination},
                                        destination = {system, settlement},
                                        volume = ?fixed(50, <<"RUB">>)
                                    }
                                ]
                            }
                        },
                        #domain_CashFlowDecision{
                            if_ =
                                {condition,
                                    {cost_in,
                                        ?cashrng(
                                            {inclusive, ?cash(3001, <<"RUB">>)},
                                            {exclusive, ?cash(10000, <<"RUB">>)}
                                        )}},
                            then_ = {
                                value,
                                [
                                    #domain_CashFlowPosting{
                                        source = {wallet, receiver_destination},
                                        destination = {system, settlement},
                                        volume = ?share(1, 100, operation_amount)
                                    }
                                ]
                            }
                        }
                    ]},
                fees =
                    {decisions, [
                        #domain_FeeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {decisions, [
                                    #domain_FeeDecision{
                                        if_ =
                                            {condition,
                                                {cost_in,
                                                    ?cashrng(
                                                        {inclusive, ?cash(0, <<"RUB">>)},
                                                        {exclusive, ?cash(3000, <<"RUB">>)}
                                                    )}},
                                        then_ =
                                            {value, #domain_Fees{
                                                fees = #{surplus => ?fixed(50, <<"RUB">>)}
                                            }}
                                    },
                                    #domain_FeeDecision{
                                        if_ =
                                            {condition,
                                                {cost_in,
                                                    ?cashrng(
                                                        {inclusive, ?cash(3000, <<"RUB">>)},
                                                        {exclusive, ?cash(300000, <<"RUB">>)}
                                                    )}},
                                        then_ =
                                            {value, #domain_Fees{
                                                fees = #{
                                                    surplus => ?share(4, 100, operation_amount)
                                                }
                                            }}
                                    }
                                ]}
                        }
                    ]}
            }
        }
    },
    Decision1 =
        {delegates, [
            #domain_RoutingDelegate{
                allowed = {condition, {party, #domain_PartyCondition{id = <<"12345">>}}},
                ruleset = ?ruleset(2)
            },
            #domain_RoutingDelegate{
                allowed = {condition, {party, #domain_PartyCondition{id = <<"67890">>}}},
                ruleset = ?ruleset(3)
            },
            #domain_RoutingDelegate{
                allowed = {constant, true},
                ruleset = ?ruleset(4)
            }
        ]},
    Decision2 =
        {candidates, [
            #domain_RoutingCandidate{
                allowed = {constant, true},
                terminal = ?trm(1)
            }
        ]},
    Decision3 =
        {candidates, [
            #domain_RoutingCandidate{
                allowed = {condition, {party, #domain_PartyCondition{id = <<"67890">>}}},
                terminal = ?trm(2)
            },
            #domain_RoutingCandidate{
                allowed = {constant, true},
                terminal = ?trm(3)
            },
            #domain_RoutingCandidate{
                allowed = {constant, true},
                terminal = ?trm(1)
            }
        ]},
    Decision4 =
        {candidates, [
            #domain_RoutingCandidate{
                allowed = {constant, true},
                terminal = ?trm(3)
            }
        ]},
    [
        pm_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        pm_ct_fixture:construct_currency(?cur(<<"USD">>)),
        pm_ct_fixture:construct_currency(?cur(<<"KZT">>)),

        pm_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        pm_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),
        pm_ct_fixture:construct_category(?cat(3), <<"Guns & Booze">>, live),
        pm_ct_fixture:construct_category(?cat(4), <<"Tech Store">>, live),
        pm_ct_fixture:construct_category(?cat(5), <<"Burger Boutique">>, live),

        pm_ct_fixture:construct_payment_system(?pmt_sys(<<"visa">>), <<"Visa">>),
        pm_ct_fixture:construct_payment_system(?pmt_sys(<<"mastercard">>), <<"Mastercard">>),
        pm_ct_fixture:construct_payment_system(?pmt_sys(<<"maestro">>), <<"Maestro">>),
        pm_ct_fixture:construct_payment_system(?pmt_sys(<<"jcb">>), <<"JCB">>),
        pm_ct_fixture:construct_payment_service(?pmt_srv(<<"alipay">>), <<"Euroset">>),
        pm_ct_fixture:construct_payment_service(?pmt_srv(<<"qiwi">>), <<"Qiwi">>),
        pm_ct_fixture:construct_mobile_operator(?mob(<<"mts">>), <<"MTS">>),
        pm_ct_fixture:construct_crypto_currency(?crypta(<<"bitcoin">>), <<"Bitcoin">>),
        pm_ct_fixture:construct_tokenized_service(?token_srv(<<"applepay">>), <<"Apple Pay">>),
        pm_ct_fixture:construct_payment_service(?pmt_srv(<<"generic">>), <<"Generic">>),
        pm_ct_fixture:construct_payment_service(?pmt_srv(<<"generic1">>), <<"Generic1">>),

        pm_ct_fixture:construct_payment_method(?pmt(bank_card, ?bank_card(<<"visa">>))),
        pm_ct_fixture:construct_payment_method(?pmt(bank_card, ?bank_card(<<"mastercard">>))),
        pm_ct_fixture:construct_payment_method(?pmt(bank_card, ?bank_card(<<"maestro">>))),
        pm_ct_fixture:construct_payment_method(?pmt(bank_card, ?bank_card(<<"jcb">>))),
        pm_ct_fixture:construct_payment_method(
            ?pmt(bank_card, ?token_bank_card(<<"visa">>, <<"applepay">>))
        ),
        pm_ct_fixture:construct_payment_method(?pmt(payment_terminal, ?pmt_srv(<<"alipay">>))),
        pm_ct_fixture:construct_payment_method(?pmt(digital_wallet, ?pmt_srv(<<"qiwi">>))),
        pm_ct_fixture:construct_payment_method(?pmt(mobile, ?mob(<<"mts">>))),
        pm_ct_fixture:construct_payment_method(?pmt(crypto_currency, ?crypta(<<"bitcoin">>))),
        pm_ct_fixture:construct_payment_method(?pmt(generic, ?gnrc(?pmt_srv(<<"generic">>)))),
        pm_ct_fixture:construct_payment_method(?pmt(generic, ?gnrc(?pmt_srv(<<"generic1">>)))),

        pm_ct_fixture:construct_payment_method(?pmt(payment_terminal, ?pmt_srv(<<"euroset">>))),
        pm_ct_fixture:construct_payment_method(?pmt(bank_card, ?bank_card_no_cvv(<<"visa">>))),

        pm_ct_fixture:construct_proxy(
            ?prx(1),
            <<"Dummy proxy">>,
            <<"http://dummy.proxy/">>,
            #{
                <<"proxy">> => <<"def">>,
                <<"override_proxy">> => <<"proxydef">>,
                <<"override_provider">> => <<"proxydef">>,
                <<"override_terminal">> => <<"proxydef">>
            }
        ),

        pm_ct_fixture:construct_inspector(?insp(1), <<"Dummy Inspector">>, ?prx(1)),
        pm_ct_fixture:construct_system_account_set(?sas(1)),
        pm_ct_fixture:construct_system_account_set(?sas(2)),
        pm_ct_fixture:construct_external_account_set(?eas(1)),

        pm_ct_fixture:construct_business_schedule(?bussched(1)),

        pm_ct_fixture:construct_payment_routing_ruleset(?ruleset(1), <<"Rule#1">>, Decision1),
        pm_ct_fixture:construct_payment_routing_ruleset(?ruleset(2), <<"Rule#2">>, Decision2),
        pm_ct_fixture:construct_payment_routing_ruleset(?ruleset(3), <<"Rule#3">>, Decision3),
        pm_ct_fixture:construct_payment_routing_ruleset(?ruleset(4), <<"Rule#4">>, Decision4),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = test
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(3),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(4),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set =
                    {decisions, [
                        #domain_SystemAccountSetDecision{
                            if_ = ?partycond(<<"12345">>, undefined),
                            then_ =
                                {value, ?sas(2)}
                        },
                        #domain_SystemAccountSetDecision{
                            if_ = ?partycond(<<"67890">>, undefined),
                            then_ =
                                {value, ?sas(1)}
                        }
                    ]},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        %% For check_all_payment_methods
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(5),
            data = #domain_PaymentInstitution{
                name = <<"All Payments GmbH">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(6)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set =
                    {decisions, [
                        #domain_ExternalAccountSetDecision{
                            if_ = {constant, true},
                            then_ = {value, ?eas(1)}
                        }
                    ]},
                payment_institutions = ?ordset([?pinst(1), ?pinst(2), ?pinst(5)])
            }
        }},
        pm_ct_fixture:construct_contract_template(
            ?tmpl(1),
            ?trms(1)
        ),
        pm_ct_fixture:construct_contract_template(
            ?tmpl(2),
            ?trms(3)
        ),
        pm_ct_fixture:construct_contract_template(
            ?tmpl(3),
            ?trms(2),
            {interval, #domain_LifetimeInterval{years = -1}},
            {interval, #domain_LifetimeInterval{days = -1}}
        ),
        pm_ct_fixture:construct_contract_template(
            ?tmpl(4),
            ?trms(1),
            undefined,
            {interval, #domain_LifetimeInterval{months = 1}}
        ),
        pm_ct_fixture:construct_contract_template(
            ?tmpl(5),
            ?trms(4)
        ),
        %% For check_all_payment_methods
        pm_ct_fixture:construct_contract_template(
            ?tmpl(6),
            ?trms(5)
        ),
        pm_ct_fixture:construct_term_set_hierarchy(?trms(1), undefined, TestTermSet),
        pm_ct_fixture:construct_term_set_hierarchy(?trms(2), undefined, DefaultTermSet),
        pm_ct_fixture:construct_term_set_hierarchy(?trms(3), ?trms(2), TermSet),
        pm_ct_fixture:construct_term_set_hierarchy(
            ?trms(4),
            ?trms(3),
            #domain_TermSet{
                payments = #domain_PaymentsServiceTerms{
                    currencies =
                        {value,
                            ordsets:from_list([
                                ?cur(<<"RUB">>)
                            ])},
                    categories =
                        {value,
                            ordsets:from_list([
                                ?cat(2)
                            ])},
                    payment_methods =
                        {value,
                            ordsets:from_list([
                                ?pmt(bank_card, ?bank_card(<<"visa">>))
                            ])}
                }
            }
        ),
        %% For check_all_payment_methods
        pm_ct_fixture:construct_term_set_hierarchy(?trms(5), ?trms(2), AllMethodsTermSet),
        {bank, #domain_BankObject{
            ref = ?bank(1),
            data = #domain_Bank{
                name = <<"Test BIN range">>,
                description = <<"Test BIN range">>,
                bins = ordsets:from_list([<<"1234">>, <<"5678">>])
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                realm = test,
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"pro">> => <<"vader">>,
                        <<"override_provider">> => <<"provider">>,
                        <<"override_terminal">> => <<"provider">>
                    }
                },
                accounts = pm_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        allow = {constant, true},
                        currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                        categories = {value, ?ordset([?cat(1)])},
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa">>)),
                                    ?pmt(bank_card, ?bank_card(<<"mastercard">>))
                                ])},
                        cash_limit =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(1000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )},
                        cash_flow =
                            {decisions, [
                                #domain_CashFlowDecision{
                                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                    then_ =
                                        {value, [
                                            ?cfpost(
                                                {system, settlement},
                                                {provider, settlement},
                                                {product,
                                                    {min_of,
                                                        ?ordset([
                                                            ?fixed(10, <<"RUB">>),
                                                            ?share(
                                                                5,
                                                                100,
                                                                operation_amount,
                                                                round_half_towards_zero
                                                            )
                                                        ])}}
                                            )
                                        ]}
                                },
                                #domain_CashFlowDecision{
                                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                                    then_ =
                                        {value, [
                                            ?cfpost(
                                                {system, settlement},
                                                {provider, settlement},
                                                {product,
                                                    {min_of,
                                                        ?ordset([
                                                            ?fixed(10, <<"USD">>),
                                                            ?share(
                                                                5,
                                                                100,
                                                                operation_amount,
                                                                round_half_towards_zero
                                                            )
                                                        ])}}
                                            )
                                        ]}
                                }
                            ]},
                        turnover_limits =
                            {decisions, [
                                #domain_TurnoverLimitDecision{
                                    if_ =
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {issuer_bank_is, #domain_BankRef{id = 1}}
                                                }}}},
                                    then_ =
                                        {value,
                                            ?ordset([
                                                #domain_TurnoverLimit{
                                                    id = <<"payment_card_month_count">>,
                                                    upper_boundary = 5,
                                                    domain_revision = undefined
                                                },
                                                %% Common limits
                                                #domain_TurnoverLimit{
                                                    id = <<"payment_card_month_amount_rub">>,
                                                    upper_boundary = 7500000,
                                                    domain_revision = undefined
                                                },
                                                #domain_TurnoverLimit{
                                                    id = <<"payment_day_amount_rub">>,
                                                    upper_boundary = 5000000,
                                                    domain_revision = undefined
                                                },
                                                #domain_TurnoverLimit{
                                                    id = <<"p_card_day_count">>,
                                                    upper_boundary = 1,
                                                    domain_revision = undefined
                                                }
                                            ])}
                                },
                                #domain_TurnoverLimitDecision{
                                    if_ =
                                        {is_not,
                                            {condition,
                                                {payment_tool,
                                                    {bank_card, #domain_BankCardCondition{
                                                        definition =
                                                            {issuer_bank_is, #domain_BankRef{
                                                                id = 1
                                                            }}
                                                    }}}}},
                                    then_ =
                                        {value,
                                            ?ordset([
                                                #domain_TurnoverLimit{
                                                    id = <<"payment_card_month_count">>,
                                                    upper_boundary = 10,
                                                    domain_revision = undefined
                                                },
                                                %% Common limits
                                                #domain_TurnoverLimit{
                                                    id = <<"payment_card_month_amount_rub">>,
                                                    upper_boundary = 7500000,
                                                    domain_revision = undefined
                                                },
                                                #domain_TurnoverLimit{
                                                    id = <<"payment_day_amount_rub">>,
                                                    upper_boundary = 5000000,
                                                    domain_revision = undefined
                                                },
                                                #domain_TurnoverLimit{
                                                    id = <<"p_card_day_count">>,
                                                    upper_boundary = 1,
                                                    domain_revision = undefined
                                                }
                                            ])}
                                }
                            ]}
                    },
                    recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                        categories = {value, ?ordset([?cat(1)])},
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa">>)),
                                    ?pmt(bank_card, ?bank_card(<<"mastercard">>))
                                ])},
                        cash_value =
                            {decisions, [
                                #domain_CashValueDecision{
                                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                    then_ = {value, ?cash(1000, <<"RUB">>)}
                                },
                                #domain_CashValueDecision{
                                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                                    then_ = {value, ?cash(1000, <<"USD">>)}
                                }
                            ]}
                    }
                }
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = #domain_Provider{
                name = <<"Provider 2">>,
                description = <<"Provider without terms">>,
                realm = test,
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                accounts = pm_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)])
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                realm = test,
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"pro">> => <<"vader">>
                    }
                },
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        allow = ?partycond(<<"PARTYID1">>, undefined),
                        global_allow =
                            {condition,
                                {cost_in,
                                    ?cashrng(
                                        {inclusive, ?cash(100, <<"RUB">>)},
                                        {inclusive, ?cash(100, <<"RUB">>)}
                                    )}}
                    }
                }
            }
        }},

        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                provider_ref = ?prv(1),
                options = #{
                    <<"term">> => <<"inal">>,
                    <<"override_terminal">> => <<"terminal">>
                },
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa">>))
                                ])}
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(2),
            data = #domain_Terminal{
                name = <<"Brominal 2">>,
                description = <<"Brominal 2">>,
                provider_ref = ?prv(1),
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa">>))
                                ])}
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(3),
            data = #domain_Terminal{
                name = <<"Brominal 3">>,
                description = <<"Brominal 3">>,
                provider_ref = ?prv(1),
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa">>))
                                ])}
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(4),
            data = #domain_Terminal{
                name = <<"Terminal 4">>,
                description = <<"Terminal without terms">>,
                provider_ref = ?prv(2)
            }
        }},

        {terminal, #domain_TerminalObject{
            ref = ?trm(5),
            data = #domain_Terminal{
                name = <<"Brominal 5">>,
                description = <<"Brominal 5">>,
                provider_ref = ?prv(3),
                options = #{
                    <<"term">> => <<"inal">>,
                    <<"override_terminal">> => <<"terminal">>
                },
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        allow = ?partycond(<<"PARTYID2">>, undefined),
                        global_allow =
                            {condition,
                                {cost_in,
                                    ?cashrng(
                                        {inclusive, ?cash(101, <<"RUB">>)},
                                        {inclusive, ?cash(101, <<"RUB">>)}
                                    )}}
                    }
                }
            }
        }}
    ].

mk_payment_decision(PaymentTool, PaymentMethods) ->
    #domain_PaymentMethodDecision{
        if_ = {condition, {payment_tool, PaymentTool}},
        then_ = {value, ordsets:from_list(PaymentMethods)}
    }.
