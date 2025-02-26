-module(pm_party_machine).

-include("party_events.hrl").
-include("legacy_party_structures.hrl").

-include_lib("damsel/include/dmsl_claimmgmt_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("pm_proto/include/pm_state_thrift.hrl").

-include("claim_management.hrl").

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_notification/4]).

%%
-export([namespace/0]).
-export([start/3]).
-export([get_party/1]).
-export([checkout/2]).
-export([call/4]).
-export([get_claim/2]).
-export([get_claims/1]).
-export([get_public_history/3]).
-export([get_meta/1]).
-export([get_metadata/2]).
-export([get_last_revision/1]).
-export([get_status/1]).

%%

-define(NS, party).
-define(STEP, 5).
-define(SNAPSHOT_STEP, 10).
-define(CT_ERLANG_BINARY, <<"application/x-erlang-binary">>).

-type st() :: #state_State{}.

-type service_name() :: atom().

-type call_target() :: party | {shop, shop_id()}.

-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type party_status() :: pm_party:party_status().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type claim_id() :: dmsl_payproc_thrift:'ClaimID'().
-type claim() :: dmsl_payproc_thrift:'Claim'().
-type meta() :: dmsl_domain_thrift:'PartyMeta'().
-type meta_ns() :: dmsl_domain_thrift:'PartyMetaNamespace'().
-type meta_data() :: dmsl_domain_thrift:'PartyMetaData'().
-type party_revision_param() :: dmsl_payproc_thrift:'PartyRevisionParam'().
-type party_revision() :: dmsl_domain_thrift:'PartyRevision'().
-type event_id() :: non_neg_integer().

-type content_type() :: binary().
-type party_aux_st() :: #{
    snapshot_index := snapshot_index(),
    party_revision_index := party_revision_index(),
    last_event_id => event_id()
}.

-type snapshot_index() :: [event_id()].
-type party_revision_index() :: #{
    party_revision() => event_range()
}.

-type event_range() :: {
    FromEventID :: event_id() | undefined,
    ToEventID :: event_id() | undefined
}.

-type woody_context() :: woody_context:ctx().

-type event() :: _.

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(event(), _).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result() :: machinery:result(event(), map()).
-type repair_response() :: ok.
-type response(T) :: machinery:response(T).

-export_type([event/0]).

-export_type([party_revision/0]).
-export_type([st/0]).

-spec namespace() -> machinery:namespace().
namespace() ->
    ?NS.

-spec not_implemented(any()) -> no_return().
not_implemented(What) ->
    erlang:error({not_implemented, What}).

-spec init(args([event()]), machine(), handler_args(), handler_opts()) -> result().
init(Events, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        events => [wrap_event_payload(Events)],
        aux_state => wrap_aux_state(#{
            snapshot_index => [],
            party_revision_index => #{}
        })
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) -> no_return().
process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) -> {ok, {repair_response(), result()}}.
process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    {ok, {ok, #{}}}.

-spec process_notification(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_notification(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(notification).

-spec process_call(args(_), machine(), handler_args(), handler_opts()) ->
    {response(_), result()} | no_return().
process_call({{_, Fun}, Args}, Machine, _HandlerArgs, #{woody_ctx := WoodyCtx}) ->
    ContextOptions = #{woody_context => WoodyCtx},
    ok = pm_context:save(pm_context:create(ContextOptions)),
    PartyID = erlang:element(1, Args),
    process_call_(PartyID, Fun, Args, Machine).

process_call_(PartyID, Fun, Args, Machine) ->
    #{id := PartyID, history := History, aux_state := WrappedAuxSt} = Machine,
    try
        scoper:scope(
            party,
            #{
                id => PartyID,
                activity => Fun
            },
            fun() ->
                AuxSt0 = unwrap_aux_state(WrappedAuxSt),
                {St, AuxSt1} = get_state_for_call(PartyID, History, AuxSt0),
                handle_call(Fun, Args, AuxSt1, St)
            end
        )
    catch
        throw:Exception ->
            % error({test, Exception}),
            respond_w_exception(Exception)
    end.

%% Party

handle_call('Block', {_PartyID, Reason}, AuxSt, St) ->
    handle_block(party, Reason, AuxSt, St);
handle_call('Unblock', {_PartyID, Reason}, AuxSt, St) ->
    handle_unblock(party, Reason, AuxSt, St);
handle_call('Suspend', {_PartyID}, AuxSt, St) ->
    handle_suspend(party, AuxSt, St);
handle_call('Activate', {_PartyID}, AuxSt, St) ->
    handle_activate(party, AuxSt, St);
%% Shop

handle_call('BlockShop', {_PartyID, ID, Reason}, AuxSt, St) ->
    handle_block({shop, ID}, Reason, AuxSt, St);
handle_call('UnblockShop', {_PartyID, ID, Reason}, AuxSt, St) ->
    handle_unblock({shop, ID}, Reason, AuxSt, St);
handle_call('SuspendShop', {_PartyID, ID}, AuxSt, St) ->
    handle_suspend({shop, ID}, AuxSt, St);
handle_call('ActivateShop', {_PartyID, ID}, AuxSt, St) ->
    handle_activate({shop, ID}, AuxSt, St);
%% PartyMeta

handle_call('SetMetaData', {_PartyID, NS, Data}, AuxSt, St) ->
    respond(
        ok,
        [?party_meta_set(NS, Data)],
        AuxSt,
        St
    );
handle_call('RemoveMetaData', {_PartyID, NS}, AuxSt, St) ->
    _ = get_st_metadata(NS, St),
    respond(
        ok,
        [?party_meta_removed(NS)],
        AuxSt,
        St
    );
%% Claim

handle_call('CreateClaim', {_PartyID, Changeset}, AuxSt, St) ->
    ok = assert_party_operable(St),
    {Claim, Changes} = create_claim(Changeset, St),
    respond(
        Claim,
        Changes,
        AuxSt,
        St
    );
handle_call('UpdateClaim', {_PartyID, ID, ClaimRevision, Changeset}, AuxSt, St) ->
    ok = assert_party_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    respond(
        ok,
        update_claim(ID, Changeset, St),
        AuxSt,
        St
    );
handle_call('AcceptClaim', {_PartyID, ID, ClaimRevision}, AuxSt, St) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    Claim = pm_claim:accept(
        Timestamp,
        pm_domain:head(),
        get_st_party(St),
        get_st_claim(ID, St)
    ),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );
handle_call('DenyClaim', {_PartyID, ID, ClaimRevision, Reason}, AuxSt, St) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = pm_datetime:format_now(),
    Claim = pm_claim:deny(Reason, Timestamp, get_st_claim(ID, St)),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp)],
        AuxSt,
        St
    );
handle_call('RevokeClaim', {_PartyID, ID, ClaimRevision, Reason}, AuxSt, St) ->
    ok = assert_party_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = pm_datetime:format_now(),
    Claim = pm_claim:revoke(Reason, Timestamp, get_st_claim(ID, St)),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp)],
        AuxSt,
        St
    );
%% ClaimCommitter

handle_call('Accept', {_PartyID, #claimmgmt_Claim{changeset = Changeset}}, AuxSt, St) ->
    Party = get_st_party(St),
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Modifications = pm_claim_committer:filter_party_modifications(Changeset),
    ok = pm_claim_committer:assert_cash_register_modifications_applicable(Modifications, Party),
    ok = pm_claim_committer:assert_modifications_applicable(Modifications, Timestamp, Revision, Party),
    ok = pm_claim_committer:assert_modifications_acceptable(Modifications, Timestamp, Revision, Party),
    respond(ok, [], AuxSt, St);
handle_call('Commit', {_PartyID, Claim}, AuxSt, St) ->
    #claimmgmt_Claim{
        id = ID,
        changeset = Changeset,
        revision = Revision,
        created_at = CreatedAt,
        updated_at = UpdatedAt
    } = Claim,
    Party = get_st_party(St),
    Timestamp = pm_datetime:format_now(),
    DomainRevision = pm_domain:head(),
    Modifications = pm_claim_committer:filter_party_modifications(Changeset),
    ok = pm_claim_committer:assert_modifications_acceptable(Modifications, Timestamp, DomainRevision, Party),
    Effects = pm_claim_committer_effect:make_modifications_effects(Modifications, Timestamp, DomainRevision),
    PartyClaim = pm_claim_committer_converter:new_party_claim(ID, Revision, CreatedAt, UpdatedAt),
    AcceptedPartyClaim = set_status(?accepted(Effects), get_next_revision(PartyClaim), Timestamp, PartyClaim),
    PartyRevision = get_next_party_revision(St),
    respond(
        ok,
        [
            ?claim_created(PartyClaim),
            finalize_claim(AcceptedPartyClaim, Timestamp),
            ?revision_changed(Timestamp, PartyRevision)
        ],
        AuxSt,
        St
    ).

get_next_revision(#payproc_Claim{revision = ClaimRevision}) ->
    ClaimRevision + 1.

set_status(Status, NewRevision, Timestamp, Claim) ->
    Claim#payproc_Claim{
        revision = NewRevision,
        updated_at = Timestamp,
        status = Status
    }.

%% Generic handlers

-spec handle_block(call_target(), binary(), party_aux_st(), st()) -> {response(ok), result()}.
handle_block(Target, Reason, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [block(Target, Reason, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

-spec handle_unblock(call_target(), binary(), party_aux_st(), st()) -> {response(ok), result()}.
handle_unblock(Target, Reason, AuxSt, St) ->
    ok = assert_blocked(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [unblock(Target, Reason, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

-spec handle_suspend(call_target(), party_aux_st(), st()) -> {response(ok), result()}.
handle_suspend(Target, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    ok = assert_active(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [suspend(Target, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

-spec handle_activate(call_target(), party_aux_st(), st()) -> {response(ok), result()}.
handle_activate(Target, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    ok = assert_suspended(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [activate(Target, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

publish_party_event(Source, {ID, Dt, {Changes, _}}) ->
    #payproc_Event{id = ID, source = Source, created_at = Dt, payload = ?party_ev(Changes)}.

%%
-spec get_backend(woody_context()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    get_backend(genlib_app:env(party_management, machinery_backend), WoodyCtx).

%%% Internal functions

get_backend(hybrid, WoodyCtx) ->
    {machinery_hybrid_backend, #{
        primary_backend => get_backend(progressor, WoodyCtx),
        fallback_backend => get_backend(machinegun, WoodyCtx)
    }};
get_backend(progressor, WoodyCtx) ->
    machinery_prg_backend:new(WoodyCtx, #{
        namespace => ?NS,
        handler => pm_party_machine,
        schema => party_management_machinery_schema
    });
get_backend(machinegun, WoodyCtx) ->
    Backend = maps:get(?NS, genlib_app:env(party_management, backends, #{})),
    {Mod, Opts} = machinery_utils:get_backend(Backend),
    {Mod, Opts#{
        woody_ctx => WoodyCtx
    }}.

-spec start(party_id(), Args :: term(), woody_context()) -> ok | no_return().
start(PartyID, PartyParams, WoodyCtx) ->
    #payproc_PartyParams{contact_info = ContactInfo} = PartyParams,
    Timestamp = pm_datetime:format_now(),
    Changes = [?party_created(PartyID, ContactInfo, Timestamp), ?revision_changed(Timestamp, 0)],
    case machinery:start(?NS, PartyID, Changes, get_backend(WoodyCtx)) of
        ok ->
            ok;
        {error, exists} ->
            throw(#payproc_PartyExists{})
    end.

-spec get_party(party_id()) -> dmsl_domain_thrift:'Party'() | no_return().
get_party(PartyID) ->
    get_st_party(get_state(PartyID)).

get_state(PartyID) ->
    AuxSt = get_aux_state(PartyID),
    get_state(PartyID, get_snapshot_index(AuxSt)).

get_state(PartyID, []) ->
    %% No snapshots, so we need entire history
    Events = unwrap_events(get_history(PartyID, undefined, undefined, forward)),
    merge_events(Events, #state_State{});
get_state(PartyID, [FirstID | _]) ->
    History = get_history(PartyID, FirstID - 1, undefined, forward),
    Events = [FirstEvent | _] = unwrap_events(History),
    St = unwrap_state(FirstEvent),
    merge_events(Events, St).

get_state_for_call(PartyID, ReversedHistoryPart, AuxSt) ->
    {St, History} = parse_history(ReversedHistoryPart),
    get_state_for_call(PartyID, {St, History}, [], AuxSt).

get_state_for_call(PartyID, {undefined, [{FirstID, _, _} | _] = Events}, EventsAcc, AuxSt) when FirstID > 1 ->
    Limit = get_limit(FirstID, get_snapshot_index(AuxSt)),
    NewHistoryPart = parse_history(get_history(PartyID, FirstID, Limit, backward)),
    get_state_for_call(PartyID, NewHistoryPart, Events ++ EventsAcc, AuxSt);
get_state_for_call(_, {St0, Events}, EventsAcc, AuxSt0) ->
    %% here we can get entire history.
    %% we can use it to create revision index for AuxSt
    PartyRevisionIndex0 = get_party_revision_index(AuxSt0),
    {St1, PartyRevisionIndex1} = build_revision_index(
        Events ++ EventsAcc,
        PartyRevisionIndex0,
        pm_utils:select_defined(St0, #state_State{})
    ),
    AuxSt1 = set_party_revision_index(PartyRevisionIndex1, AuxSt0),
    {St1, AuxSt1}.

parse_history(ReversedHistoryPart) ->
    parse_history(ReversedHistoryPart, []).

parse_history([WrappedEvent | Others], EventsAcc) ->
    Event = unwrap_event(WrappedEvent),
    case unwrap_state(Event) of
        undefined ->
            parse_history(Others, [Event | EventsAcc]);
        #state_State{} = St ->
            {St, [Event | EventsAcc]}
    end;
parse_history([], EventsAcc) ->
    {undefined, EventsAcc}.

-spec checkout(party_id(), party_revision_param()) -> dmsl_domain_thrift:'Party'() | no_return().
checkout(PartyID, RevisionParam) ->
    get_st_party(
        pm_utils:unwrap_result(
            checkout_party(PartyID, RevisionParam)
        )
    ).

-spec get_last_revision(party_id()) -> party_revision() | no_return().
get_last_revision(PartyID) ->
    AuxState = get_aux_state(PartyID),
    LastEventID = maps:get(last_event_id, AuxState),
    case get_party_revision_index(AuxState) of
        RevisionIndex when map_size(RevisionIndex) > 0 ->
            MaxRevision = lists:max(maps:keys(RevisionIndex)),
            % we should check if this is the last revision for real
            {_, ToEventID} = get_party_revision_range(MaxRevision, RevisionIndex),
            case ToEventID < LastEventID of
                true ->
                    % there are events after MaxRevision, so it can be a bug
                    _ = logger:warning(
                        "Max revision EventID (~p) and LastEventID (~p) missmatch",
                        [ToEventID, LastEventID]
                    ),
                    get_last_revision_old_way(PartyID);
                false ->
                    MaxRevision
            end;
        _ ->
            get_last_revision_old_way(PartyID)
    end.

-spec get_last_revision_old_way(party_id()) -> party_revision() | no_return().
get_last_revision_old_way(PartyID) ->
    get_revision_of_part(PartyID, undefined, ?STEP).

-spec get_status(party_id()) -> party_status() | no_return().
get_status(PartyID) ->
    pm_party:get_status(
        get_party(PartyID)
    ).

-spec call(party_id(), service_name(), pm_proto_utils:thrift_fun_ref(), woody:args()) -> term() | no_return().
call(PartyID, _ServiceName, FucntionRef, Args) ->
    WoodyCtx = pm_context:get_woody_context(pm_context:load()),
    Result = machinery:call(
        ?NS,
        PartyID,
        {undefined, ?SNAPSHOT_STEP, backward},
        {FucntionRef, Args},
        get_backend(WoodyCtx)
    ),
    map_error(
        Result
    ).

map_error({ok, {exception, Reason}}) ->
    throw(Reason);
map_error({ok, CallResult}) ->
    CallResult;
map_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{}).

-spec get_claim(claim_id(), party_id()) -> claim() | no_return().
get_claim(ID, PartyID) ->
    get_st_claim(ID, get_state(PartyID)).

-spec get_claims(party_id()) -> [claim()] | no_return().
get_claims(PartyID) ->
    #state_State{claims = Claims} = get_state(PartyID),
    maps:values(Claims).

-spec get_meta(party_id()) -> meta() | no_return().
get_meta(PartyID) ->
    #state_State{meta = Meta} = get_state(PartyID),
    Meta.

-spec get_metadata(meta_ns(), party_id()) -> meta_data() | no_return().
get_metadata(NS, PartyID) ->
    get_st_metadata(NS, get_state(PartyID)).

-spec get_public_history(party_id(), integer() | undefined, non_neg_integer()) ->
    [dmsl_payproc_thrift:'Event'()].
get_public_history(PartyID, AfterID, Limit) ->
    Events = unwrap_events(get_history(PartyID, AfterID, Limit)),
    [publish_party_event({party_id, PartyID}, Ev) || Ev <- Events].

get_history(PartyID, AfterID, Limit) ->
    get_history(PartyID, AfterID, Limit, forward).

get_history(PartyID, AfterID, Limit, Direction) ->
    WoodyCtx = pm_context:get_woody_context(pm_context:load()),
    #{history := History} = map_history_error(
        machinery:get(?NS, PartyID, {AfterID, Limit, Direction}, get_backend(WoodyCtx))
    ),
    History.

-spec get_aux_state(party_id()) -> party_aux_st().
get_aux_state(PartyID) ->
    WoodyCtx = pm_context:get_woody_context(pm_context:load()),
    State =
        #{history := History} = map_history_error(
            machinery:get(
                ?NS,
                PartyID,
                {undefined, 1, backward},
                get_backend(WoodyCtx)
            )
        ),
    AuxState = unwrap_aux_state(maps:get(aux_state, State, undefined)),
    case History of
        [] ->
            AuxState#{last_event_id => 0};
        [{EventID, _, _}] ->
            AuxState#{last_event_id => EventID}
    end.

get_revision_of_part(PartyID, Last, Step) ->
    {History, LastNext, StepNext} = get_history_part(PartyID, Last, Step),
    case find_revision_in_history(History) of
        revision_not_found when LastNext == 0 ->
            0;
        revision_not_found ->
            get_revision_of_part(PartyID, LastNext, StepNext);
        Revision ->
            Revision
    end.

get_history_part(PartyID, Last, Step) ->
    case unwrap_events(get_history(PartyID, Last, Step, backward)) of
        [] ->
            {[], 0, 0};
        History ->
            {LastID, _, _} = lists:last(History),
            {History, LastID, Step * 2}
    end.

find_revision_in_history([]) ->
    revision_not_found;
find_revision_in_history([{_, _, {PartyChanges, _}} | Rest]) when is_list(PartyChanges) ->
    case find_revision_in_changes(PartyChanges) of
        revision_not_found ->
            find_revision_in_history(Rest);
        Revision ->
            Revision
    end.

find_revision_in_changes([]) ->
    revision_not_found;
find_revision_in_changes([Event | Rest]) ->
    case Event of
        ?revision_changed(_, Revision) when Revision =/= undefined ->
            Revision;
        _ ->
            find_revision_in_changes(Rest)
    end.

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{}).

%%

get_st_party(#state_State{party = Party}) ->
    Party.

get_next_party_revision(#state_State{party = Party}) ->
    Party#domain_Party.revision + 1.

get_st_claim(ID, #state_State{claims = Claims}) ->
    assert_claim_exists(maps:get(ID, Claims, undefined)).

get_st_pending_claims(#state_State{claims = Claims}) ->
    % TODO cache it during history collapse
    % Looks like little overhead, compared to previous version (based on maps:fold),
    % but I hope for small amount of pending claims simultaniously.
    maps:values(
        maps:filter(
            fun(_ID, Claim) ->
                pm_claim:is_pending(Claim)
            end,
            Claims
        )
    ).

-spec get_st_metadata(meta_ns(), st()) -> meta_data().
get_st_metadata(NS, #state_State{meta = Meta}) ->
    case maps:get(NS, Meta, undefined) of
        MetaData when MetaData =/= undefined ->
            MetaData;
        undefined ->
            throw(#payproc_PartyMetaNamespaceNotFound{})
    end.

set_claim(
    #payproc_Claim{id = ID} = Claim,
    #state_State{claims = Claims} = St
) ->
    St#state_State{claims = Claims#{ID => Claim}}.

assert_claim_exists(Claim = #payproc_Claim{}) ->
    Claim;
assert_claim_exists(undefined) ->
    throw(#payproc_ClaimNotFound{}).

assert_claim_modification_allowed(ID, Revision, St) ->
    Claim = get_st_claim(ID, St),
    ok = pm_claim:assert_revision(Claim, Revision),
    ok = pm_claim:assert_pending(Claim).

assert_claims_not_conflict(Claim, ClaimsPending, Timestamp, Revision, Party) ->
    ConflictedClaims = lists:dropwhile(
        fun(PendingClaim) ->
            pm_claim:get_id(Claim) =:= pm_claim:get_id(PendingClaim) orelse
                not pm_claim:is_conflicting(Claim, PendingClaim, Timestamp, Revision, Party)
        end,
        ClaimsPending
    ),
    case ConflictedClaims of
        [] ->
            ok;
        [#payproc_Claim{id = ID} | _] ->
            throw(#payproc_ChangesetConflict{conflicted_id = ID})
    end.

%%

create_claim(Changeset, St) ->
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Party = get_st_party(St),
    Claim = pm_claim:create(get_next_claim_id(St), Changeset, Party, Timestamp, Revision),
    ClaimsPending = get_st_pending_claims(St),
    % Check for conflicts with other pending claims
    ok = assert_claims_not_conflict(Claim, ClaimsPending, Timestamp, Revision, Party),
    % Test if we can safely accept proposed changes.
    case pm_claim:is_need_acceptance(Claim, Party, Revision) of
        false ->
            % Try to submit new accepted claim
            try
                AcceptedClaim = pm_claim:accept(Timestamp, Revision, Party, Claim),
                PartyRevision = get_next_party_revision(St),
                {
                    AcceptedClaim,
                    [
                        ?claim_created(Claim),
                        finalize_claim(AcceptedClaim, Timestamp),
                        ?revision_changed(Timestamp, PartyRevision)
                    ]
                }
            catch
                throw:_AnyException ->
                    {Claim, [?claim_created(Claim)]}
            end;
        true ->
            % Submit new pending claim
            {Claim, [?claim_created(Claim)]}
    end.

update_claim(ID, Changeset, St) ->
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Party = get_st_party(St),
    Claim = pm_claim:update(
        Changeset,
        get_st_claim(ID, St),
        Party,
        Timestamp,
        Revision
    ),
    ClaimsPending = get_st_pending_claims(St),
    ok = assert_claims_not_conflict(Claim, ClaimsPending, Timestamp, Revision, Party),
    [?claim_updated(ID, Changeset, pm_claim:get_revision(Claim), Timestamp)].

finalize_claim(Claim, Timestamp) ->
    ?claim_status_changed(
        pm_claim:get_id(Claim),
        pm_claim:get_status(Claim),
        pm_claim:get_revision(Claim),
        Timestamp
    ).

get_next_claim_id(#state_State{claims = Claims}) ->
    % TODO cache sequences on history collapse
    lists:max([0 | maps:keys(Claims)]) + 1.

apply_accepted_claim(Claim, St) ->
    case pm_claim:is_accepted(Claim) of
        true ->
            Party = pm_claim:apply(Claim, pm_datetime:format_now(), get_st_party(St)),
            St#state_State{party = Party};
        false ->
            St
    end.

respond(ok, Changes, AuxSt, St) ->
    do_respond(ok, Changes, AuxSt, St);
respond(Response, Changes, AuxSt, St) ->
    do_respond(Response, Changes, AuxSt, St).

do_respond(Response, Changes, AuxSt0, St) ->
    AuxSt1 = append_party_revision_index(Changes, St, AuxSt0),
    {Events, AuxSt2} = try_attach_snapshot(Changes, AuxSt1, St),
    {
        Response,
        #{
            events => Events,
            aux_state => AuxSt2
        }
    }.

respond_w_exception(Exception) ->
    {{exception, Exception}, #{}}.

append_party_revision_index(Changes, St0, AuxSt) ->
    PartyRevisionIndex0 = get_party_revision_index(AuxSt),
    LastEventID = St0#state_State.last_event,
    % Brave prediction of next EventID ))
    St1 = merge_party_changes(Changes, St0#state_State{last_event = LastEventID + 1}),
    PartyRevisionIndex1 = update_party_revision_index(St1, PartyRevisionIndex0),
    set_party_revision_index(PartyRevisionIndex1, AuxSt).

update_party_revision_index(St, PartyRevisionIndex) ->
    #domain_Party{revision = PartyRevision} = get_st_party(St),
    EventID = St#state_State.last_event,
    {FromEventID, ToEventID} = get_party_revision_range(PartyRevision, PartyRevisionIndex),
    PartyRevisionIndex#{
        PartyRevision => {
            pm_utils:select_defined(FromEventID, EventID),
            max(pm_utils:select_defined(ToEventID, EventID), EventID)
        }
    }.

get_party_revision_index(AuxSt) ->
    maps:get(party_revision_index, AuxSt, #{}).

set_party_revision_index(PartyRevisionIndex, AuxSt) ->
    AuxSt#{party_revision_index => PartyRevisionIndex}.

get_party_revision_range(PartyRevision, PartyRevisionIndex) ->
    maps:get(PartyRevision, PartyRevisionIndex, {undefined, undefined}).

%% TODO crunch func, will be removed after a short (or not so short) time
build_revision_index([Event | History], PartyRevisionIndex0, St0) ->
    St1 = merge_event(Event, St0),
    PartyRevisionIndex1 = update_party_revision_index(St1, PartyRevisionIndex0),
    build_revision_index(History, PartyRevisionIndex1, St1);
build_revision_index([], PartyRevisionIndex, St) ->
    {St, PartyRevisionIndex}.

append_snapshot_index(EventID, AuxSt) ->
    SnapshotIndex = get_snapshot_index(AuxSt),
    set_snapshot_index([EventID | SnapshotIndex], AuxSt).

get_snapshot_index(AuxSt) ->
    maps:get(snapshot_index, AuxSt, []).

set_snapshot_index(SnapshotIndex, AuxSt) ->
    AuxSt#{snapshot_index => SnapshotIndex}.

get_limit(undefined, _) ->
    %% we can't get any reasonable limit in this case
    undefined;
get_limit(ToEventID, [SnapshotEventID | _]) when SnapshotEventID < ToEventID ->
    ToEventID - SnapshotEventID;
get_limit(ToEventID, [_ | SnapshotIndex]) ->
    get_limit(ToEventID, SnapshotIndex);
get_limit(_ToEventID, []) ->
    undefined.

%%

-spec checkout_party(party_id(), party_revision_param()) -> {ok, st()} | {error, revision_not_found}.
checkout_party(PartyID, {timestamp, Timestamp}) ->
    Events = unwrap_events(get_history(PartyID, undefined, undefined)),
    checkout_history_by_timestamp(Events, Timestamp, #state_State{});
checkout_party(PartyID, {revision, Revision}) ->
    checkout_cached_party_by_revision(PartyID, Revision).

checkout_history_by_timestamp([Ev | Rest], Timestamp, #state_State{timestamp = PrevTimestamp} = St) ->
    St1 = merge_event(Ev, St),
    EventTimestamp = St1#state_State.timestamp,
    case pm_datetime:compare(EventTimestamp, Timestamp) of
        later when PrevTimestamp =/= undefined ->
            {ok, St#state_State{timestamp = Timestamp}};
        later when PrevTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            checkout_history_by_timestamp(Rest, Timestamp, St1)
    end;
checkout_history_by_timestamp([], Timestamp, St) ->
    {ok, St#state_State{timestamp = Timestamp}}.

checkout_cached_party_by_revision(PartyID, Revision) ->
    case pm_party_cache:get_party(PartyID, Revision) of
        {ok, Party} ->
            _ = logger:info("PartyID: ~p Revision: ~p cache hit", [PartyID, Revision]),
            {ok, Party};
        not_found ->
            case checkout_party_by_revision(PartyID, Revision) of
                {ok, Party} = Res ->
                    _ = logger:info("PartyID: ~p Revision: ~p cache miss", [PartyID, Revision]),
                    ok = pm_party_cache:update_party(PartyID, Revision, Party),
                    Res;
                OtherRes ->
                    OtherRes
            end
    end.

checkout_party_by_revision(PartyID, Revision) ->
    AuxSt = get_aux_state(PartyID),
    FromEventID =
        case get_party_revision_range(Revision, get_party_revision_index(AuxSt)) of
            {_, undefined} ->
                undefined;
            {_, EventID} ->
                EventID + 1
        end,
    Limit = get_limit(FromEventID, get_snapshot_index(AuxSt)),
    ReversedHistory = get_history(PartyID, FromEventID, Limit, backward),
    case parse_history(ReversedHistory) of
        {undefined, Events} ->
            checkout_history_by_revision(Events, Revision, #state_State{});
        {St, Events} ->
            checkout_history_by_revision(Events, Revision, St)
    end.

checkout_history_by_revision([Ev | Rest], Revision, St) ->
    St1 = merge_event(Ev, St),
    case get_st_party(St1) of
        #domain_Party{revision = Revision1} when Revision1 > Revision ->
            {ok, St};
        _ ->
            checkout_history_by_revision(Rest, Revision, St1)
    end;
checkout_history_by_revision([], Revision, St) ->
    case get_st_party(St) of
        #domain_Party{revision = Revision} ->
            {ok, St};
        _ ->
            {error, revision_not_found}
    end.

merge_events(Events, St) ->
    lists:foldl(fun merge_event/2, St, Events).

merge_event({ID, _Dt, {PartyChanges, _}}, #state_State{last_event = LastEventID} = St) when
    is_list(PartyChanges) andalso ID =:= LastEventID + 1
->
    merge_party_changes(PartyChanges, St#state_State{last_event = ID}).

merge_party_changes(Changes, St) ->
    lists:foldl(fun merge_party_change/2, St, Changes).

merge_party_change(?party_created(PartyID, ContactInfo, Timestamp), St) ->
    St#state_State{
        timestamp = Timestamp,
        party = pm_party:create_party(PartyID, ContactInfo, Timestamp)
    };
merge_party_change(?party_blocking(Blocking), St) ->
    Party = get_st_party(St),
    St#state_State{party = pm_party:blocking(Blocking, Party)};
merge_party_change(?revision_changed(Timestamp, Revision), St) ->
    Party = get_st_party(St),
    St#state_State{
        timestamp = Timestamp,
        party = Party#domain_Party{revision = Revision}
    };
merge_party_change(?party_suspension(Suspension), St) ->
    Party = get_st_party(St),
    St#state_State{party = pm_party:suspension(Suspension, Party)};
merge_party_change(?party_meta_set(NS, Data), #state_State{meta = Meta} = St) ->
    NewMeta = Meta#{NS => Data},
    St#state_State{meta = NewMeta};
merge_party_change(?party_meta_removed(NS), #state_State{meta = Meta} = St) ->
    NewMeta = maps:remove(NS, Meta),
    St#state_State{meta = NewMeta};
merge_party_change(?shop_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#state_State{party = pm_party:shop_blocking(ID, Blocking, Party)};
merge_party_change(?shop_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#state_State{party = pm_party:shop_suspension(ID, Suspension, Party)};
merge_party_change(?wallet_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#state_State{party = pm_party:wallet_blocking(ID, Blocking, Party)};
merge_party_change(?wallet_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#state_State{party = pm_party:wallet_suspension(ID, Suspension, Party)};
merge_party_change(?claim_created(Claim0), St) ->
    Claim = ensure_claim(Claim0),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_change(?claim_updated(ID, Changeset, Revision, UpdatedAt), St) ->
    Claim0 = pm_claim:update_changeset(Changeset, Revision, UpdatedAt, get_st_claim(ID, St)),
    Claim = ensure_claim(Claim0),
    set_claim(Claim, St);
merge_party_change(?claim_status_changed(ID, Status, Revision, UpdatedAt), St) ->
    Claim0 = pm_claim:set_status(Status, Revision, UpdatedAt, get_st_claim(ID, St)),
    Claim = ensure_claim(Claim0),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1).

block(party, Reason, Timestamp) ->
    ?party_blocking(?blocked(Reason, Timestamp));
block({shop, ID}, Reason, Timestamp) ->
    ?shop_blocking(ID, ?blocked(Reason, Timestamp)).

unblock(party, Reason, Timestamp) ->
    ?party_blocking(?unblocked(Reason, Timestamp));
unblock({shop, ID}, Reason, Timestamp) ->
    ?shop_blocking(ID, ?unblocked(Reason, Timestamp)).

suspend(party, Timestamp) ->
    ?party_suspension(?suspended(Timestamp));
suspend({shop, ID}, Timestamp) ->
    ?shop_suspension(ID, ?suspended(Timestamp)).

activate(party, Timestamp) ->
    ?party_suspension(?active(Timestamp));
activate({shop, ID}, Timestamp) ->
    ?shop_suspension(ID, ?active(Timestamp)).

assert_party_operable(St) ->
    _ = assert_unblocked(party, St),
    _ = assert_active(party, St).

assert_unblocked(party, St) ->
    assert_blocking(get_st_party(St), unblocked);
assert_unblocked({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_blocking(Shop, unblocked).

assert_blocked(party, St) ->
    assert_blocking(get_st_party(St), blocked);
assert_blocked({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_blocking(Shop, blocked).

assert_blocking(#domain_Party{blocking = {Status, _}}, Status) ->
    ok;
assert_blocking(#domain_Party{blocking = Blocking}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {blocking, Blocking}}).

assert_active(party, St) ->
    assert_suspension(get_st_party(St), active);
assert_active({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_suspension(Shop, active).

assert_suspended(party, St) ->
    assert_suspension(get_st_party(St), suspended);
assert_suspended({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_suspension(Shop, suspended).

assert_suspension(#domain_Party{suspension = {Status, _}}, Status) ->
    ok;
assert_suspension(#domain_Party{suspension = Suspension}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {suspension, Suspension}}).

assert_shop_found(#domain_Shop{} = Shop) ->
    Shop;
assert_shop_found(undefined) ->
    throw(#payproc_ShopNotFound{}).

assert_shop_blocking(#domain_Shop{blocking = {Status, _}}, Status) ->
    ok;
assert_shop_blocking(#domain_Shop{blocking = Blocking}, _) ->
    throw(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    throw(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).

%% backward compatibility stuff
%% TODO remove after migration

ensure_claim(
    #payproc_Claim{
        created_at = Timestamp,
        changeset = Changeset0,
        status = Status0
    } = Claim
) ->
    Changeset = ensure_claim_changeset(Changeset0, Timestamp),
    Status = ensure_claim_status(Status0, Timestamp),
    Claim#payproc_Claim{
        changeset = Changeset,
        status = Status
    }.

ensure_claim_changeset(undefined, _) ->
    undefined;
ensure_claim_changeset(Changeset, Timestamp) ->
    [ensure_contract_change(C, Timestamp) || C <- Changeset].

ensure_contract_change(?contract_modification(ID, {creation, ContractParams}), Timestamp) ->
    ?contract_modification(
        ID,
        {creation, ensure_payment_institution(ContractParams, Timestamp)}
    );
ensure_contract_change(C, _) ->
    C.

ensure_claim_status({accepted, #payproc_ClaimAccepted{effects = Effects} = S}, Timestamp) ->
    {accepted, S#payproc_ClaimAccepted{
        effects = [ensure_contract_effect(E, Timestamp) || E <- Effects]
    }};
ensure_claim_status(S, _) ->
    S.

ensure_contract_effect(?contract_effect(ID, {created, Contract}), Timestamp) ->
    ?contract_effect(ID, {created, ensure_payment_institution(Contract, Timestamp)});
ensure_contract_effect(E, _) ->
    E.

ensure_payment_institution(#domain_Contract{payment_institution = undefined} = Contract, Timestamp) ->
    Revision = pm_domain:head(),
    PaymentInstitutionRef = get_default_payment_institution(
        get_realm(Contract, Timestamp, Revision),
        Revision
    ),
    Contract#domain_Contract{payment_institution = PaymentInstitutionRef};
ensure_payment_institution(#domain_Contract{} = Contract, _) ->
    Contract;
ensure_payment_institution(
    #payproc_ContractParams{
        template = TemplateRef,
        payment_institution = undefined
    } = ContractParams,
    Timestamp
) ->
    Revision = pm_domain:head(),
    Realm =
        case TemplateRef of
            undefined ->
                % use default live payment institution
                live;
            _ ->
                Template = get_template(TemplateRef, Revision),
                get_realm(Template, Timestamp, Revision)
        end,
    ContractParams#payproc_ContractParams{
        payment_institution = get_default_payment_institution(Realm, Revision)
    };
ensure_payment_institution(#payproc_ContractParams{} = ContractParams, _) ->
    ContractParams.

get_realm(C, Timestamp, Revision) ->
    Categories = pm_contract:get_categories(C, Timestamp, Revision),
    {Test, Live} = lists:foldl(
        fun(CategoryRef, {TestFound, LiveFound}) ->
            case pm_domain:get(Revision, {category, CategoryRef}) of
                #domain_Category{type = test} ->
                    {true, LiveFound};
                #domain_Category{type = live} ->
                    {TestFound, true}
            end
        end,
        {false, false},
        ordsets:to_list(Categories)
    ),
    case Test /= Live of
        true when Test =:= true ->
            test;
        true when Live =:= true ->
            live;
        false ->
            error({
                misconfiguration,
                {'Test and live category in same term set', C, Timestamp, Revision}
            })
    end.

get_default_payment_institution(Realm, Revision) ->
    Globals = pm_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    Defaults = Globals#domain_Globals.contract_payment_institution_defaults,
    case Realm of
        test ->
            Defaults#domain_ContractPaymentInstitutionDefaults.test;
        live ->
            Defaults#domain_ContractPaymentInstitutionDefaults.live
    end.

get_template(TemplateRef, Revision) ->
    pm_domain:get(Revision, {contract_template, TemplateRef}).

%%

try_attach_snapshot(Changes, AuxSt0, #state_State{last_event = LastEventID} = St) when
    LastEventID > 0 andalso
        LastEventID rem ?SNAPSHOT_STEP =:= 0
->
    AuxSt1 = append_snapshot_index(LastEventID + 1, AuxSt0),
    {
        [wrap_event_payload_w_snapshot(Changes, St)],
        wrap_aux_state(AuxSt1)
    };
try_attach_snapshot(Changes, AuxSt, _) ->
    {
        [wrap_event_payload(Changes)],
        wrap_aux_state(AuxSt)
    }.

-define(FORMAT_VERSION_THRIFT, 2).

wrap_event_payload(Changes) ->
    marshal_event_payload(?FORMAT_VERSION_THRIFT, Changes, undefined).

wrap_event_payload_w_snapshot(Changes, St) ->
    {FormatVsn, StateSnapshot} = encode_state(St),
    marshal_event_payload(FormatVsn, Changes, StateSnapshot).

marshal_event_payload(FormatVsn, Changes, StateSnapshot) ->
    Type = {struct, struct, {dmsl_payproc_thrift, 'PartyEventData'}},
    Bin = pm_proto_utils:serialize(Type, #payproc_PartyEventData{changes = Changes, state_snapshot = StateSnapshot}),
    #{
        format_version => FormatVsn,
        data => {bin, Bin}
    }.

unwrap_events(History) ->
    [unwrap_event(E) || E <- History].

unwrap_event({ID, Dt, Event}) ->
    {ID, machinery_mg_codec:marshal(timestamp, Dt), unwrap_event_payload(Event)}.

unwrap_event_payload(#{format_version := Format, data := Data}) ->
    unwrap_event_payload(Format, Data).

unwrap_event_payload(
    FormatVsn,
    {bin, ThriftEncodedBin}
) when is_integer(FormatVsn) ->
    Type = {struct, struct, {dmsl_payproc_thrift, 'PartyEventData'}},
    ?party_event_data(Changes, Snapshot) = pm_proto_utils:deserialize(Type, ThriftEncodedBin),
    {Changes, pm_maybe:apply(fun(S) -> {FormatVsn, S} end, Snapshot)}.

unwrap_state({_ID, _Dt, {_Changes, {FormatVsn, EncodedSt}}}) ->
    decode_state_format(FormatVsn, EncodedSt);
unwrap_state({_ID, _Dt, {_Changes, undefined}}) ->
    undefined.

-define(STATE_THRIFT_TYPE, {struct, struct, {pm_state_thrift, 'State'}}).

encode_state(St) ->
    {?FORMAT_VERSION_THRIFT, {bin, pm_proto_utils:serialize(?STATE_THRIFT_TYPE, St)}}.

decode_state_format(?FORMAT_VERSION_THRIFT, {bin, EncodedSt}) ->
    pm_proto_utils:deserialize(?STATE_THRIFT_TYPE, EncodedSt).

-spec wrap_aux_state(party_aux_st()) -> pm_msgpack_marshalling:msgpack_value().
wrap_aux_state(AuxSt) ->
    ContentType = ?CT_ERLANG_BINARY,
    #{<<"ct">> => ContentType, <<"aux_state">> => encode_aux_state(ContentType, AuxSt)}.

-spec unwrap_aux_state(pm_msgpack_marshalling:msgpack_value()) -> party_aux_st().
unwrap_aux_state(#{<<"ct">> := ContentType, <<"aux_state">> := AuxSt}) ->
    decode_aux_state(ContentType, AuxSt);
%% backward compatibility
unwrap_aux_state(undefined) ->
    #{}.

-spec encode_aux_state(content_type(), party_aux_st()) -> dmsl_msgpack_thrift:'Value'().
encode_aux_state(?CT_ERLANG_BINARY, AuxSt) ->
    {bin, term_to_binary(AuxSt)}.

-spec decode_aux_state(content_type(), dmsl_msgpack_thrift:'Value'()) -> party_aux_st().
decode_aux_state(?CT_ERLANG_BINARY, {bin, AuxSt}) ->
    binary_to_term(AuxSt).
