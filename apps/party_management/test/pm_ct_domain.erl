-module(pm_ct_domain).

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

-export([upsert/2]).
-export([reset/1]).
-export([commit/2]).

-export([with/2]).

%%

-type revision() :: pm_domain:revision().
-type object() :: pm_domain:object().

-spec upsert(revision(), object() | [object()]) -> revision() | no_return().
upsert(Revision, NewObject) when not is_list(NewObject) ->
    upsert(Revision, [NewObject]);
upsert(Revision, NewObjects) ->
    Commit = #'domain_conf_Commit'{
        ops = lists:foldl(
            fun(NewObject = {Tag, {ObjectName, Ref, NewData}}, Ops) ->
                case pm_domain:find(Revision, {Tag, Ref}) of
                    NewData ->
                        Ops;
                    notfound ->
                        [
                            {insert, #'domain_conf_InsertOp'{
                                object = NewObject
                            }}
                            | Ops
                        ];
                    OldData ->
                        [
                            {update, #'domain_conf_UpdateOp'{
                                old_object = {Tag, {ObjectName, Ref, OldData}},
                                new_object = NewObject
                            }}
                            | Ops
                        ]
                end
            end,
            [],
            NewObjects
        )
    },
    ok = commit(Revision, Commit),
    pm_domain:head().

-spec reset(revision()) -> revision() | no_return().
reset(ToRevision) ->
    #'domain_conf_Snapshot'{domain = Domain} = dmt_client:checkout(ToRevision),
    upsert(pm_domain:head(), maps:values(Domain)).

-spec commit(revision(), dmt_client:commit()) -> ok | no_return().
commit(Revision, Commit) ->
    Revision = dmt_client:commit(Revision, Commit) - 1,
    ok.

-spec with(object() | [object()], fun((revision()) -> R)) -> R | no_return().
with(NewObjects, Fun) ->
    WasRevision = pm_domain:head(),
    Revision = upsert(WasRevision, NewObjects),
    try
        Fun(Revision)
    after
        reset(WasRevision)
    end.
