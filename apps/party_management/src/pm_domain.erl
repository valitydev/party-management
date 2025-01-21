%%% Domain config interfaces
%%%
%%% TODO
%%%  - Use proper reflection instead of blind pattern matching when (un)wrapping
%%%    domain objects

-module(pm_domain).

-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

%%

-export([head/0]).
-export([get/2]).
-export([find/2]).
-export([exists/2]).

-export([insert/1]).
-export([update/1]).
-export([cleanup/1]).

%%

-type revision() :: pos_integer().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type object() :: dmsl_domain_thrift:'DomainObject'().
-type data() :: _.
-type commit_response() :: dmsl_domain_conf_v2_thrift:'CommitResponse'().

-export_type([revision/0]).
-export_type([ref/0]).
-export_type([object/0]).
-export_type([data/0]).

-spec head() -> revision().
head() ->
    dmt_client:get_last_version().

-spec get(revision(), ref()) -> data() | no_return().
get(Revision0, Ref) ->
    Revision1 = maybe_migrate_version(Revision0),
    try
        extract_data(dmt_client:checkout_object(Ref, Revision1))
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            error({object_not_found, {Revision1, Ref}})
    end.

-spec find(revision(), ref()) -> data() | notfound.
find(Revision0, Ref) ->
    Revision1 = maybe_migrate_version(Revision0),
    try
        extract_data(dmt_client:checkout_object(Ref, Revision1))
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            notfound
    end.

-spec exists(revision(), ref()) -> boolean().
exists(Revision0, Ref) ->
    Revision1 = maybe_migrate_version(Revision0),
    try
        _ = dmt_client:checkout_object(Ref, Revision1),
        true
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            false
    end.

extract_data(#domain_conf_v2_VersionedObject{object = {_Tag, {_Name, _Ref, Data}}}) ->
    Data.

-spec commit(revision(), dmt_client:commit(), binary()) -> commit_response() | no_return().
commit(Revision, Commit, AuthorID) ->
    dmt_client:commit(Revision, Commit, AuthorID).

-spec insert(object() | [object()]) -> {revision(), [ref()]} | no_return().
insert(Objects) ->
    insert(Objects, generate_author()).

-spec insert(object() | [object()], binary()) -> {revision(), [ref()]} | no_return().
insert(Object, AuthorID) when not is_list(Object) ->
    insert([Object], AuthorID);
insert(Objects, AuthorID) ->
    Commit = #domain_conf_v2_Commit{
        ops = [
            {insert, #domain_conf_v2_InsertOp{
                object = {Type, Object},
                force_ref = {Type, ForceRef}
            }}
         || {Type, {_ObjectName, ForceRef, Object}} <- Objects
        ]
    },
    #domain_conf_v2_CommitResponse{
        version = Version, new_objects = NewObjects
    } = commit(head(), Commit, AuthorID),
    NewObjectsIDs = [
        {Tag, Ref}
     || {Tag, {_ON, Ref, _Obj}} <- ordsets:to_list(NewObjects)
    ],
    {Version, NewObjectsIDs}.

-spec update(object() | [object()]) -> revision() | no_return().
update(NewObject) when not is_list(NewObject) ->
    update(NewObject, generate_author()).

-spec update(object() | [object()], binary()) -> revision() | no_return().
update(NewObject, AuthorID) when not is_list(NewObject) ->
    update([NewObject], AuthorID);
update(NewObjects, AuthorID) ->
    Revision = head(),
    Commit = #domain_conf_v2_Commit{
        ops = [
            {update, #domain_conf_v2_UpdateOp{
                targeted_ref = {Tag, Ref},
                new_object = NewObject
            }}
         || NewObject = {Tag, {_ObjectName, Ref, _Data}} <- NewObjects
        ]
    },
    #domain_conf_v2_CommitResponse{version = Version} = commit(Revision, Commit, AuthorID),
    Version.

-spec remove([object()], binary()) -> revision() | no_return().
remove(Objects, AuthorID) ->
    Commit = #domain_conf_v2_Commit{
        ops = [
            {remove, #domain_conf_v2_RemoveOp{
                ref = Ref
            }}
         || Ref <- Objects
        ]
    },
    #domain_conf_v2_CommitResponse{version = Version} = commit(head(), Commit, AuthorID),
    Version.

-spec cleanup([ref()]) -> revision() | no_return().
cleanup(Refs) ->
    remove(Refs, generate_author()).

generate_author() ->
    Random = genlib:unique(),
    Params = #domain_conf_v2_UserOpParams{email = Random, name = Random},
    #domain_conf_v2_UserOp{id = Id} = dmt_client:user_op_create(Params, #{}),
    Id.

maybe_migrate_version(N) when is_number(N) ->
    N;
maybe_migrate_version({version, N}) ->
    N;
maybe_migrate_version({head, _}) ->
    head().
