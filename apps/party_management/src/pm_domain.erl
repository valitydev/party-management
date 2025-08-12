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
-type ref() :: dmt_client:object_ref().
-type object() :: dmt_client:domain_object().
-type data() :: _.

-export_type([revision/0]).
-export_type([ref/0]).
-export_type([object/0]).
-export_type([data/0]).

-spec head() -> revision().
head() ->
    dmt_client:get_latest_version().

-spec get(revision(), ref()) -> data() | no_return().
get(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object(Revision, Ref))
    catch
        error:version_not_found ->
            error({object_not_found, {Revision, Ref}});
        throw:#domain_conf_v2_ObjectNotFound{} ->
            error({object_not_found, {Revision, Ref}})
    end.

-spec find(revision(), ref()) -> data() | notfound.
find(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object(Revision, Ref))
    catch
        error:version_not_found ->
            notfound;
        throw:#domain_conf_v2_ObjectNotFound{} ->
            notfound
    end.

-spec exists(revision(), ref()) -> boolean().
exists(Revision, Ref) ->
    try
        _ = dmt_client:checkout_object(Revision, Ref),
        true
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            false
    end.

extract_data(#domain_conf_v2_VersionedObject{object = {_Tag, {_Name, _Ref, Data}}}) ->
    Data.

commit(Revision, Operations, AuthorID) ->
    dmt_client:commit(Revision, Operations, AuthorID).

-spec insert(object() | [object()]) -> {revision(), [ref()]} | no_return().
insert(Objects) ->
    insert(Objects, generate_author()).

-spec insert(object() | [object()], binary()) -> {revision(), [ref()]} | no_return().
insert(Object, AuthorID) when not is_list(Object) ->
    insert([Object], AuthorID);
insert(Objects, AuthorID) ->
    Commit = [
        {insert, #domain_conf_v2_InsertOp{
            object = {Type, Object},
            force_ref = {Type, ForceRef}
        }}
     || {Type, {_ObjectName, ForceRef, Object}} <- Objects
    ],
    #domain_conf_v2_CommitResponse{version = Version, new_objects = NewObjects} =
        commit(head(), Commit, AuthorID),
    NewObjectsIDs = [
        {Tag, Ref}
     || {Tag, {_ON, Ref, _Obj}} <- ordsets:to_list(NewObjects)
    ],
    {Version, NewObjectsIDs}.

-spec update(object() | [object()]) -> revision() | no_return().
update(NewObject) when not is_list(NewObject) ->
    update(NewObject, generate_author()).

-spec update(object() | [object()], binary()) -> revision() | no_return().
update(Objects, AuthorID) ->
    dmt_client:update(Objects, AuthorID).

-spec cleanup([ref()]) -> revision() | no_return().
cleanup(Refs) ->
    Commit = [
        {remove, #domain_conf_v2_RemoveOp{
            ref = Ref
        }}
     || Ref <- Refs
    ],
    #domain_conf_v2_CommitResponse{version = Version} =
        commit(head(), Commit, generate_author()),
    Version.

generate_author() ->
    dmt_client:create_author(genlib:unique(), genlib:unique()).
