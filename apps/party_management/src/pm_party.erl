-module(pm_party).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Party support functions

-export([get_term_set/2]).
-export([reduce_terms/3]).

-export([get_shop_account/3]).
-export([get_wallet_account/3]).
-export([get_account_state/3]).

-export_type([party/0]).
-export_type([party_id/0]).

%%

-type party() :: dmsl_domain_thrift:'PartyConfig'().
-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type termset_ref() :: dmsl_domain_thrift:'TermSetHierarchyRef'().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type shop_account() :: dmsl_domain_thrift:'ShopAccount'().
-type wallet_id() :: dmsl_domain_thrift:'WalletID'().
-type wallet_account() :: dmsl_domain_thrift:'WalletAccount'().

-type revision() :: pm_domain:revision().

%% Interface

-spec get_shop_account(shop_id(), party_id(), revision()) -> shop_account().
get_shop_account(ShopID, PartyID, DomainRevision) ->
    #domain_PartyConfig{shops = Shops} =
        ensure_found({party_config, #domain_PartyConfigRef{id = PartyID}}, DomainRevision),
    case
        ensure_owned(
            {shop_config, #domain_ShopConfigRef{id = ShopID}},
            Shops,
            #payproc_ShopNotFound{},
            DomainRevision
        )
    of
        #domain_ShopConfig{account = Account, party_id = PartyID} ->
            Account;
        _ ->
            throw(#payproc_ShopNotFound{})
    end.

-spec get_wallet_account(wallet_id(), party_id(), revision()) -> wallet_account().
get_wallet_account(WalletID, PartyID, DomainRevision) ->
    #domain_PartyConfig{wallets = Wallets} =
        ensure_found({party_config, #domain_PartyConfigRef{id = PartyID}}, DomainRevision),
    case
        ensure_owned(
            {wallet_config, #domain_WalletConfigRef{id = WalletID}},
            Wallets,
            #payproc_WalletNotFound{},
            DomainRevision
        )
    of
        #domain_WalletConfig{account = Account, party_id = PartyID} ->
            Account;
        _ ->
            throw(#payproc_WalletNotFound{})
    end.

-spec get_account_state(dmsl_accounter_thrift:'AccountID'(), party_id(), revision()) ->
    dmsl_payproc_thrift:'AccountState'().
get_account_state(AccountID, PartyID, DomainRevision) ->
    #domain_PartyConfig{shops = Shops, wallets = Wallets} =
        ensure_found({party_config, #domain_PartyConfigRef{id = PartyID}}, DomainRevision),
    ok = ensure_account(
        AccountID,
        wrap_in_tag(shop_config, Shops) ++ wrap_in_tag(wallet_config, Wallets),
        DomainRevision
    ),
    Account = pm_accounting:get_account(AccountID),
    #{
        currency_code := CurrencyCode
    } = Account,
    CurrencyRef = #domain_CurrencyRef{
        symbolic_code = CurrencyCode
    },
    Currency = pm_domain:get(pm_domain:head(), {currency, CurrencyRef}),
    Balance = pm_accounting:get_balance(AccountID),
    #{
        own_amount := OwnAmount,
        min_available_amount := MinAvailableAmount
    } = Balance,
    #payproc_AccountState{
        account_id = AccountID,
        own_amount = OwnAmount,
        available_amount = MinAvailableAmount,
        currency = Currency
    }.

wrap_in_tag(Tag, Items) ->
    lists:map(fun(Item) -> {Tag, Item} end, Items).

ensure_found(Ref, DomainRevision) ->
    case pm_domain:find(DomainRevision, Ref) of
        notfound ->
            throw(#payproc_PartyNotFound{});
        ObjectData ->
            ObjectData
    end.

ensure_owned(_Ref, [], Exception, _DomainRevision) ->
    throw(Exception);
ensure_owned({_Tag, Record} = Ref, [Record | _], _Exception, DomainRevision) ->
    ensure_found(Ref, DomainRevision);
ensure_owned(Ref, [_ | Items], Exception, DomainRevision) ->
    ensure_owned(Ref, Items, Exception, DomainRevision).

%% Internals

-spec reduce_terms(dmsl_domain_thrift:'TermSet'(), pm_selector:varset(), revision()) -> dmsl_domain_thrift:'TermSet'().
reduce_terms(TermSet, VS, Revision) ->
    reduce_terms(TermSet, {struct, struct, {dmsl_domain_thrift, 'TermSet'}}, VS, Revision).

reduce_terms(undefined, _Type, _VS, _Revision) ->
    undefined;
reduce_terms(Terms, Type, VS, Revision) ->
    case is_terms(Type, Terms) of
        true ->
            reduce_terms_fields(Terms, Type, VS, Revision);
        false ->
            case is_selector(Type) of
                true ->
                    pm_selector:reduce(Terms, VS, Revision);
                false ->
                    case is_predicate(Type) of
                        true -> pm_selector:reduce_predicate(Terms, VS, Revision);
                        false -> error({unknown_reducee, Terms})
                    end
            end
    end.

%% Explicit Terms check here: since for predicates and selectors it's done in corresponding modules
is_terms({struct, struct, {dmsl_domain_thrift, Struct}}, Terms) when
    Struct =:= 'TermSet';
    Struct =:= 'PaymentsServiceTerms';
    Struct =:= 'RecurrentPaytoolsServiceTerms';
    Struct =:= 'PaymentHoldsServiceTerms';
    Struct =:= 'PaymentRefundsServiceTerms';
    Struct =:= 'PartialRefundsServiceTerms';
    Struct =:= 'PaymentChargebackServiceTerms';
    Struct =:= 'PartialCaptureServiceTerms';
    Struct =:= 'ReportsServiceTerms';
    Struct =:= 'ServiceAcceptanceActsTerms';
    Struct =:= 'WalletServiceTerms';
    Struct =:= 'WithdrawalServiceTerms';
    Struct =:= 'W2WServiceTerms';
    Struct =:= 'PaymentAllocationServiceTerms'
->
    is_record(Terms, dmsl_domain_thrift:record_name(Struct));
is_terms(_, _) ->
    false.

is_predicate({struct, union, {dmsl_domain_thrift, 'Predicate'}}) -> true;
is_predicate(_FieldType) -> false.

is_selector({struct, union, {dmsl_domain_thrift, UnionName}}) ->
    pm_utils:binary_ends_with(atom_to_binary(UnionName), <<"Selector">>);
is_selector(_) ->
    false.

reduce_terms_fields(Terms, Type, VS, Revision) ->
    StructInfo = get_terms_struct_info(Type),
    reduce_terms_fields(Terms, 2, StructInfo, VS, Revision).

reduce_terms_fields(Terms, Idx, [{_, optional, Type, _Name, _} | Rest], VS, Revision) ->
    Term = reduce_terms(element(Idx, Terms), Type, VS, Revision),
    reduce_terms_fields(setelement(Idx, Terms, Term), Idx + 1, Rest, VS, Revision);
reduce_terms_fields(Terms, _Idx, [], _, _) ->
    Terms.

get_terms_struct_info(Type) ->
    {struct, struct, {Mod, StructName}} = Type,
    {struct, struct, StructInfo} = Mod:struct_info(StructName),
    StructInfo.

-spec get_term_set(termset_ref(), revision()) -> dmsl_domain_thrift:'TermSet'() | no_return().
get_term_set(TermsRef, Revision) ->
    case pm_domain:find(Revision, {term_set_hierarchy, TermsRef}) of
        #domain_TermSetHierarchy{parent_terms = ParentRef, term_set = TermSet} ->
            case ParentRef of
                undefined ->
                    TermSet;
                #domain_TermSetHierarchyRef{} ->
                    ParentTermSet = get_term_set(ParentRef, Revision),
                    merge_terms([ParentTermSet, TermSet])
            end;
        notfound ->
            throw(#payproc_TermSetHierarchyNotFound{})
    end.

merge_terms(TermSets) when is_list(TermSets) ->
    Type = {struct, struct, {dmsl_domain_thrift, 'TermSet'}},
    lists:foldl(fun(Left, Right) -> merge_terms(Left, Right, Type) end, undefined, TermSets).

merge_terms(Left, Right, Type) when element(1, Left) == element(1, Right), tuple_size(Left) == tuple_size(Right) ->
    case is_terms(Type, Left) of
        false ->
            %% Replace the value altogether
            Left;
        true ->
            merge_terms_fields(Left, Right, Type)
    end;
merge_terms(undefined, Right, _Type) ->
    Right;
merge_terms(Left, _Right, _Type) ->
    Left.

merge_terms_fields(Left, Right, Type) ->
    StructInfo = get_terms_struct_info(Type),
    Target = setelement(1, erlang:make_tuple(tuple_size(Left), undefined), element(1, Left)),
    merge_terms_fields(Target, Left, Right, 2, StructInfo).

merge_terms_fields(Target, Left, Right, Idx, [{_, optional, Type, _Name, _} | Rest]) ->
    Term = merge_terms(element(Idx, Left), element(Idx, Right), Type),
    merge_terms_fields(setelement(Idx, Target, Term), Left, Right, Idx + 1, Rest);
merge_terms_fields(Target, _Left, _Right, _Idx, []) ->
    Target.

ensure_account(_AccountID, [], _DomainRevision) ->
    throw(#payproc_AccountNotFound{});
ensure_account(AccountID, [Ref | Items], DomainRevision) ->
    case pm_domain:find(DomainRevision, Ref) of
        #domain_ShopConfig{account = #domain_ShopAccount{settlement = AccountID}} ->
            ok;
        #domain_ShopConfig{account = #domain_ShopAccount{guarantee = AccountID}} ->
            ok;
        #domain_WalletConfig{account = #domain_WalletAccount{settlement = AccountID}} ->
            ok;
        notfound ->
            ensure_account(AccountID, Items, DomainRevision)
    end.
