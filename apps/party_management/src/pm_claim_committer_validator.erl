%%%
%%% Copyright 2021 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(pm_claim_committer_validator).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").

-include("claim_management.hrl").
-include("party_events.hrl").

-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type wallet_id() :: dmsl_domain_thrift:'WalletID'().
-type contract_id() :: dmsl_domain_thrift:'ContractID'().
-type party() :: pm_party:party().
-type timestamp() :: pm_datetime:timestamp().
-type revision() :: pm_domain:revision().

%% API
-export([assert_contracts_valid/1]).
-export([assert_shops_valid/3]).
-export([assert_wallets_valid/3]).

-spec assert_contracts_valid(party()) -> ok | no_return().
assert_contracts_valid(Party) ->
    genlib_map:foreach(
        fun(_ID, Contract) ->
            assert_contract_valid(Contract, Party)
        end,
        Party#domain_Party.contracts
    ).

-spec assert_shops_valid(timestamp(), revision(), party()) -> ok | no_return().
assert_shops_valid(Timestamp, Revision, Party) ->
    genlib_map:foreach(
        fun(_ID, Shop) ->
            assert_shop_valid(Shop, Timestamp, Revision, Party)
        end,
        Party#domain_Party.shops
    ).

-spec assert_wallets_valid(timestamp(), revision(), party()) -> ok | no_return().
assert_wallets_valid(Timestamp, Revision, Party) ->
    genlib_map:foreach(
        fun(_ID, Wallet) ->
            assert_wallet_valid(Wallet, Timestamp, Revision, Party)
        end,
        Party#domain_Party.wallets
    ).

assert_contract_valid(
    #domain_Contract{id = ID, contractor_id = ContractorID},
    Party
) when ContractorID /= undefined ->
    case pm_party:get_contractor(ContractorID, Party) of
        #domain_PartyContractor{} ->
            ok;
        undefined ->
            throw({invalid_changeset, ?cm_invalid_contract_contractor_not_exists(ID, ContractorID)})
    end;
assert_contract_valid(
    #domain_Contract{id = ID, contractor_id = undefined, contractor = undefined},
    _Party
) ->
    throw({invalid_changeset, ?cm_invalid_contract_contractor_not_exists(ID, undefined)});
assert_contract_valid(_, _) ->
    ok.

assert_shop_valid(#domain_Shop{contract_id = ContractID} = Shop, Timestamp, Revision, Party) ->
    case pm_party:get_contract(ContractID, Party) of
        #domain_Contract{} = Contract ->
            _ = assert_shop_contract_valid(Shop, Contract, Timestamp, Revision),
            _ = assert_shop_payout_tool_valid(Shop, Contract),
            ok;
        undefined ->
            throw({invalid_changeset, ?cm_invalid_contract_not_exists(ContractID)})
    end.

assert_shop_contract_valid(
    #domain_Shop{id = ID, category = CategoryRef, account = ShopAccount},
    Contract,
    Timestamp,
    Revision
) ->
    Terms = pm_party:get_terms(Contract, Timestamp, Revision),
    case ShopAccount of
        #domain_ShopAccount{currency = CurrencyRef} ->
            _ = assert_currency_valid({shop, ID}, pm_contract:get_id(Contract), CurrencyRef, Terms, Revision);
        undefined ->
            throw({invalid_changeset, ?cm_invalid_shop_account_not_exists(ID)})
    end,
    #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{categories = CategorySelector}
    } = Terms,
    Categories = pm_selector:reduce_to_value(CategorySelector, #{}, Revision),
    _ =
        ordsets:is_element(CategoryRef, Categories) orelse
            throw(
                {invalid_changeset,
                    ?cm_invalid_shop_contract_terms_violated(
                        ID,
                        pm_contract:get_id(Contract),
                        #domain_TermSet{payments = #domain_PaymentsServiceTerms{categories = CategorySelector}}
                    )}
            ),
    ok.

assert_shop_payout_tool_valid(#domain_Shop{payout_tool_id = undefined, payout_schedule = undefined}, _) ->
    % automatic payouts disabled for this shop and it's ok
    ok;
assert_shop_payout_tool_valid(#domain_Shop{id = ID, payout_tool_id = undefined, payout_schedule = Schedule}, _) ->
    % automatic payouts enabled for this shop but no payout tool specified
    pm_claim_committer:raise_invalid_changeset(?cm_invalid_shop_payout_tool_not_set_for_payouts(ID, Schedule), []);
assert_shop_payout_tool_valid(#domain_Shop{id = ID, payout_tool_id = PayoutToolID} = Shop, Contract) ->
    ShopAccountCurrency = (Shop#domain_Shop.account)#domain_ShopAccount.currency,
    ContractID = Contract#domain_Contract.id,
    case pm_contract:get_payout_tool(PayoutToolID, Contract) of
        #domain_PayoutTool{currency = ShopAccountCurrency} ->
            ok;
        #domain_PayoutTool{currency = PayoutToolCurrency} ->
            throw(
                {invalid_changeset,
                    ?cm_invalid_shop_payout_tool_currency_mismatch(
                        ID,
                        PayoutToolID,
                        ShopAccountCurrency,
                        PayoutToolCurrency
                    )}
            );
        undefined ->
            throw({invalid_changeset, ?cm_invalid_shop_payout_tool_not_in_contract(ID, ContractID, PayoutToolID)})
    end.

assert_wallet_valid(#domain_Wallet{contract = ContractID} = Wallet, Timestamp, Revision, Party) ->
    case pm_party:get_contract(ContractID, Party) of
        #domain_Contract{} = Contract ->
            _ = assert_wallet_contract_valid(Wallet, Contract, Timestamp, Revision),
            ok;
        undefined ->
            throw({invalid_changeset, ?cm_invalid_contract_not_exists(ContractID)})
    end.

assert_wallet_contract_valid(
    #domain_Wallet{id = ID, account = Account},
    Contract,
    Timestamp,
    Revision
) ->
    case Account of
        #domain_WalletAccount{currency = CurrencyRef} ->
            Terms = pm_party:get_terms(Contract, Timestamp, Revision),
            _ = assert_currency_valid({wallet, ID}, pm_contract:get_id(Contract), CurrencyRef, Terms, Revision),
            ok;
        undefined ->
            throw({invalid_changeset, ?cm_invalid_wallet_account_not_exists(ID)})
    end,
    ok.

assert_currency_valid(
    {shop, _} = Prefix,
    ContractID,
    CurrencyRef,
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{currencies = Selector}},
    Revision
) ->
    Terms = #domain_TermSet{payments = #domain_PaymentsServiceTerms{currencies = Selector}},
    assert_currency_valid(Prefix, ContractID, CurrencyRef, Selector, Terms, Revision);
assert_currency_valid(
    {shop, _} = Prefix,
    ContractID,
    _,
    T = #domain_TermSet{payments = undefined},
    _
) ->
    raise_contract_terms_violated(Prefix, ContractID, T);
assert_currency_valid(
    {wallet, _} = Prefix,
    ContractID,
    CurrencyRef,
    #domain_TermSet{wallets = #domain_WalletServiceTerms{currencies = Selector}},
    Revision
) ->
    Terms = #domain_TermSet{wallets = #domain_WalletServiceTerms{currencies = Selector}},
    assert_currency_valid(Prefix, ContractID, CurrencyRef, Selector, Terms, Revision);
assert_currency_valid(
    {wallet, _} = Prefix,
    ContractID,
    _,
    T = #domain_TermSet{wallets = undefined},
    _
) ->
    raise_contract_terms_violated(Prefix, ContractID, T).

assert_currency_valid(Prefix, ContractID, CurrencyRef, Selector, Terms, Revision) ->
    Currencies = pm_selector:reduce_to_value(Selector, #{}, Revision),
    _ = ordsets:is_element(CurrencyRef, Currencies) orelse raise_contract_terms_violated(Prefix, ContractID, Terms).

-spec raise_contract_terms_violated(
    {shop, shop_id()} | {wallet, wallet_id()},
    contract_id(),
    dmsl_domain_thrift:'TermSet'()
) -> no_return().
raise_contract_terms_violated({shop, ID}, ContractID, Terms) ->
    throw({invalid_changeset, ?cm_invalid_shop_contract_terms_violated(ID, ContractID, Terms)});
raise_contract_terms_violated({wallet, ID}, ContractID, Terms) ->
    throw({invalid_changeset, ?cm_invalid_wallet_contract_terms_violated(ID, ContractID, Terms)}).
