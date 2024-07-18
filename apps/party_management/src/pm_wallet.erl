-module(pm_wallet).

-include("claim_management.hrl").
-include("party_events.hrl").

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([create/3]).
-export([create_account/1]).
-export([create_fake_account/1]).

%% Interface

-type wallet() :: dmsl_domain_thrift:'Wallet'().
-type wallet_id() :: dmsl_domain_thrift:'WalletID'().
-type wallet_params() ::
    dmsl_payproc_thrift:'WalletParams'() | dmsl_claimmgmt_thrift:'WalletParams'().
-type wallet_account() :: dmsl_domain_thrift:'WalletAccount'().
-type wallet_account_params() ::
    dmsl_payproc_thrift:'WalletAccountParams'() | dmsl_claimmgmt_thrift:'WalletAccountParams'().

-spec create(wallet_id(), wallet_params(), pm_datetime:timestamp()) -> wallet().
create(
    ID,
    #payproc_WalletParams{
        name = Name,
        contract_id = ContractID
    },
    Timestamp
) ->
    #domain_Wallet{
        id = ID,
        name = Name,
        created_at = Timestamp,
        blocking = ?unblocked(Timestamp),
        suspension = ?active(Timestamp),
        contract = ContractID
    };
create(
    ID,
    #claimmgmt_WalletParams{
        name = Name,
        contract_id = ContractID
    },
    Timestamp
) ->
    #domain_Wallet{
        id = ID,
        name = Name,
        created_at = Timestamp,
        blocking = ?unblocked(Timestamp),
        suspension = ?active(Timestamp),
        contract = ContractID
    }.

-spec create_account(wallet_account_params()) -> wallet_account().
create_account(#payproc_WalletAccountParams{currency = Currency}) ->
    SymbolicCode = Currency#domain_CurrencyRef.symbolic_code,
    SettlementID = pm_accounting:create_account(SymbolicCode),
    #domain_WalletAccount{
        currency = Currency,
        settlement = SettlementID
    };
create_account(#claimmgmt_WalletAccountParams{currency = Currency}) ->
    SymbolicCode = Currency#domain_CurrencyRef.symbolic_code,
    SettlementID = pm_accounting:create_account(SymbolicCode),
    #domain_WalletAccount{
        currency = Currency,
        settlement = SettlementID
    }.

-spec create_fake_account(wallet_account_params()) -> wallet_account().
create_fake_account(#payproc_WalletAccountParams{currency = Currency}) ->
    #domain_WalletAccount{
        currency = Currency,
        settlement = 0
    }.
