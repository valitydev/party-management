-ifndef(__pm_claim_management_hrl__).
-define(__pm_claim_management_hrl__, included).

-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").

-define(cm_modification_unit(ModID, Timestamp, Mod, UserInfo), #claim_management_ModificationUnit{
    modification_id = ModID,
    created_at = Timestamp,
    modification = Mod,
    user_info = UserInfo
}).

-define(cm_party_modification(ModID, Timestamp, Mod, UserInfo),
    ?cm_modification_unit(ModID, Timestamp, {party_modification, Mod}, UserInfo)
).

-define(cm_claim_modification(ModID, Timestamp, Mod, UserInfo),
    ?cm_modification_unit(ModID, Timestamp, {claim_modification, Mod}, UserInfo)
).

%%% Contractor

-define(cm_contractor_modification(ContractorID, Mod),
    {contractor_modification, #claim_management_ContractorModificationUnit{
        id = ContractorID,
        modification = Mod
    }}
).

-define(cm_contractor_creation(ContractorID, Contractor),
    ?cm_contractor_modification(ContractorID, {creation, Contractor})
).

-define(cm_contractor_identity_documents_modification(ContractorID, Documents),
    ?cm_contractor_modification(ContractorID, ?cm_identity_documents_modification(Documents))
).

-define(cm_contractor_identification_level_modification(ContractorID, Level),
    ?cm_contractor_modification(ContractorID, {identification_level_modification, Level})
).

%%% Contract

-define(cm_contract_modification(ContractID, Mod),
    {contract_modification, #claim_management_ContractModificationUnit{
        id = ContractID,
        modification = Mod
    }}
).

-define(cm_contract_creation(ContractID, ContractParams),
    ?cm_contract_modification(ContractID, {creation, ContractParams})
).

-define(cm_contract_termination(Reason),
    {termination, #claim_management_ContractTermination{reason = Reason}}
).

-define(cm_payout_tool_modification(PayoutToolID, Mod),
    {payout_tool_modification, #claim_management_PayoutToolModificationUnit{
        payout_tool_id = PayoutToolID,
        modification = Mod
    }}
).

-define(cm_payout_tool_creation(PayoutToolID, PayoutToolParams),
    ?cm_payout_tool_modification(PayoutToolID, {creation, PayoutToolParams})
).

-define(cm_payout_tool_info_modification(PayoutToolID, Info),
    ?cm_payout_tool_modification(PayoutToolID, {info_modification, Info})
).

-define(cm_payout_schedule_modification(BusinessScheduleRef),
    {payout_schedule_modification, #claim_management_ScheduleModification{
        schedule = BusinessScheduleRef
    }}
).

-define(cm_cash_register_unit_creation(ID, Params),
    {creation, #claim_management_CashRegisterParams{
        cash_register_provider_id = ID,
        cash_register_provider_params = Params
    }}
).

-define(cm_shop_cash_register_modification_unit(ShopID, Unit),
    ?cm_shop_modification(ShopID, {cash_register_modification_unit, Unit})
).

-define(cm_cash_register_modification_unit(Unit),
    {cash_register_modification_unit, Unit}
).

-define(cm_adjustment_modification(ContractAdjustmentID, Mod),
    {adjustment_modification, #claim_management_ContractAdjustmentModificationUnit{
        adjustment_id = ContractAdjustmentID,
        modification = Mod
    }}
).

-define(cm_adjustment_creation(ContractAdjustmentID, Params),
    ?cm_adjustment_modification(
        ContractAdjustmentID,
        {creation, Params}
    )
).

%%% Shop

-define(cm_shop_modification(ShopID, Mod),
    {shop_modification, #claim_management_ShopModificationUnit{
        id = ShopID,
        modification = Mod
    }}
).

-define(cm_shop_contract_modification(ContractID, PayoutToolID),
    {contract_modification, #claim_management_ShopContractModification{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }}
).

-define(cm_shop_creation(ShopID, ShopParams),
    ?cm_shop_modification(ShopID, {creation, ShopParams})
).

-define(cm_shop_account_creation_params(CurrencyRef),
    {shop_account_creation, #claim_management_ShopAccountParams{
        currency = CurrencyRef
    }}
).

-define(cm_shop_account_creation(ShopID, CurrencyRef),
    ?cm_shop_modification(
        ShopID,
        ?cm_shop_account_creation_params(CurrencyRef)
    )
).

%%% Wallet
-define(cm_wallet_modification(ID, Modification),
    {wallet_modification, #claim_management_WalletModificationUnit{id = ID, modification = Modification}}
).

-define(cm_wallet_account_creation_params(CurrencyRef),
    {account_creation, #claim_management_WalletAccountParams{
        currency = CurrencyRef
    }}
).

-define(cm_wallet_account_creation(WalletID, CurrencyRef),
    ?cm_wallet_modification(
        WalletID,
        ?cm_wallet_account_creation_params(CurrencyRef)
    )
).

%%% Error

-define(cm_invalid_party_changeset(Reason, InvalidChangeset), #claim_management_InvalidChangeset{
    reason = {invalid_party_changeset, Reason},
    invalid_changeset = InvalidChangeset
}).

-define(cm_invalid_shop(ID, Reason),
    {invalid_shop, #claim_management_InvalidShop{id = ID, reason = Reason}}
).

-define(cm_invalid_shop_account_not_exists(ID),
    ?cm_invalid_shop(ID, {account_not_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_shop_not_exists(ID),
    ?cm_invalid_shop(ID, {not_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_shop_already_exists(ID),
    ?cm_invalid_shop(ID, {already_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_shop_contract_terms_violated(ID, ContractID, Terms),
    ?cm_invalid_shop(
        ID,
        {contract_terms_violated, #claim_management_ContractTermsViolated{
            contract_id = ContractID,
            terms = Terms
        }}
    )
).

-define(cm_invalid_shop_payout_tool(ID, Reason),
    ?cm_invalid_shop(ID, {payout_tool_invalid, Reason})
).

-define(cm_invalid_shop_payout_tool_not_set_for_payouts(ID, Schedule),
    ?cm_invalid_shop_payout_tool(
        ID,
        {not_set_for_payouts, #claim_management_PayoutToolNotSetForPayouts{
            payout_schedule = Schedule
        }}
    )
).

-define(cm_invalid_shop_payout_tool_currency_mismatch(ID, PayoutToolID, ShopAccountCurrency, PayoutToolCurrency),
    ?cm_invalid_shop_payout_tool(
        ID,
        {currency_mismatch, #claim_management_PayoutToolCurrencyMismatch{
            shop_account_currency = ShopAccountCurrency,
            payout_tool_id = PayoutToolID,
            payout_tool_currency = PayoutToolCurrency
        }}
    )
).

-define(cm_invalid_shop_payout_tool_not_in_contract(ID, ContractID, PayoutToolID),
    ?cm_invalid_shop_payout_tool(
        ID,
        {not_in_contract, #claim_management_PayoutToolNotInContract{
            contract_id = ContractID,
            payout_tool_id = PayoutToolID
        }}
    )
).

-define(cm_invalid_contract(ID, Reason),
    {invalid_contract, #claim_management_InvalidContract{id = ID, reason = Reason}}
).

-define(cm_invalid_contract_not_exists(ID),
    ?cm_invalid_contract(ID, {not_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_contract_already_exists(ID),
    ?cm_invalid_contract(ID, {already_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_contract_invalid_status_terminated(ID, T),
    ?cm_invalid_contract(ID, {invalid_status, {terminated, #domain_ContractTerminated{terminated_at = T}}})
).

-define(cm_invalid_contract_contractor_not_exists(ID, ContractorID),
    ?cm_invalid_contract(ID, {contractor_not_exists, #claim_management_ContractorNotExists{id = ContractorID}})
).

-define(cm_invalid_contractor(ID, Reason),
    {invalid_contractor, #claim_management_InvalidContractor{id = ID, reason = Reason}}
).

-define(cm_invalid_contractor_not_exists(ID),
    ?cm_invalid_contractor(ID, {not_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_contractor_already_exists(ID),
    ?cm_invalid_contractor(ID, {already_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet(ID, Reason),
    {invalid_wallet, #claim_management_InvalidWallet{id = ID, reason = Reason}}
).

-define(cm_invalid_wallet_not_exists(ID),
    ?cm_invalid_wallet(ID, {not_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_already_exists(ID),
    ?cm_invalid_wallet(ID, {already_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_account_not_exists(ID),
    ?cm_invalid_wallet(ID, {account_not_exists, #claim_management_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_contract_terms_violated(ID, ContractID, Terms),
    ?cm_invalid_wallet(
        ID,
        {contract_terms_violated, #claim_management_ContractTermsViolated{
            contract_id = ContractID,
            terms = Terms
        }}
    )
).

-endif.
