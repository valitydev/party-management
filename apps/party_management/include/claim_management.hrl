-ifndef(__pm_claim_management_hrl__).
-define(__pm_claim_management_hrl__, included).

-include_lib("damsel/include/dmsl_claimmgmt_thrift.hrl").

-define(cm_modification_unit(ModID, Timestamp, Mod, UserInfo), #claimmgmt_ModificationUnit{
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
    {contractor_modification, #claimmgmt_ContractorModificationUnit{
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
    {contract_modification, #claimmgmt_ContractModificationUnit{
        id = ContractID,
        modification = Mod
    }}
).

-define(cm_contract_creation(ContractID, ContractParams),
    ?cm_contract_modification(ContractID, {creation, ContractParams})
).

-define(cm_contract_termination(Reason),
    {termination, #claimmgmt_ContractTermination{reason = Reason}}
).

-define(cm_cash_register_unit_creation(ID, Params),
    {creation, #claimmgmt_CashRegisterParams{
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
    {adjustment_modification, #claimmgmt_ContractAdjustmentModificationUnit{
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
    {shop_modification, #claimmgmt_ShopModificationUnit{
        id = ShopID,
        modification = Mod
    }}
).

-define(cm_shop_contract_modification(ContractID),
    {contract_modification, #claimmgmt_ShopContractModification{
        contract_id = ContractID
    }}
).

-define(cm_shop_creation(ShopID, ShopParams),
    ?cm_shop_modification(ShopID, {creation, ShopParams})
).

-define(cm_shop_account_creation_params(CurrencyRef),
    {shop_account_creation, #claimmgmt_ShopAccountParams{
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
    {wallet_modification, #claimmgmt_WalletModificationUnit{id = ID, modification = Modification}}
).

-define(cm_wallet_creation_params(Name, ContractID),
    {creation, #claimmgmt_WalletParams{
        name = Name,
        contract_id = ContractID
    }}
).

-define(cm_wallet_account_creation_params(CurrencyRef),
    {account_creation, #claimmgmt_WalletAccountParams{
        currency = CurrencyRef
    }}
).

-define(cm_wallet_creation(WalletID, Name, ContractID),
    ?cm_wallet_modification(
        WalletID,
        ?cm_wallet_creation_params(Name, ContractID)
    )
).

-define(cm_wallet_account_creation(WalletID, CurrencyRef),
    ?cm_wallet_modification(
        WalletID,
        ?cm_wallet_account_creation_params(CurrencyRef)
    )
).

%%% Additional info
-define(cm_additional_info_modification(PartyName, Comment, Emails),
    {additional_info_modification, #claimmgmt_AdditionalInfoModificationUnit{
        party_name = PartyName,
        comment = Comment,
        manager_contact_emails = Emails
    }}
).

-define(cm_additional_info_party_name_modification(PartyName),
    {additional_info_party_name_modification, PartyName}
).
-define(cm_additional_info_party_comment_modification(PartyComment),
    {additional_info_party_comment_modification, PartyComment}
).
-define(cm_additional_info_emails_modification(Emails),
    {additional_info_emails_modification, Emails}
).

%%% Error

-define(cm_invalid_party_changeset(Reason, InvalidChangeset), #claimmgmt_InvalidChangeset{
    reason = {invalid_party_changeset, Reason},
    invalid_changeset = InvalidChangeset
}).

-define(cm_invalid_shop(ID, Reason),
    {invalid_shop, #claimmgmt_InvalidShop{id = ID, reason = Reason}}
).

-define(cm_invalid_shop_account_not_exists(ID),
    ?cm_invalid_shop(ID, {account_not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_shop_not_exists(ID),
    ?cm_invalid_shop(ID, {not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_shop_already_exists(ID),
    ?cm_invalid_shop(ID, {already_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_shop_contract_terms_violated(ID, ContractID, Terms),
    ?cm_invalid_shop(
        ID,
        {contract_terms_violated, #claimmgmt_ContractTermsViolated{
            contract_id = ContractID,
            terms = Terms
        }}
    )
).

-define(cm_invalid_contract(ID, Reason),
    {invalid_contract, #claimmgmt_InvalidContract{id = ID, reason = Reason}}
).

-define(cm_invalid_contract_not_exists(ID),
    ?cm_invalid_contract(ID, {not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_contract_already_exists(ID),
    ?cm_invalid_contract(ID, {already_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_contract_invalid_status_terminated(ID, T),
    ?cm_invalid_contract(ID, {invalid_status, {terminated, #domain_ContractTerminated{terminated_at = T}}})
).

-define(cm_invalid_contract_contractor_not_exists(ID, ContractorID),
    ?cm_invalid_contract(ID, {contractor_not_exists, #claimmgmt_ContractorNotExists{id = ContractorID}})
).

-define(cm_invalid_contractor(ID, Reason),
    {invalid_contractor, #claimmgmt_InvalidContractor{id = ID, reason = Reason}}
).

-define(cm_invalid_contractor_not_exists(ID),
    ?cm_invalid_contractor(ID, {not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_contractor_already_exists(ID),
    ?cm_invalid_contractor(ID, {already_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet(ID, Reason),
    {invalid_wallet, #claimmgmt_InvalidWallet{id = ID, reason = Reason}}
).

-define(cm_invalid_wallet_not_exists(ID),
    ?cm_invalid_wallet(ID, {not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_already_exists(ID),
    ?cm_invalid_wallet(ID, {already_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_account_not_exists(ID),
    ?cm_invalid_wallet(ID, {account_not_exists, #claimmgmt_InvalidClaimConcreteReason{}})
).

-define(cm_invalid_wallet_contract_terms_violated(ID, ContractID, Terms),
    ?cm_invalid_wallet(
        ID,
        {contract_terms_violated, #claimmgmt_ContractTermsViolated{
            contract_id = ContractID,
            terms = Terms
        }}
    )
).

-endif.
