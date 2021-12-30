-ifndef(__pm_ct_domain__).
-define(__pm_ct_domain__, 42).

-include("domain.hrl").

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-define(ordset(Es), ordsets:from_list(Es)).

-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(pmt_sys(ID), #domain_PaymentSystemRef{id = ID}).
-define(pmt_srv(ID), #domain_PaymentServiceRef{id = ID}).
-define(pomt(M), #domain_PayoutMethodRef{id = M}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(prvtrm(ID), #domain_ProviderTerminalRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(tmpl(ID), #domain_ContractTemplateRef{id = ID}).
-define(trms(ID), #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).
-define(pinst(ID), #domain_PaymentInstitutionRef{id = ID}).
-define(bank(ID), #domain_BankRef{id = ID}).
-define(bussched(ID), #domain_BusinessScheduleRef{id = ID}).
-define(ruleset(ID), #domain_RoutingRulesetRef{id = ID}).
-define(mob(ID), #domain_MobileOperatorRef{id = ID}).
-define(crypta(ID), #domain_CryptoCurrencyRef{id = ID}).
-define(token_srv(ID), #domain_BankCardTokenServiceRef{id = ID}).
-define(bank_card(ID), #domain_BankCardPaymentMethod{payment_system = ?pmt_sys(ID)}).
-define(token_bank_card(ID, Prv), ?token_bank_card(ID, Prv, dpan)).
-define(token_bank_card(ID, Prv, Method), #domain_BankCardPaymentMethod{
    payment_system = ?pmt_sys(ID),
    payment_token = ?token_srv(Prv),
    tokenization_method = Method
}).
-define(crit(ID), #domain_CriterionRef{id = ID}).
-define(crp(ID), #domain_CashRegisterProviderRef{id = ID}).

-define(cashrng(Lower, Upper), #domain_CashRange{lower = Lower, upper = Upper}).

-define(prvacc(Stl), #domain_ProviderAccount{settlement = Stl}).
-define(partycond(ID, Def), {condition, {party, #domain_PartyCondition{id = ID, definition = Def}}}).

-define(fixed(Amount, Currency),
    {fixed, #domain_CashVolumeFixed{
        cash = #domain_Cash{
            amount = Amount,
            currency = ?currency(Currency)
        }
    }}
).

-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C
    }}
).

-define(share_with_rounding_method(P, Q, C, RM),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C,
        rounding_method = RM
    }}
).

-define(cfpost(A1, A2, V), #domain_CashFlowPosting{
    source = A1,
    destination = A2,
    volume = V
}).

-define(tkz_bank_card(PaymentSystem, TokenProvider), ?tkz_bank_card(PaymentSystem, TokenProvider, dpan)).

-define(tkz_bank_card(PaymentSystem, TokenProvider, TokenizationMethod), #domain_TokenizedBankCard{
    payment_system_deprecated = PaymentSystem,
    token_provider_deprecated = TokenProvider,
    tokenization_method = TokenizationMethod
}).

-define(timeout_reason(), <<"Timeout">>).

-define(bank_card_payment_tool(BankName, IsCVVEmpty),
    {bank_card, #domain_BankCard{
        token = <<>>,
        bin = <<>>,
        last_digits = <<>>,
        bank_name = BankName,
        payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
        payment_system_deprecated = visa,
        issuer_country = rus,
        is_cvv_empty = IsCVVEmpty
    }}
).

-define(bank_card_payment_tool(BankName),
    ?bank_card_payment_tool(BankName, undefined)
).

-endif.
