-module(pm_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API
-export([reduce_provider/3]).
-export([reduce_provider_terminal_terms/4]).

-export([compute_proxy/3]).

-type provider() :: dmsl_domain_thrift:'Provider'().
-type terminal() :: dmsl_domain_thrift:'Terminal'().
-type provision_terms() :: dmsl_domain_thrift:'ProvisionTermSet'().
-type varset() :: pm_selector:varset().
-type domain_revision() :: pm_domain:revision().

-spec reduce_provider(provider(), varset(), domain_revision()) -> provider().
reduce_provider(Provider, VS, Rev) ->
    Provider#domain_Provider{
        terms = reduce_provision_term_set(Provider#domain_Provider.terms, VS, Rev)
    }.

-spec reduce_provider_terminal_terms(provider(), terminal(), varset(), domain_revision()) ->
    provision_terms() | undefined.
reduce_provider_terminal_terms(Provider, Terminal, VS, Rev) ->
    ProviderTerms = Provider#domain_Provider.terms,
    TerminalTerms = Terminal#domain_Terminal.terms,
    MergedTerms = merge_provision_term_sets(ProviderTerms, TerminalTerms),
    reduce_provision_term_set(MergedTerms, VS, Rev).

reduce_withdrawal_terms(undefined = Terms, _VS, _Rev) ->
    Terms;
reduce_withdrawal_terms(#domain_WithdrawalProvisionTerms{} = Terms, VS, Rev) ->
    Terms#domain_WithdrawalProvisionTerms{
        allow = reduce_predicate_if_defined(Terms#domain_WithdrawalProvisionTerms.allow, VS, Rev),
        currencies = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.currencies, VS, Rev),
        payout_methods = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.payout_methods, VS, Rev),
        cash_limit = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.cash_limit, VS, Rev),
        cash_flow = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.cash_flow, VS, Rev),
        turnover_limit = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.turnover_limit, VS, Rev)
    }.

reduce_provision_term_set(undefined = ProvisionTermSet, _VS, _DomainRevision) ->
    ProvisionTermSet;
reduce_provision_term_set(ProvisionTermSet, VS, DomainRevision) ->
    #domain_ProvisionTermSet{
        payments = pm_maybe:apply(
            fun(X) -> reduce_payment_terms(X, VS, DomainRevision) end,
            ProvisionTermSet#domain_ProvisionTermSet.payments
        ),
        recurrent_paytools = pm_maybe:apply(
            fun(X) -> reduce_recurrent_paytool_terms(X, VS, DomainRevision) end,
            ProvisionTermSet#domain_ProvisionTermSet.recurrent_paytools
        ),
        wallet = pm_maybe:apply(
            fun(X) -> reduce_wallet_provision(X, VS, DomainRevision) end,
            ProvisionTermSet#domain_ProvisionTermSet.wallet
        )
    }.

reduce_payment_terms(undefined = PaymentTerms, _VS, _DomainRevision) ->
    PaymentTerms;
reduce_payment_terms(PaymentTerms, VS, DomainRevision) ->
    PaymentTerms#domain_PaymentsProvisionTerms{
        allow = reduce_predicate_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.allow, VS, DomainRevision),
        currencies = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.currencies, VS, DomainRevision),
        categories = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.categories, VS, DomainRevision),
        payment_methods = reduce_if_defined(
            PaymentTerms#domain_PaymentsProvisionTerms.payment_methods,
            VS,
            DomainRevision
        ),
        cash_limit = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.cash_limit, VS, DomainRevision),
        cash_flow = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.cash_flow, VS, DomainRevision),
        holds = pm_maybe:apply(
            fun(X) -> reduce_payment_hold_terms(X, VS, DomainRevision) end,
            PaymentTerms#domain_PaymentsProvisionTerms.holds
        ),
        refunds = pm_maybe:apply(
            fun(X) -> reduce_payment_refund_terms(X, VS, DomainRevision) end,
            PaymentTerms#domain_PaymentsProvisionTerms.refunds
        ),
        chargebacks = pm_maybe:apply(
            fun(X) -> reduce_payment_chargeback_terms(X, VS, DomainRevision) end,
            PaymentTerms#domain_PaymentsProvisionTerms.chargebacks
        ),
        risk_coverage = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.risk_coverage, VS, DomainRevision)
    }.

reduce_payment_hold_terms(PaymentHoldTerms, VS, DomainRevision) ->
    PaymentHoldTerms#domain_PaymentHoldsProvisionTerms{
        lifetime = reduce_if_defined(PaymentHoldTerms#domain_PaymentHoldsProvisionTerms.lifetime, VS, DomainRevision),
        partial_captures = pm_maybe:apply(
            fun(X) -> reduce_partial_captures_terms(X, VS, DomainRevision) end,
            PaymentHoldTerms#domain_PaymentHoldsProvisionTerms.partial_captures
        )
    }.

reduce_partial_captures_terms(#domain_PartialCaptureProvisionTerms{} = Terms, _VS, _DomainRevision) ->
    Terms.

reduce_payment_refund_terms(PaymentRefundTerms, VS, DomainRevision) ->
    PaymentRefundTerms#domain_PaymentRefundsProvisionTerms{
        cash_flow = reduce_if_defined(
            PaymentRefundTerms#domain_PaymentRefundsProvisionTerms.cash_flow,
            VS,
            DomainRevision
        ),
        partial_refunds = pm_maybe:apply(
            fun(X) -> reduce_partial_refunds_terms(X, VS, DomainRevision) end,
            PaymentRefundTerms#domain_PaymentRefundsProvisionTerms.partial_refunds
        )
    }.

reduce_partial_refunds_terms(PartialRefundTerms, VS, DomainRevision) ->
    PartialRefundTerms#domain_PartialRefundsProvisionTerms{
        cash_limit = reduce_if_defined(
            PartialRefundTerms#domain_PartialRefundsProvisionTerms.cash_limit,
            VS,
            DomainRevision
        )
    }.

reduce_payment_chargeback_terms(PaymentChargebackTerms, VS, DomainRevision) ->
    PaymentChargebackTerms#domain_PaymentChargebackProvisionTerms{
        cash_flow = reduce_if_defined(
            PaymentChargebackTerms#domain_PaymentChargebackProvisionTerms.cash_flow,
            VS,
            DomainRevision
        )
    }.

reduce_recurrent_paytool_terms(RecurrentPaytoolTerms, VS, DomainRevision) ->
    RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms{
        cash_value = reduce_if_defined(
            RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms.cash_value,
            VS,
            DomainRevision
        ),
        categories = reduce_if_defined(
            RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms.categories,
            VS,
            DomainRevision
        ),
        payment_methods = reduce_if_defined(
            RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms.payment_methods,
            VS,
            DomainRevision
        )
    }.

reduce_wallet_provision(WalletProvisionTerms, VS, DomainRevision) ->
    #domain_WalletProvisionTerms{
        turnover_limit = reduce_if_defined(
            WalletProvisionTerms#domain_WalletProvisionTerms.turnover_limit,
            VS,
            DomainRevision
        ),
        withdrawals = pm_maybe:apply(
            fun(X) -> reduce_withdrawal_terms(X, VS, DomainRevision) end,
            WalletProvisionTerms#domain_WalletProvisionTerms.withdrawals
        )
    }.

merge_provision_term_sets(
    #domain_ProvisionTermSet{
        payments = PPayments,
        recurrent_paytools = PRecurrents,
        wallet = PWallet
    },
    #domain_ProvisionTermSet{
        payments = TPayments,
        % TODO: Allow to define recurrent terms in terminal
        recurrent_paytools = _TRecurrents,
        wallet = TWallet
    }
) ->
    #domain_ProvisionTermSet{
        payments = merge_payment_terms(PPayments, TPayments),
        recurrent_paytools = PRecurrents,
        wallet = merge_wallet_terms(PWallet, TWallet)
    };
merge_provision_term_sets(ProviderTerms, TerminalTerms) ->
    pm_utils:select_defined(TerminalTerms, ProviderTerms).

merge_payment_terms(
    #domain_PaymentsProvisionTerms{
        allow = PAllow,
        currencies = PCurrencies,
        categories = PCategories,
        payment_methods = PPaymentMethods,
        cash_limit = PCashLimit,
        cash_flow = PCashflow,
        holds = PHolds,
        refunds = PRefunds,
        chargebacks = PChargebacks,
        risk_coverage = PRiskCoverage,
        turnover_limits = PTurnoverLimits
    },
    #domain_PaymentsProvisionTerms{
        allow = TAllow,
        currencies = TCurrencies,
        categories = TCategories,
        payment_methods = TPaymentMethods,
        cash_limit = TCashLimit,
        cash_flow = TCashflow,
        holds = THolds,
        refunds = TRefunds,
        chargebacks = TChargebacks,
        risk_coverage = TRiskCoverage,
        turnover_limits = TTurnoverLimits
    }
) ->
    AllowOrdset = ordsets:from_list(genlib_list:compact([PAllow, TAllow])),
    #domain_PaymentsProvisionTerms{
        allow = {all_of, AllowOrdset},
        currencies = pm_utils:select_defined(TCurrencies, PCurrencies),
        categories = pm_utils:select_defined(TCategories, PCategories),
        payment_methods = pm_utils:select_defined(TPaymentMethods, PPaymentMethods),
        cash_limit = pm_utils:select_defined(TCashLimit, PCashLimit),
        cash_flow = pm_utils:select_defined(TCashflow, PCashflow),
        holds = pm_utils:select_defined(THolds, PHolds),
        refunds = pm_utils:select_defined(TRefunds, PRefunds),
        chargebacks = pm_utils:select_defined(TChargebacks, PChargebacks),
        risk_coverage = pm_utils:select_defined(TRiskCoverage, PRiskCoverage),
        turnover_limits = pm_utils:select_defined(TTurnoverLimits, PTurnoverLimits)
    };
merge_payment_terms(ProviderTerms, TerminalTerms) ->
    pm_utils:select_defined(TerminalTerms, ProviderTerms).

merge_wallet_terms(
    #domain_WalletProvisionTerms{
        turnover_limit = PLimit,
        withdrawals = PWithdrawal
    },
    #domain_WalletProvisionTerms{
        turnover_limit = TLimit,
        withdrawals = TWithdrawal
    }
) ->
    #domain_WalletProvisionTerms{
        turnover_limit = pm_utils:select_defined(TLimit, PLimit),
        withdrawals = merge_withdrawal_terms(PWithdrawal, TWithdrawal)
    };
merge_wallet_terms(ProviderTerms, TerminalTerms) ->
    pm_utils:select_defined(TerminalTerms, ProviderTerms).

merge_withdrawal_terms(
    #domain_WithdrawalProvisionTerms{
        allow = PAllow,
        currencies = PCurrencies,
        payout_methods = PMethods,
        cash_limit = PLimit,
        cash_flow = PCashflow,
        turnover_limit = PTurnoverLimit
    },
    #domain_WithdrawalProvisionTerms{
        allow = TAllow,
        currencies = TCurrencies,
        payout_methods = TMethods,
        cash_limit = TLimit,
        cash_flow = TCashflow,
        turnover_limit = TTurnoverLimit
    }
) ->
    #domain_WithdrawalProvisionTerms{
        allow = pm_utils:select_defined(TAllow, PAllow),
        currencies = pm_utils:select_defined(TCurrencies, PCurrencies),
        payout_methods = pm_utils:select_defined(TMethods, PMethods),
        cash_limit = pm_utils:select_defined(TLimit, PLimit),
        cash_flow = pm_utils:select_defined(TCashflow, PCashflow),
        turnover_limit = pm_utils:select_defined(TTurnoverLimit, PTurnoverLimit)
    };
merge_withdrawal_terms(ProviderTerms, TerminalTerms) ->
    pm_utils:select_defined(TerminalTerms, ProviderTerms).

reduce_if_defined(Selector, VS, Rev) ->
    pm_maybe:apply(fun(X) -> pm_selector:reduce(X, VS, Rev) end, Selector).

reduce_predicate_if_defined(Predicate, VS, Rev) ->
    pm_maybe:apply(fun(X) -> pm_selector:reduce_predicate(X, VS, Rev) end, Predicate).

-spec compute_proxy(provider(), terminal(), domain_revision()) ->
    dmsl_domain_thrift:'ProxyDefinition'().
compute_proxy(Provider, Terminal, DomainRevision) ->
    Proxy = Provider#domain_Provider.proxy,
    ProxyDef = pm_domain:get(DomainRevision, {proxy, Proxy#domain_Proxy.ref}),
    EffectiveOptions = lists:foldl(
        fun
            (undefined, M) ->
                M;
            (M1, M) ->
                maps:merge(M1, M)
        end,
        #{},
        [
            Terminal#domain_Terminal.options,
            Proxy#domain_Proxy.additional,
            ProxyDef#domain_ProxyDefinition.options
        ]
    ),
    ProxyDef#domain_ProxyDefinition{
        options = EffectiveOptions
    }.
