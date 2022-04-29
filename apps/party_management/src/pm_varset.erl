-module(pm_varset).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([encode_varset/1]).
-export([decode_varset/2]).
-export([decode_varset/1]).

-export_type([varset/0]).

-type varset() :: #{
    category => dmsl_domain_thrift:'CategoryRef'(),
    currency => dmsl_domain_thrift:'CurrencyRef'(),
    cost => dmsl_domain_thrift:'Cash'(),
    payment_method => dmsl_domain_thrift:'PaymentMethodRef'(),
    payout_method => dmsl_domain_thrift:'PayoutMethodRef'(),
    wallet_id => dmsl_domain_thrift:'WalletID'(),
    shop_id => dmsl_domain_thrift:'ShopID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'(),
    payment_tool => dmsl_domain_thrift:'PaymentTool'(),
    party_id => dmsl_domain_thrift:'PartyID'(),
    bin_data => dmsl_domain_thrift:'BinData'()
}.

-type encoded_varset() :: dmsl_payment_processing_thrift:'Varset'().
-type contract_terms_varset() :: dmsl_payment_processing_thrift:'ComputeContractTermsVarset'().
-type shop_terms_varset() :: dmsl_payment_processing_thrift:'ComputeShopTermsVarset'().

-spec encode_varset(varset()) -> encoded_varset().
encode_varset(Varset) ->
    #payproc_Varset{
        category = genlib_map:get(category, Varset),
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        payment_method = genlib_map:get(payment_method, Varset),
        payout_method = genlib_map:get(payout_method, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        shop_id = genlib_map:get(shop_id, Varset),
        identification_level = genlib_map:get(identification_level, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset),
        party_id = genlib_map:get(party_id, Varset),
        bin_data = genlib_map:get(bin_data, Varset)
    }.

-spec decode_varset(encoded_varset()) -> varset().
decode_varset(Varset) ->
    decode_varset(Varset, #{}).
-spec decode_varset(encoded_varset() | contract_terms_varset() | shop_terms_varset(), varset()) -> varset().
decode_varset(#payproc_Varset{} = Varset, VS) ->
    genlib_map:compact(VS#{
        category => Varset#payproc_Varset.category,
        currency => Varset#payproc_Varset.currency,
        cost => Varset#payproc_Varset.amount,
        payment_method => Varset#payproc_Varset.payment_method,
        payout_method => Varset#payproc_Varset.payout_method,
        wallet_id => Varset#payproc_Varset.wallet_id,
        shop_id => Varset#payproc_Varset.shop_id,
        identification_level => Varset#payproc_Varset.identification_level,
        payment_tool => prepare_payment_tool_var(
            Varset#payproc_Varset.payment_method,
            Varset#payproc_Varset.payment_tool
        ),
        party_id => Varset#payproc_Varset.party_id,
        bin_data => Varset#payproc_Varset.bin_data
    });
decode_varset(#payproc_ComputeShopTermsVarset{} = Varset, VS) ->
    genlib_map:compact(VS#{
        cost => Varset#payproc_ComputeShopTermsVarset.amount,
        payout_method => Varset#payproc_ComputeShopTermsVarset.payout_method,
        payment_tool => Varset#payproc_ComputeShopTermsVarset.payment_tool
    });
decode_varset(#payproc_ComputeContractTermsVarset{} = Varset, VS) ->
    genlib_map:compact(VS#{
        currency => Varset#payproc_ComputeContractTermsVarset.currency,
        cost => Varset#payproc_ComputeContractTermsVarset.amount,
        shop_id => Varset#payproc_ComputeContractTermsVarset.shop_id,
        payout_method => Varset#payproc_ComputeContractTermsVarset.payout_method,
        payment_tool => Varset#payproc_ComputeContractTermsVarset.payment_tool,
        wallet_id => Varset#payproc_ComputeContractTermsVarset.wallet_id,
        bin_data => Varset#payproc_ComputeContractTermsVarset.bin_data
    }).

prepare_payment_tool_var(_PaymentMethodRef, PaymentTool) when PaymentTool /= undefined ->
    PaymentTool;
prepare_payment_tool_var(PaymentMethodRef = #domain_PaymentMethodRef{}, _PaymentTool) ->
    pm_payment_tool:create_from_method(PaymentMethodRef);
prepare_payment_tool_var(undefined, undefined) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec encode_decode_test() -> _.
encode_decode_test() ->
    Varset = #{
        category => #domain_CategoryRef{id = 1},
        currency => #domain_CurrencyRef{symbolic_code = <<"RUB">>},
        cost => #domain_Cash{
            amount = 20,
            currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
        },
        payment_method => #domain_PaymentMethodRef{
            id = {bank_card, #domain_BankCardPaymentMethod{
                payment_system = #domain_PaymentSystemRef{id = <<"visa">>}
            }}
        },
        payout_method => #domain_PayoutMethodRef{id = any},
        wallet_id => <<"wallet_id">>,
        shop_id => <<"shop_id">>,
        identification_level => full,
        payment_tool =>
            {digital_wallet, #domain_DigitalWallet{
                payment_service = #domain_PaymentServiceRef{id = <<"qiwi">>},
                id = <<"digital_wallet_id">>
            }},
        party_id => <<"party_id">>,
        bin_data => #domain_BinData{
            payment_system = <<"payment_system">>,
            bank_name = <<"bank_name">>
        }
    },
    Varset = decode_varset(encode_varset(Varset)).

-endif.
