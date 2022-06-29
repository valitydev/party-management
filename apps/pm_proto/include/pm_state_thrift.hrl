-ifndef(pm_state_thrift_included__).
-define(pm_state_thrift_included__, yeah).

%% YOU MIGHT ALSO CONDIDER INCLUDING "dmsl_base_thrift.hrl"
%% YOU MIGHT ALSO CONDIDER INCLUDING "dmsl_domain_thrift.hrl"
%% YOU MIGHT ALSO CONDIDER INCLUDING "dmsl_payproc_thrift.hrl"



%% struct 'State'
-record('state_State', {
    'party' :: dmsl_domain_thrift:'Party'() | undefined,
    'timestamp' :: dmsl_base_thrift:'Timestamp'() | undefined,
    'claims' = #{} :: #{dmsl_payproc_thrift:'ClaimID'() => dmsl_payproc_thrift:'Claim'()} | undefined,
    'meta' = #{} :: dmsl_domain_thrift:'PartyMeta'() | undefined,
    'last_event' = 0 :: dmsl_base_thrift:'EventID'() | undefined
}).

-endif.
