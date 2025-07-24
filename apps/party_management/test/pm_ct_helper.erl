-module(pm_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([cfg/2]).

-export([create_client/0]).
-export([create_client/1]).

-export_type([config/0]).
-export_type([test_case_name/0]).
-export_type([group_name/0]).

%%

-type app_name() :: atom().

-spec start_app(app_name()) -> {[app_name()], map()}.
start_app(scoper = AppName) ->
    {
        start_app(AppName, [
            {storage, scoper_storage_logger}
        ]),
        #{}
    };
start_app(woody = AppName) ->
    {
        start_app(AppName, [
            {acceptors_pool_size, 4}
        ]),
        #{}
    };
start_app(dmt_client = AppName) ->
    {
        start_app(AppName, [
            % milliseconds
            {cache_update_interval, 5000},
            {max_cache_size, #{
                elements => 20,
                % 50Mb
                memory => 52428800
            }},
            {woody_event_handlers, [
                {scoper_woody_event_handler, #{
                    event_handler_opts => #{
                        formatter_opts => #{
                            max_length => 1000
                        }
                    }
                }}
            ]},
            {service_urls, #{
                'Repository' => <<"http://dmt:8022/v1/domain/repository">>,
                'RepositoryClient' => <<"http://dmt:8022/v1/domain/repository_client">>,
                'AuthorManagement' => <<"http://dmt:8022/v1/domain/author">>
            }}
        ]),
        #{}
    };
start_app(party_management = AppName) ->
    {
        start_app(AppName, [
            {scoper_event_handler_options, #{
                event_handler_opts => #{
                    formatter_opts => #{
                        max_length => 1000
                    }
                }
            }},
            {machinery_backend, hybrid},
            {services, #{
                accounter => <<"http://shumway:8022/accounter">>,
                automaton => <<"http://machinegun:8022/v1/automaton">>,
                party_management => #{
                    url => <<"http://party-management:8022/v1/processing/partymgmt">>,
                    transport_opts => #{
                        pool => party_management,
                        max_connections => 300
                    }
                },
                claim_committer => #{
                    url => <<"http://party-management:8022/v1/processing/claim_committer">>,
                    transport_opts => #{
                        pool => claim_committer,
                        max_connections => 300
                    }
                }
            }}
        ]),
        #{}
    };
start_app(epg_connector = AppName) ->
    {
        start_app(AppName, [
            {databases, #{
                default_db => #{
                    host => "postgres",
                    port => 5432,
                    database => "progressor_db",
                    username => "progressor",
                    password => "progressor"
                }
            }},
            {pools, #{
                default_pool => #{
                    database => default_db,
                    size => 30
                }
            }}
        ]),
        #{}
    };
start_app(progressor = AppName) ->
    {
        start_app(AppName, [
            {call_wait_timeout, 20},
            {defaults, #{
                storage => #{
                    client => prg_pg_backend,
                    options => #{
                        pool => default_pool
                    }
                },
                retry_policy => #{
                    initial_timeout => 5,
                    backoff_coefficient => 1.0,
                    %% seconds
                    max_timeout => 180,
                    max_attempts => 3,
                    non_retryable_errors => []
                },
                task_scan_timeout => 1,
                worker_pool_size => 100,
                process_step_timeout => 30
            }},
            {namespaces, #{
                'party' => #{
                    processor => #{
                        client => machinery_prg_backend,
                        options => #{
                            namespace => 'party',
                            handler => {pm_party_machine, #{}},
                            schema => party_management_machinery_schema
                        }
                    }
                }
            }}
        ]),
        #{}
    };
start_app(AppName) ->
    {genlib_app:start_application(AppName), #{}}.

-spec start_app(app_name(), list()) -> [app_name()].
start_app(cowboy = AppName, Env) ->
    #{
        listener_ref := Ref,
        acceptors_count := Count,
        transport_opts := TransOpt,
        proto_opts := ProtoOpt
    } = Env,
    {ok, _} = cowboy:start_clear(Ref, [{num_acceptors, Count} | TransOpt], ProtoOpt),
    [AppName];
start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_apps([app_name() | {app_name(), list()}]) -> {[app_name()], map()}.
start_apps(Apps) ->
    lists:foldl(
        fun
            ({AppName, Env}, {AppsAcc, RetAcc}) ->
                {lists:reverse(start_app(AppName, Env)) ++ AppsAcc, RetAcc};
            (AppName, {AppsAcc, RetAcc}) ->
                {Apps0, Ret0} = start_app(AppName),
                {lists:reverse(Apps0) ++ AppsAcc, maps:merge(Ret0, RetAcc)}
        end,
        {[], #{}},
        Apps
    ).

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-spec cfg(atom(), config()) -> term().
cfg(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        _ -> undefined
    end.

%%

-spec create_client() -> pm_client_api:t().
create_client() ->
    create_client_w_context(woody_context:new()).

-spec create_client(woody:trace_id()) -> pm_client_api:t().
create_client(TraceID) ->
    create_client_w_context(woody_context:new(TraceID)).

create_client_w_context(WoodyCtx) ->
    pm_client_api:new(WoodyCtx).
