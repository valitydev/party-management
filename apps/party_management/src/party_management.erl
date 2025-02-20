%%% @doc Public API, supervisor and application startup.
%%% @end

-module(party_management).

-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

% 30 seconds
-define(DEFAULT_HANDLING_TIMEOUT, 30000).

%%
%% API
%%
-spec start() -> {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Options = application:get_env(?MODULE, cache_options, #{}),
    {ok,
        {
            #{strategy => one_for_all, intensity => 6, period => 30},
            [
                pm_party_cache:cache_child_spec(party_cache, Options),
                get_api_child_spec(Options)
            ]
        }}.

get_api_child_spec(Opts) ->
    {ok, Ip} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    HealthRoutes = construct_health_routes(genlib_app:env(?MODULE, health_check, #{})),
    EventHandlerOpts = genlib_app:env(?MODULE, scoper_event_handler_options, #{}),
    PrometeusRoute = get_prometheus_route(),

    woody_server:child_spec(
        ?MODULE,
        #{
            ip => Ip,
            port => genlib_app:env(?MODULE, port, 8022),
            transport_opts => genlib_app:env(?MODULE, transport_opts, #{}),
            protocol_opts => genlib_app:env(?MODULE, protocol_opts, #{}),
            event_handler => {pm_woody_event_handler, EventHandlerOpts},
            handlers =>
                [
                    construct_service_handler(claim_committer, pm_claim_committer_handler, Opts),
                    construct_service_handler(party_management, pm_party_handler, Opts)
                ],
            additional_routes => setup_machinery_routes() ++ [PrometeusRoute | HealthRoutes],
            shutdown_timeout => genlib_app:env(?MODULE, shutdown_timeout, 0)
        }
    ).

setup_machinery_routes() ->
    Schema = party_management_machinery_schema,
    Backend = construct_machinery_backend_spec(Schema),
    ok = application:set_env(?MODULE, backends, #{pm_party_machine:namespace() => Backend}),

    MachineHandler = construct_machinery_handler_spec(pm_party_machine, Schema),
    ModernizerHandler = construct_machinery_modernizer_spec(Schema),

    RouteOptsEnv = genlib_app:env(?MODULE, route_opts, #{}),
    EventHandlerOpts = genlib_app:env(?MODULE, scoper_event_handler_options, #{}),
    RouteOpts = RouteOptsEnv#{event_handler => {scoper_woody_event_handler, EventHandlerOpts}},

    machinery_mg_backend:get_routes([MachineHandler], RouteOpts) ++
        machinery_modernizer_mg_backend:get_routes([ModernizerHandler], RouteOpts).

construct_machinery_backend_spec(Schema) ->
    {machinery_mg_backend, #{
        schema => Schema,
        client => get_service_client(automaton)
    }}.

construct_machinery_handler_spec(Handler, Schema) ->
    {Handler, #{
        path => "/v1/stateproc/party",
        backend_config => #{schema => Schema}
    }}.

construct_machinery_modernizer_spec(Schema) ->
    #{
        path => "/v1/modernizer",
        backend_config => #{schema => Schema}
    }.

get_service_client(ServiceName) ->
    case get_service_client_url(ServiceName) of
        undefined ->
            error({unknown_service, ServiceName});
        Url ->
            genlib_map:compact(#{
                url => Url,
                event_handler => genlib_app:env(party_management, woody_event_handlers, [
                    {scoper_woody_event_handler, #{}}
                ])
            })
    end.

get_service_client_url(ServiceName) ->
    ServiceClients = genlib_app:env(party_management, services, #{}),
    maps:get(ServiceName, ServiceClients, undefined).

construct_health_routes(Check) ->
    [erl_health_handle:get_route(enable_health_logging(Check))].

enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

construct_service_handler(Name, Module, Opts) ->
    {Path, Service} = pm_proto:get_service_spec(Name),
    {Path, {Service, {pm_woody_wrapper, maps:merge(#{handler => Module}, Opts)}}}.

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.

%% Application callbacks

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    ok = setup_metrics(),
    supervisor:start_link(?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%

setup_metrics() ->
    ok = woody_ranch_prometheus_collector:setup(),
    ok = woody_hackney_prometheus_collector:setup().
