-module(decker).

-export([start/0]).

-export([list_containers/0, list_containers/1,
         inspect_container/1, container_changes/1,
         create_container/1, export_container/2,
         start_container/1, stop_container/1, stop_container/2,
         restart_container/2,
         kill_container/1, attach_container/2,
         wait_container/1, remove_container/2]).

-export([list_images/1, create_image/1,
         insert_image_file/3, inspect_image/1,
         image_history/1, push_image/1, push_image/2,
         tag_image/2, remove_image/1, search_images/1]).

-export([build/1, build/2,
         get_auth/0, set_auth/3,
         info/0, version/0,
         commit/1]).

-compile(native).

-include("decker.hrl").

-define(PORT, "4243").

-type id() :: string().
-type error() :: {error, Reason::term()}.

%% ----------------------------------------------------------------------------

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(lhttpc).

%% ----------------------------------------------------------------------------

-spec list_containers(Opts::[all |
                             {limit, Id::id()} |
                             {since, Id::id()} |
                             {before, Id::id()}] | []) -> [#container_info{}] |
                                                          error().
list_containers(Opts) ->
    case call("/containers/json", Opts) of
        {ok, Json} ->
            {array, Jsons} = mochijson:decode(Json),
            [ decker_data:parse_container_info(I) || I <- Jsons ];
        Other ->
            Other
    end.

list_containers() ->
    list_containers([]).

-spec inspect_container(Id::id()) -> #container{} | error().
inspect_container(Id) ->
    case call("/containers/"++Id++"/json") of
        {ok, Json} ->
            decker_data:parse_container(mochijson:decode(Json));
        Other ->
            Other
    end.

-spec container_changes(Id::id()) -> [#container_change{}] | error().
container_changes(Id) ->
    case call("/containers/"++Id++"/changes") of
        {ok, Json} ->
            {array, Changes} = mochijson:decode(Json),
            [ #container_change{path=P, kind=K} ||
              {struct, [{_,P},{_,K}]} <- Changes ];
        Other ->
            Other
    end.

-spec create_container(#container_config{}) ->
    {ok, Id::id()} | {warn, Id::id(), Warnings::[string()]} | error().
create_container(#container_config{} = Config) ->
    JsonConfig = decker_data:to_json(Config),
    case call(post, "/containers/create", [], JsonConfig, 10000) of
        {ok, Json} ->
            case mochijson:decode(Json) of
                {struct, [{_,Id},{_,null}]} ->
                    {ok, Id};
                {struct, [{_,Id},{_,Warnings}]} ->
                    {warn, [{id, Id}, {warnings, Warnings}]}
            end;
        Other ->
            Other
    end.

%% TODO: Handle http_streaming
export_container(Id, _Filename) ->
    call(get, "/containers/"++Id++"/export", [], "", 60000).

-spec start_container(Id::id()) -> ok | error().
start_container(Id) ->
    case call(post, "/containers/"++Id++"/start", [], "", 2000) of
        {ok, <<>>} ->
            ok;
        Other ->
            Other
    end.

stop_container(Id) ->
    case call(post, "/containers/"++Id++"/stop", [], "", 60000) of
        {ok, <<>>} ->
            ok;
        Other ->
            Other
    end.

stop_container(Id, Ts) ->
    case call(post, "/containers/"++Id++"/stop", [{t, Ts}], "", Ts*2+2000) of
        {ok, <<>>} ->
            ok;
        Other ->
            Other
    end.

restart_container(Id, Ts) ->
    case call(post, "/containers/"++Id++"/restart", [{t, Ts}], "", Ts+2000) of
        {ok, <<>>} ->
            ok;
        Other ->
            Other
    end.

kill_container(Id) ->
    case call(post, "/containers/"++Id++"/kill", [], "", 60000) of
        {ok, <<>>} ->
            ok;
        Other ->
            Other
    end.

%% TODO: Handle hijacking
attach_container(Id, Opts) ->
    call(post, "/containers/"++Id++"/attach", Opts, "", 5000).

wait_container(Id) ->
    case call(post, "/containers/"++Id++"/wait", [], "", _24h = 86400000) of
        {ok, Json} ->
            {struct, [{_, Code}]} = mochijson:decode(Json),
            {ok, {status_code, Code}};
        Other ->
            Other
    end.

remove_container(Id, RemoveVolumes) ->
    case call(delete, "/containers/"++Id, [{v, RemoveVolumes}], "", 5000) of
        {ok, <<>>} ->
            ok;
        Other ->
            Other
    end.

%% ----------------------------------------------------------------------------

list_images(Opts) ->
    case call("/images/json", Opts) of
        {ok, Json} ->
            {array, Images} = mochijson:decode(Json),
            [ decker_data:parse_image_info(I) || I <- Images ];
        Other ->
            Other
    end.

create_image(Opts) ->
    call(post, "/images/create", Opts, "", 60000).

insert_image_file(Id, Url, Path) ->
    call(post, "/images/"++Id++"/insert", [{url,Url},{path,Path}], "", 60000).

inspect_image(Id) ->
    case call("/images/"++Id++"/json") of
        {ok, Json} ->
            decker_data:parse_image(mochijson:decode(Json));
        Other ->
            Other
    end.

image_history(Id) ->
    case call("/images/"++Id++"/history") of
        {ok, Json} ->
            {array, Changes} = mochijson:decode(Json),
            [ decker_data:parse_image_change(C) || C <- Changes ];
        Other ->
            Other
    end.

push_image(Id) ->
    call(post, "/images/"++Id++"/push", [], "", 60000).

push_image(Id, Registry) ->
    call(post, "/images/"++Id++"/push", [{registry, Registry}], "", 60000).

tag_image(Id, Opts) ->
    call(post, "/images/"++Id++"/tag", Opts, "", 2*60000).

remove_image(Id) ->
    call(delete, "/images/"++Id, [], "", 60000).

search_images(SearchTerm) ->
    call("/images/search", [{term, SearchTerm}]).

%% ----------------------------------------------------------------------------

build(BuildCommands) ->
    call(post, "/build", [], BuildCommands, 60000).

%% TODO: Stream the result
build(BuildCommands, Tag) ->
    call(post, "/build", [{t, Tag}], BuildCommands, 60000).

get_auth() ->
    call("/auth").

set_auth(_User, _Password, _Email) ->
    call(post, "/auth", [], "", 5000).

info() ->
    case call("/info") of
        {ok, Json} ->
            decker_data:parse_info(mochijson:decode(Json));
        Other ->
            Other
    end.

version() ->
    case call("/version") of
        {ok, Json} ->
            decker_data:parse_version(mochijson:decode(Json));
        Other ->
            Other
    end.

commit(Opts) ->
    call(post, "/commit", Opts, "", 20000).

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

call(Path) ->
    call(get, Path, [], "", 2000).

call(Path, Opts) ->
    call(get, Path, Opts, "", 10000).

call(Cmd, Path, Opts, SendBody, Timeout) ->
    Url = lists:append(["http://localhost:", ?PORT, Path, join_opts(Opts)]),
    case lhttpc:request(Url, Cmd, [], SendBody, Timeout) of
        {ok, {{Code,_},_,Body}} when Code >= 200, Code < 210 ->
            {ok, Body};
        {ok, {{Code,_},_,Body}} ->
            {error, {Code, Body}};
        {error, Error} ->
            {error, Error}
    end.

join_opts(Opts) ->
    Qs = proplists:unfold(Opts),
    QsStr = [ to_string(K)++"="++to_string(V) || {K, V} <- Qs ],
    case string:join(QsStr, "&") of
        [] ->
            "";
        Str ->
            [$?|Str]
    end.

to_string(Key) when is_list(Key) -> Key;
to_string(Key) when is_atom(Key) -> atom_to_list(Key);
to_string(Key) when is_binary(Key) -> binary_to_list(Key);
to_string(Key) when is_integer(Key) -> integer_to_list(Key);
to_string(Key) when is_float(Key) -> float_to_list(Key).
