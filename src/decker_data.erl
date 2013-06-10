-module(decker_data).

-export([container_config_to_json/1]).

-export([parse_container_config/1, parse_container_info/1,
         parse_container/1, parse_info/1, parse_version/1,
         parse_image_info/1, parse_image/1,
         parse_container_change/1, parse_image_change/1]).

-include("decker.hrl").

%% ----------------------------------------------------------------------------

container_config_to_json(#container_config{} = Config) ->
    #container_config{hostname=Hostname,
                      user=User,
                      memory=Memory,
                      memory_swap=MemorySwap,
                      attach_stdin=AttachStdin,
                      attach_stdout=AttachStdout,
                      attach_stderr=AttachStderr,
                      port_specs=PortSpecs,
                      tty=Tty,
                      open_stdin=OpenStdin,
                      stdin_once=StdinOnce,
                      env=Env,
                      cmd=Cmds,
                      dns=Dns,
                      image=Image,
                      volumes=Volumes,
                      volumes_from=VolumesFrom} = Config,
    mochijson:encode({struct, [{"Hostname", Hostname},
                                {"user", User},
                                {"Memory", Memory},
                                {"MemorySwap", MemorySwap},
                                {"AttachStdin", AttachStdin},
                                {"AttachStdout", AttachStdout},
                                {"AttachStderr", AttachStderr},
                                {"PortSpecs", {array, PortSpecs}},
                                {"Tty", Tty},
                                {"OpenStdin", OpenStdin},
                                {"StdinOnce", StdinOnce},
                                {"Env", {array, [ {K, V} || {K, V} <- Env ]}},
                                {"Cmd", {array, Cmds}},
                                {"Dns", {array, [Dns]}},
                                {"Image", Image},
                                {"Volumes", {struct,
                                             [ To || {_From, To} <- Volumes ]}},
                                {"VolumesFrom", VolumesFrom}]}).

%% ----------------------------------------------------------------------------

parse_container_config({struct, Fields}) ->
    #container_config{hostname=prop("Hostname", Fields),
                      user=prop("User", Fields),
                      memory=prop("Memory", Fields),
                      memory_swap=prop("MemorySwap", Fields),
                      attach_stdin=prop("AttachStdin", Fields),
                      attach_stdout=prop("AttachStdout", Fields),
                      attach_stderr=prop("AttachStderr", Fields),
                      port_specs=prop("PortSpecs", Fields),
                      tty=prop("Tty", Fields),
                      open_stdin=prop("OpenStdin", Fields),
                      stdin_once=prop("StdinOnce", Fields),
                      env=prop("Env", Fields),
                      cmd=prop("Cmd", Fields),
                      dns=prop("Dns", Fields),
                      image=prop("Image", Fields),
                      volumes=prop("Volumes", Fields),
                      volumes_from=prop("VolumesFrom", Fields)}.

parse_container_info({struct, Fields}) ->
    #container_info{id=prop("Id", Fields),
                    image=prop("Image", Fields),
                    command=prop("Command", Fields),
                    created=prop("Created", Fields),
                    status= prop("Status", Fields)}.

parse_network_settings({struct, Fields}) ->
    #network_settings{ip_address=prop("IpAddress", Fields),
                      ip_prefix_len=prop("IpPrefixLen", Fields),
                      gateway=prop("Gateway", Fields),
                      bridge=prop("Bridge", Fields),
                      port_mapping=prop("PortMapping", Fields)}.

parse_state({struct, Fields}) ->
    #state{running=prop("Running", Fields),
           pid=prop("Pid", Fields),
           exit_code=prop("ExitCode", Fields),
           started_at=prop("StartedAt", Fields),
           ghost=prop("Ghost", Fields)}.

parse_container({struct, Fields}) ->
    NetworkSettings = parse_network_settings(prop("NetworkSettings", Fields)),
    #container{id=prop("Id", Fields),
               created=prop("Created", Fields),
               path=prop("Path", Fields),
               args=prop("Args", Fields),
               config=parse_container_config(prop("Config", Fields)),
               state=parse_state(prop("State", Fields)),
               image=prop("Image", Fields),
               network_settings=NetworkSettings,
               sys_init_path=prop("SysInitPath", Fields),
               resolv_conf_path=prop("ResolvConfPath", Fields),
               volumes=prop("Volumes", Fields)}.

parse_image_info({struct, Fields}) ->
    #image_info{repository=prop("Repository", Fields),
                tag=prop("Tag", Fields),
                id=prop("Id", Fields),
                created=prop("Created", Fields)}.

parse_image({struct, Fields}) ->
    #image{id=prop("id", Fields),
           parent=prop("parent", Fields),
           created=prop("created", Fields),
           container=prop("container", Fields),
           config=parse_container_config(prop("container_config", Fields))}.

parse_info({struct, Fields}) ->
    #info{containers=prop("Containers", Fields),
          images=prop("Images", Fields),
          debug=prop("Debug", Fields),
          num_fds=prop("NFd", Fields),
          num_goroutines=prop("NGoroutines", Fields),
          memory_limit=prop("MemoryLimit", Fields),
          swap_limit=prop("SwapLimit", Fields)}.

parse_version({struct, Fields}) ->
    #version{version=prop("Version", Fields),
             git_commit=prop("GitCommit", Fields),
             memory_limit=prop("MemoryLimit", Fields),
             swap_limit=prop("SwapLimit", Fields)}.

parse_container_change({struct, Fields}) ->
    #container_change{path=prop("Path", Fields),
                      kind=prop("Kind", Fields)}.

parse_image_change({struct, Fields}) ->
    #image_change{id=prop("Id", Fields),
                  created=prop("Created", Fields),
                  created_by=prop("CreatedBy", Fields)}.

%% ----------------------------------------------------------------------------

prop(Key, Lst) ->
    case lists:keyfind(Key, 1, Lst) of
        false ->
            undefined;
        {Key, Value} ->
            Value
    end.
