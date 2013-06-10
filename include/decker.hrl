%% edocker record definitions
%%

-type timestamp() :: {{integer(), integer(), integer()},
                      {integer(), integer(), float()}}.

-record(container_info, {id :: string(),
                         image :: string(),
                         command :: string(),
                         created :: timestamp(),
                         status :: string()}).

-record(network_settings, {ip_address :: string(),
                           ip_prefix_len :: integer(),
                           gateway :: string(),
                           bridge :: string(),
                           port_mapping :: [{integer(), integer()}]}).

-record(state, {running :: boolean(),
                pid :: integer(),
                exit_code :: integer(),
                started_at :: timestamp(),
                ghost :: boolean()}).

-record(container_config, {hostname="" :: string(),
                           user="" :: string(),
                           memory=0 :: integer(),
                           memory_swap=0 :: integer(),
                           attach_stdin=false :: boolean(),
                           attach_stdout=true :: boolean(),
                           attach_stderr=true :: boolean(),
                           port_specs=[] :: [integer()],
                           tty=false :: boolean(),
                           open_stdin=false :: boolean(),
                           stdin_once=false :: boolean(),
                           env=[] :: [{string(), string()}],
                           cmd="" :: string(),
                           dns="" :: string(),
                           image="base" :: string(),
                           volumes=[] :: [{string(), string()}],
                           volumes_from="" :: string()}).

-record(container, {id :: string(),
                    created :: timestamp(),
                    path :: string(),
                    args :: [string()],
                    config :: #container_config{},
                    state :: #state{},
                    image :: string(),
                    network_settings :: #network_settings{},
                    sys_init_path :: string(),
                    resolv_conf_path :: string(),
                    volumes :: [{string(), string()}]}).

-record(image_info, {repository :: string(),
                     tag :: string(),
                     id :: string(),
                     created :: timestamp()}).

-record(image, {id :: string(),
                parent :: string(),
                created :: timestamp(),
                container :: string(),
                config :: #container_config{}}).

-record(info, {containers :: integer(),
               images :: integer(),
               debug :: boolean(),
               num_fd :: integer(),
               num_goroutines :: integer(),
               memory_limit :: boolean(),
               swap_limit :: boolean()}).

-record(version, {version :: string(),
                  git_commit :: string(),
                  go_version :: string()}).

-record(change, {path :: string(), kind :: string()}).
