{   
    application, server,
    [   
        {description, "This is sd game server."},   
        {vsn, "1.0a"},   
        {modules,[en]},   
        {registered, [boot_entrance]},
        {applications, [kernel, stdlib, sasl]},   
        {mod, {boot_entrance, []}},
        {start_phases, []},
        {env,[
                %% ticket
                {ticket, "SDFSDESF123DFSDF"},
                %% 数据库相关设置
                {db_host, "localhost"},
                {db_port, 3306},
                {db_user, "root"},
                {db_pass, "123456"},
                {db_name, "sdzmmo"},
                {db_encode, utf8},
                {facebook, false},
                {fcm_min_value, 3},
                {fcm_max_value, 5},
                %% 新手卡
                {card_key, "sZ5Y9EejuXRwMynE"},
                {server, "T1"},
                {hf_server, ["T1","T2"]},
                %% log_level
                {log_level, 5},
                %% log_path
                {log_path, "sd_alarm.log"}
           ]
        }
    ]
}. 