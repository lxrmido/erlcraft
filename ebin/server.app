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
                {ja_action, <<"/api/subscribe?source=console&key=dced10a4a6be5ccc82a21086fac34b9279054edb708b9da31cdedb710d6ee918\n">>},
                {ja_host, "mc.bilicraft.com"},
                {ja_port, 25570}
           ]
        }
    ]
}. 