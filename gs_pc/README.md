gs_pc
=====

An OTP application running the GS server.
Recommended to run after GUI is already running.
Required to update gs_args.args file with current gui node name prior to running.

Build
-----
    cd gs_pc
    mkdir ebin
    erlc -o ebin src/*.erl
    cp src/gs_pc.app.src ebin/gs_pc.app

Run
-----
    echo "-gui_node  gui@<GUI_PC_local_ip>" > gs_args.args
    
    erl -pa ebin/ -name gs<Number>@<localIP> -setcookie cookie -args_file gs_args.args
    
    $ application:ensure_all_started(gs_pc).