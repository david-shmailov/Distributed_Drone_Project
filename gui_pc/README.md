gui_pc
=====

An OTP application running the GUI server and starting the python pyqt gui.
Recommended to run prior to the GS applications

Build
-----
    cd gui_pc
    mkdir ebin
    erlc -o ebin src/*.erl
    cp src/gui_pc.app.src ebin/gui_pc.app

Run
-----
    erl -pa ebin/ -name gui@<GUI_PC_local_ip> -setcookie cookie
    
    $ application:ensure_all_started(gui_pc).