# SWI-Prolog based shared diagram server

This repository provides a demonstration for   using  the recently added
websocket support for realising a  chat  server.   To  use  it, you must
install *[SWI-Prolog](http://www.swi-prolog.org) 7.1.29 or later*. Then,
you can load `demo_diagrammer.pl` and run

    ?- server.

This will start the  server  at  port   3080  in  debug  mode, showing a
graphical window with the running server threads and debug messages in
the console. 

This advice is probably bogus, I have retained it because it's probably easy to get it working,
but I haven't worked on it

## Running as a service

The script `daemon.pl` is provided to start  the server as a Unix daemon
process. Run `./daemon.pl --help` for a  brief help message.

### Starting as an upstart job

The service can be  started  as  an   (Ubuntu)  upstart  job  by copying
`upstart/chat.conf`  to  `/etc/init`  editing  `/etc/init/chat.conf`  to
reflect the installation directory. After  that,   run  this  command to
start the server:

    % sudo service chat start

## Status

Minimally functional, pre alpha, at the moment this is mostly a way of poking at websockets.   
Tested with the server running
on Linux using firefox and chromium as clients. Anne Ogborn confirmed it
also works using Windows 7 as server and IE 11.0 as client.

## Future

The library hub.pl is probably going to end up as a core library and the
demo program will than be added as a demo application.


