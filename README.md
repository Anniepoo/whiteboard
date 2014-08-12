# SWI-Prolog based shared whiteboard

Based on Jan Wielemaker's swi-Prolog chat server

https://github.com/JanWielemaker/swi-chat

This repository provides a minimal whiteboard drawing solution.   
To  use  it, you must
install *[SWI-Prolog](http://www.swi-prolog.org) 7.1.19 or later*. Then,
you can load `demo_whiteboard.pl` and run

    ?- server.

This will start the server at port 3080.

## Status



## chatroom.pl Status

The library chatroom.pl is probably going to   end  up as a SWI-Prolog core library 
(per Jan), so I'm trying to avoid modifying it.


