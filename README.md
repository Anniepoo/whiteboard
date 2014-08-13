# SWI-Prolog based shared diagram editor

Based on Jan Wielemaker's swi-Prolog chat server

https://github.com/JanWielemaker/swi-chat

This repository provides bare bones functionality for drawing arcs and nodes type diagrams.
 
To  use  it, you must
install *[SWI-Prolog](http://www.swi-prolog.org) 7.1.19 or later*. Then,
you can load `demo_diagrammer.pl` and run

    ?- server.

This will start the server at port 3080.

## Status

Minimally functional, pre alpha, at the moment this is mostly a way of poking at websockets.

## chatroom.pl Status

The library chatroom.pl is probably going to   end  up as a SWI-Prolog core library 
(per Jan), so I'm trying to avoid modifying it.


