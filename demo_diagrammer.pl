/*  Part of SWI-Prolog

    Author:        Jan Wielemaker, Anne Ogborn
    E-mail:        aogborn@uh.edu
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(demo_diagrammer,
	  [ server/0,
	    server/1
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(debug)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).

:- use_module(chatroom).

:- debug(websocket).

/** <module> Demo of scalable websocket based diagrammer server in SWI-Prolog

Logic languages have a natural affinity to problems best expressed
by 'ball and circle' graphs or 'arrows between boxes' diagrams.

This is a small shared diagrammer application for collaboratively
drawing digraph diagrams. It's based on Jan Wielemaker's =|chatroom.pl|=
demo available at https://github.com/JanWielemaker/swi-chat  .

 @tbd  Write this up after you know where the API is going

  - Create a diagrammerroom using chatroom_create/3 and a thread that
    listens to diagrammer events and broadcasts the changes.

  - Serve a web page that provides the diagrammer frontend.  The frontend
    contains JavaScript that establishes a websocket on /chat.  If
    a websocket is obtained, hand it to to the room using
    chatroom_add/2
*/

% be a bit chatty.  Comment for silent operation.
:- debug(chat).
:- debug(diagrammer).

%%	server is det.
%%	server(?Port) is det.
%
%	Create the chat room and start the   server. The default port is
%	3080.

server :-
	server(3080).

server(Port) :-
	(   debugging(chat)
	->  prolog_ide(thread_monitor)
	;   true
	),
	create_chat_room,
	http_server(http_dispatch, [port(Port)]),
	format(user_error, 'Started server at http://localhost:~d/~n', [Port]).

http:location(img, root(img), []).
http:location(js, root(js), []).
http:location(css, root(css), []).
user:file_search_path(img, './img').
user:file_search_path(js, './js').
user:file_search_path(css, './css').

:- html_resource(js('diagrammer.js'),[requires(js('jquery-2.0.3.min.js'))]).

% setup the HTTP location. The  first   (/)  loads  the application. The
% loaded application will create  a   websocket  using  /chat. Normally,
% http_upgrade_to_websocket/3 runs call(Goal, WebSocket)  and closes the
% connection if Goal terminates. Here, we use guarded(false) to tell the
% server we will take responsibility for the websocket.

:- http_handler(root(.),    diagrammer_page,      []).
:- http_handler(root(chat),
		http_upgrade_to_websocket(
		    accept_chat,
		    [ guarded(false),
		      subprotocols([echo])
		    ]),
		[ id(chat_websocket)
		]).

:- http_handler(root(img),
		serve_files_in_directory(img), [prefix]).
:- http_handler(root(js),
		serve_files_in_directory(js), [prefix]).
:- http_handler(root(css),
		serve_files_in_directory(css), [prefix]).

diagrammer_page(_Request) :-
	reply_html_page(
	    title('Collaborative Diagram Editor'),
	    \diagrammer_body).

%%	diagrammer_body//
%
%	Generate the web page. To  keep   everything  compact  we do the
%	styling inline.

diagrammer_body -->
	{
           http_absolute_location(img('rect.png'), RectLoc, []),
	   http_absolute_location(img('oval.png'), OvalLoc, []),
           http_absolute_location(img('diamond.png'), DiamondLoc, [])
        },
	html_requires(css('diagrammer.css')),
	html_requires(js('jquery-2.0.3.min.js')),
	html([ h1('A Collaborative Diagram Editor'),
	       div(id(diagrammer), [
		   div([class(componentbar)], [
			   img([id(rect_tool), src(RectLoc)],[]),
			   img([class(selected), id(oval_tool), src(OvalLoc)]),
			   img([id(diamond_tool), src(DiamondLoc)])
		       ]),
		   canvas([class(drawarea),
			   width('1000'), height('612')], [])
		       % http://stackoverflow.com/questions/17034795/html-canvas-scale
		   ]),
	       p(id(msg), 'message area'),
	       p(id(output), 'output area')
	     ]),
	script.

%%	script//
%
%	Generate the JavaScript  that  establishes   the  websocket  and
%	handles events on the websocket.

script -->
	html_requires(js('diagrammer.js')),
	{ http_link_to_id(chat_websocket, [], WebSocketURL)
	},
	js_script({|javascript(WebSocketURL)||
$(document).ready(function() {
    ws_initialize(WebSocketURL);
});
		  |}).

%%	accept_chat(+WebSocket) is det.
%
%	Normally,  the  goal  called    by   http_upgrade_to_websocket/3
%	processes all communication with the   websocket in a read/write
%	loop. In this case however,  we tell http_upgrade_to_websocket/3
%	that we will take responsibility for   the websocket and we hand
%	it to the chat room.

accept_chat(WebSocket) :-
	chatroom_add(chat, WebSocket, _Id).

%%	create_chat_room
%
%	Create our actual chat room.

:- dynamic
	utterance/1,			% messages
	visitor/1.			% joined visitors

create_chat_room :-
	chatroom_create(chat, Room, _{}),
	thread_create(chatroom(Room), _, [alias(chatroom)]).

%%	chatroom(+Room)
%
%	Realise the chatroom main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

chatroom(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	chatroom(Room).

handle_message(Message, Room) :-
	websocket{opcode:text, data:String} :< Message, !,
	read_term_from_atom(String, Term, []),
	term_to_json(Term, JSON),
	debug(diagrammer, 'JSON ~w', [JSON]),
	atom_json_term(TextJSON, JSON, [as(atom)]),
	debug(diagrammer, 'TextJSON ~w', [TextJSON]),
	assertz(utterance(TextJSON)),
	chatroom_broadcast(Room.name, Message.put(data, TextJSON)).
handle_message(Message, _Room) :-
	chatroom{joined:Id} :< Message, !,
	assertz(visitor(Id)),
	forall(utterance(Utterance),
	       chatroom_send(Id, text(Utterance))).
handle_message(Message, _Room) :-
	chatroom{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).
