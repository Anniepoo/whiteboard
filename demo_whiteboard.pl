/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
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

:- module(whiteboard_server,
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

:- use_module(chatroom).

:- debug(websocket).

/** <module> A scalable websocket based whiteboard server in SWI-Prolog

Logic languages have a natural affinity to problems best expressed
by 'ball and circle' graphs or 'arrows between boxes' diagrams.

This is a small shared whiteboard application for collaboratively
drawing digraph diagrams. It's based on Jan Wielemaker's =|chatroom.pl|=
demo available at https://github.com/JanWielemaker/swi-chat  .

 @tbd  Write this up after you know where the API is going

  - Create a whiteboardroom using chatroom_create/3 and a thread that
    listens to whiteboard events and broadcasts the changes.

  - Serve a web page that provides the whiteboard frontend.  The frontend
    contains JavaScript that establishes a websocket on /chat.  If
    a websocket is obtained, hand it to to the room using
    chatroom_add/2
*/

% be a bit chatty.  Comment for silent operation.
:- debug(chat).
:- debug(whiteboard).

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

% setup the HTTP location. The  first   (/)  loads  the application. The
% loaded application will create  a   websocket  using  /chat. Normally,
% http_upgrade_to_websocket/3 runs call(Goal, WebSocket)  and closes the
% connection if Goal terminates. Here, we use guarded(false) to tell the
% server we will take responsibility for the websocket.

:- http_handler(root(.),    whiteboard_page,      []).
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

whiteboard_page(_Request) :-
	reply_html_page(
	    \whiteboard_head,
	    \whiteboard_body).

whiteboard_head -->
	html([ title('Collaborative Diagram Editor'),
	       link([rel(stylesheet), href('/css/whiteboard.css')], [])
	     ]).

%%	whiteboard_page//
%
%	Generate the web page. To  keep   everything  compact  we do the
%	styling inline.

whiteboard_body -->
	html([ h1('A Collaborative Diagram Editor'),
	       div(id(whiteboard), [
		   div([class(componentbar)], [
			   img([class(selected), id(rect_tool), src('img/rect.png')],[]),
			   img([id(oval_tool), src('img/oval.png')]),
			   img([id(diamond_tool), src('img/diamond.png')])
		       ]),
		   canvas(class(drawarea), [])
		   ]),
	       p(id(msg), 'message area'),
	       p(id(output), 'output area'),
	       script(src('/js/jquery-2.0.3.min.js'), [])
	     ]),
	script.

%%	script//
%
%	Generate the JavaScript  that  establishes   the  websocket  and
%	handles events on the websocket.

script -->
	{ http_link_to_id(chat_websocket, [], WebSocketURL)
	},
	js_script({|javascript(WebSocketURL)||

$(document).ready(function() {

    var whiteboard = {
	pengine: undefined,

	currentTool: "rect",

	connection: undefined,

	writeln: function(string) {
		$('#output').append(string + "<br />")
	},

	unchoose_tools: function() {
		$("#whiteboard .componentbar IMG").removeClass("selected");
	},

	newElement: function(e) {
			$("#msg").text("down " + e.clientX + " " + e.clientY);
	},

	newElementMoveOrDrag: function(e) {
		if (mouseDownCount === 0)
			return;

		var x = e.offsetX;
		var y = e.offsetY;

		$("#msg").text("drag " + x + " " + y + " " + e.button);
	},

	newElementCommit: function(e) {
		$("#msg").text("commit " + e.clientX + " " + e.clientY);
		whiteboard.sendChat("commit(" + whiteboard.currentTool + ", " + e.clientX +
		                ", " + e.clientY + ")");
	},

	openWebSocket: function() {
	      connection = new WebSocket("ws://"+window.location.host+WebSocketURL,
			     ['echo']);

	      connection.onerror = function (error) {
                  console.log('WebSocket Error ' + error);
              };

              connection.onmessage = function (e) {  // TODO
		console.log(e.data);
						     /*
                  var chat = document.getElementById("chat");
                  var msg = document.createElement("div");
                  msg.appendChild(document.createTextNode(e.data));
                  var child = chat.appendChild(msg);
                  child.scrollIntoView(false); */
	      };
	},

        sendChat: function(msg) {
              connection.send(msg);
        }
    };

    $("#rect_tool").on("mouseup", function() {
	   whiteboard.unchoose_tools();
	   whiteboard.currentTool = "rect";
	   $("#rect_tool").addClass("selected");
    });
    $("#oval_tool").on("mouseup", function() {
	   whiteboard.unchoose_tools();
	   whiteboard.currentTool = "oval";
	   $("#oval_tool").addClass("selected");
    });
    $("#diamond_tool").on("mouseup", function() {
	   whiteboard.unchoose_tools();
	   whiteboard.currentTool = "diamond";
	   $("#diamond_tool").addClass("selected");
    });

    $("#whiteboard .drawarea").on(
	      {	"mousedown": whiteboard.newElement,
		"mousemove": whiteboard.newElementMoveOrDrag,
		"mouseup": whiteboard.newElementCommit});

    whiteboard.openWebSocket();
});

// window.addEventListener("DOMContentLoaded", openWebSocket, false);

var mouseDown = [0, 0, 0, 0, 0, 0, 0, 0, 0],
    mouseDownCount = 0;
document.body.onmousedown = function(evt) {
  ++mouseDown[evt.button];
  ++mouseDownCount;
}
document.body.onmouseup = function(evt) {
  --mouseDown[evt.button];
  --mouseDownCount;
}


		  |}).


:- dynamic
	chat_control/2,				% Pipe, Queue
	utterance/1.				% Message

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
	websocket{opcode:text} :< Message, !,
	assertz(utterance(Message)),
	chatroom_broadcast(Room.name, Message).
handle_message(Message, _Room) :-
	chatroom{joined:Id} :< Message, !,
	assertz(visitor(Id)),
	forall(utterance(Utterance),
	       chatroom_send(Id, Utterance)).
handle_message(Message, _Room) :-
	chatroom{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).
