:- module(diagrammer , [
	      chatroom/1,
	      diagrammer//0
	  ]).

:- use_module(library(http/json)).
:- use_module(chatroom).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- dynamic
	utterance/1,			% messages
	visitor/1.			% joined visitors

:- html_resource(js('diagrammer.js'),[requires(js('jquery-2.0.3.min.js'))]).

%%	chatroom(+Room)
%
%	Realise the chatroom main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

chatroom(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	chatroom(Room).

%%	handle_message(+Message, +Room) is det
%
%	Handed a dict with a chatroom message, and a
%	dict that defines the Room, handles the message.
%
%
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


%%	diagrammer(?A, ?B) is det
%
%	DCG diagram widget
%
diagrammer -->
	{
           http_absolute_location(img('rect.png'), RectLoc, []),
	   http_absolute_location(img('oval.png'), OvalLoc, []),
           http_absolute_location(img('diamond.png'), DiamondLoc, [])
        },
	html_requires(css('diagrammer.css')),
	html_requires(js('jquery-2.0.3.min.js')),
	html([
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
