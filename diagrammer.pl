:- module(diagrammer , [
	      chatroom_loop/1,
	      diagrammer//0
	  ]).

:- use_module(library(http/json)).
:- use_module(chatroom).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(dcg/basics)).

:- dynamic
	node/1,                         % drawing element
	visitor/1.			% joined visitors

:- html_resource(js('diagrammer.js'),[requires(js('jquery-2.0.3.min.js'))]).

%%	chatroom_loop(+Room)
%
%	Realise the chatroom main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

chatroom_loop(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	chatroom_loop(Room).

%%	handle_message(+Message, +Room) is det
%
%	Handed a dict with a chatroom message, and a
%	dict that defines the Room, handles the message.
%

% add an item (eventually do other things that can be incrementally
% updated
handle_message(Message, Room) :-
	websocket{opcode:text, data:String} :< Message, !,
	read_term_from_atom(String, Term, []),
	phrase(broadcast_update(Term), CodesToBrowser),
	atom_codes(AtomToBrowser, CodesToBrowser),
	debug(diagrammer, 'AtomToBrowser ~w, ~w', [String, AtomToBrowser]),
	chatroom_broadcast(Room.name, Message.put(data, AtomToBrowser)).
% someone joined
% we are sending all in one chatroom_send here, but it's not clear that
% we have to. I did this while looking for a thread starve issue, and
% it's better architecture, so I'm leaving it.
handle_message(Message, _Room) :-
	chatroom{joined:Id} :< Message, !,
	phrase(joined_update, CodesUpdate),
	atom_codes(AtomUpdate, CodesUpdate),
	assertz(visitor(Id)),
	chatroom_send(Id, text(AtomUpdate)).
handle_message(Message, _Room) :-
	chatroom{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).

% move case
broadcast_update(commit(_, DownX, DownY, X, Y, 0)) -->
	{
            number(DownX),
	    number(DownY),
            node_hit(DownX, DownY, Node),
            Node =.. [Functor, OldX, OldY],
	    NewX is DownX - OldX + X,
            NewY is DownY - OldY + Y,
            NNode =.. [Functor, NewX, NewY],
            retractall(node(Node)),
            assertz(node(NNode))
        },
	rebuild_from_scratch.

% new case
broadcast_update(commit(rect, _DownX, _DownY, X, Y, 0)) -->
	{
            number(X),
	    number(Y),
            assertz(node(rect(X,Y)))
        },
	"diagrammer.addRect(",
	number(X),
	", ",
	number(Y),
	");".
broadcast_update(commit(oval, _DownX, _DownY, X, Y, 0)) -->
	{
            number(X),
	    number(Y),
	    assertz(node(oval(X,Y)))
        },
	"diagrammer.addOval(",
	number(X),
	", ",
	number(Y),
	");".
broadcast_update(commit(diamond, _DownX, _DownY, X, Y, 0)) -->
	{
            number(X),
	    number(Y),
            assertz(node(diamond(X,Y)))
        },
	"diagrammer.addDiamond(",
	number(X),
	", ",
	number(Y),
	");".

joined_update --> rebuild_from_scratch.

rebuild_from_scratch -->
	{
            findall(X, node(X), Bag)
        },
	"diagrammer.clear();",
	joined_update_adds(Bag).

joined_update_adds([rect(X, Y) | T]) -->
	"diagrammer.addRect(",
	number(X),
	",",
	number(Y),
	");",
	joined_update_adds(T).
joined_update_adds([oval(X, Y) | T]) -->
	"diagrammer.addOval(",
	number(X),
	",",
	number(Y),
	");",
	joined_update_adds(T).
joined_update_adds([diamond(X, Y) | T]) -->
	"diagrammer.addDiamond(",
	number(X),
	",",
	number(Y),
	");",
	joined_update_adds(T).
joined_update_adds([]) --> [].

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


node_hit(X, Y, rect(RX, RY)) :-
	node(rect(RX, RY)),
	X >= RX - 50,
	X =< RX + 50,
	Y >= RY - 37.5,
	Y =< RY + 37.5.
node_hit(X, Y, diamond(RX, RY)) :-
	node(diamond(RX, RY)),
	DX is abs(X - RX),
	DY is abs(Y - RY),
	DY < 37.5 - DX * 37.5 / 50.0.
node_hit(X, Y, oval(RX, RY)) :-
	node(oval(RX, RY)),
	37.5 * 37.5 > (X - RX)*(X - RX) + (Y - RY)*(Y - RY).
