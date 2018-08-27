:- module(diagrammer , [
	      chatroom_loop/1,
	      diagrammer//0
	  ]).

:- use_module(library(http/json)).
:- use_module(library(http/hub)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(dcg/basics)).

:- dynamic
	node/1,                         % drawing element
	visitor/1,			% joined visitors
	arc/2.

:- html_resource(js('diagrammer.js'),[requires(js('jquery-2.0.3.min.js'))]).
:- html_resource(js('query.colorPicker.min.js'),[requires(js('jquery-2.0.3.min.js'))]).

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
%	Handed a dict with a hub message, and a
%	dict that defines the Room, handles the message.
%

% add an item (eventually do other things that can be incrementally
% updated
handle_message(Message, Room) :-
	websocket{opcode:text, data:String} :< Message,
	read_term_from_atom(String, Term, []),
	phrase(broadcast_update(Term), CodesToBrowser),
	atom_codes(AtomToBrowser, CodesToBrowser),
	debug(diagrammer, 'AtomToBrowser ~w, ~w', [String, AtomToBrowser]),
	hub_broadcast(Room.name, Message.put(data, AtomToBrowser)).
% someone joined
% we are sending all in one hub_send here, but it's not clear that
% we have to. I did this while looking for a thread starve issue, and
% it's better architecture, so I'm leaving it.
handle_message(Message, _Room) :-
	hub{joined:Id} :< Message, !,
	phrase(joined_update, CodesUpdate),
	atom_codes(AtomUpdate, CodesUpdate),
	assertz(visitor(Id)),
	hub_send(Id, text(AtomUpdate)).
handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).

% arc draw case
broadcast_update(commit(_, _, DownX, DownY, X, Y, 2)) -->
	{
            number(DownX),
            number(DownY),
            node_hit(DownX, DownY, Node),
            node_hit(X, Y, EndNode),
            Node =.. [_, ID, _, _, _],
            EndNode =.. [_, EndID, _, _, _],
            assertz(arc(ID, EndID))
        },
	rebuild_from_scratch.
% move case
broadcast_update(commit(_, _, DownX, DownY, X, Y, 0)) -->
    {
            number(DownX),
            number(DownY),
            node_hit(DownX, DownY, Node),
            Node =.. [Functor, ID, FillColor, OldX, OldY],
            NewX is OldX + X - DownX,
            NewY is OldY + Y - DownY,
            NNode =.. [Functor, ID, FillColor, NewX, NewY],
            retractall(node(Node)),
            assertz(node(NNode))
        },
	rebuild_from_scratch.
% new case
broadcast_update(commit(rect, FillColor, _DownX, _DownY, X, Y, 0)) -->
	{
            number(X),
            number(Y),
            gensym(node, ID),
            assertz(node(rect(ID,FillColor,X,Y)))
        },
	"ddd.addRect(\"",
	FillColor,
	"\", ",
	number(X),
	", ",
	number(Y),
	");".
broadcast_update(commit(oval, FillColor, _DownX, _DownY, X, Y, 0)) -->
	{
            number(X),
            number(Y),
            gensym(node, ID),
            assertz(node(oval(ID,FillColor,X,Y)))
        },
	"ddd.addOval(\"",
	FillColor,
	"\", ",
	number(X),
	", ",
	number(Y),
	");".
broadcast_update(commit(diamond, FillColor, _DownX, _DownY, X, Y, 0)) -->
	{
            number(X),
            number(Y),
            gensym(node, ID),
            assertz(node(diamond(ID,FillColor,X,Y)))
        },
	"ddd.addDiamond(\"",
	FillColor,
	"\", ",
	number(X),
	", ",
	number(Y),
	");".

joined_update --> rebuild_from_scratch.

rebuild_from_scratch -->
	{
            findall(X, node(X), Bag),
	    findall(arc(A,B), arc(A,B), ArcBag)
        },
	"ddd.clear();",
	arcs(ArcBag),
	joined_update_adds(Bag).

arcs([]) --> [],!.
arcs(List) -->
	"ddd.ctx().setTransform(1,0,0,1,0,0);",
	"ddd.ctx().beginPath();",
	arc_list(List),
	"ddd.ctx().closePath(); ddd.strokeOnly();".
arc_list([]) --> [].
arc_list([arc(A,B) | T]) -->
	{
            node(S),
            S =.. [_, A, _, AX, AY],
            node(E),
            E =.. [_, B, _, BX, BY]
        },
	"ddd.ctx().moveTo(",
	number(AX),
	", ",
	number(AY),
	"); ddd.ctx().lineTo(",
	number(BX),
	", ",
	number(BY),
	");",
	arc_list(T).

joined_update_adds([rect(_ID, FillColor, X, Y) | T]) -->
	"ddd.addRect(\"",
	FillColor,
	"\", ",
	number(X),
	", ",
	number(Y),
	");",
	joined_update_adds(T).
joined_update_adds([oval(_ID, FillColor, X, Y) | T]) -->
	"ddd.addOval(\"",
	FillColor,
	"\", ",
	number(X),
	", ",
	number(Y),
	");",
	joined_update_adds(T).
joined_update_adds([diamond(_ID, FillColor, X, Y) | T]) -->
	"ddd.addDiamond(\"",
	FillColor,
	"\", ",
	number(X),
	", ",
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
            http_absolute_location(img('diamond.png'), DiamondLoc, []),
            http_absolute_location(img('text.png'), TextLoc, [])
        },
	html_requires(css('diagrammer.css')),
	html_requires(css('colorPicker.css')),
	html_requires(js('jquery-2.0.3.min.js')),
	html_requires(js('jquery.colorPicker.min.js')),
	html([
	       div(id(diagrammer), [
		   div([class(componentbar)], [
			   img([id(rect_tool), src(RectLoc)]),
			   img([class(selected), id(oval_tool), src(OvalLoc)]),
			   img([id(diamond_tool), src(DiamondLoc)]),
			   img([id(text_tool), src(TextLoc)]),
			   div([ label(for=colorpicker, "Color"),
			         input([ id=colorpicker,
			                 type=text,
			                 value="#333399" ]) ]) ]),
		   div([class('canvas-container')], [
		       canvas([class(drawarea),
			   width('1000'), height('612')], []),
		       input([type(text), id(dia_textentry)], [])				       ])
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
%   Also initialize the color picker
%   from https://github.com/laktek/really-simple-color-picker

script -->
	html_requires(js('diagrammer.js')),
	{ http_link_to_id(chat_websocket, [], WebSocketURL) },
	js_script({|javascript(WebSocketURL)||
$(document).ready(function() {
    ws_initialize(WebSocketURL);
    $('#colorpicker').colorPicker({
        onColorChange: function(id, newValue) {
            ddd.currentColor = newValue;
        },
        pickerDefault: "#333399"
    });
});
		  |}).


node_hit(X, Y, rect(ID, FillColor, RX, RY)) :-
	node(rect(ID, FillColor, RX, RY)),
	X >= RX - 50,
	X =< RX + 50,
	Y >= RY - 37.5,
	Y =< RY + 37.5.
node_hit(X, Y, diamond(ID, FillColor, RX, RY)) :-
	node(diamond(ID, FillColor, RX, RY)),
	DX is abs(X - RX),
	DY is abs(Y - RY),
	DY < 37.5 - DX * 37.5 / 50.0.
node_hit(X, Y, oval(ID, FillColor, RX, RY)) :-
	node(oval(ID, FillColor, RX, RY)),
	37.5 * 37.5 > (X - RX)*(X - RX) + (Y - RY)*(Y - RY).
