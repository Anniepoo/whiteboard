:- module(diagrammer , [
	      handle_message/2
	  ]).

:- use_module(library(http/json)).
:- use_module(chatroom).

:- dynamic
	utterance/1,			% messages
	visitor/1.			% joined visitors

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
