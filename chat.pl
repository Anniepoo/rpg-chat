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

:- module(chat_server,
	  [ server/0,
	    server/1,				% ?Port
	    create_chat_room/0
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/hub)).
:- use_module(library(debug)).


/** <module> A scalable websocket based chat server in SWI-Prolog

Chat servers are an example of   services  that require mixed initiative
and may be used to serve many connections. One way to implement is using
long-polling: the browser asks for events   from  the server. The server
waits until there is an event or  it   times  out  after -say- 1 minute,
after which the server replies there are  no events and the client tries
again. The long polling structure can   be implemented in the SWI-Prolog
server architecture, but it is  rather   expensive  because it implies a
Prolog thread for each blocking call.

This demo application implements  a   chatroom  using  _websockets_. The
implementation uses library(http/hub), which  bundles the responsibility
for multiple websockets in a  small  number   of  threads  by  using I/O
multiplexing based on wait_for_input/3. As  a   user  of hub.pl, life is
fairly straighforward:

  - Create a hub using hub_create/3 and a thread that
    listens to chat events and broadcasts the changes.

  - Serve a web page that provides the chat frontend.  The frontend
    contains JavaScript that establishes a websocket on /chat.  If
    a websocket is obtained, hand it to to the room using
    hub_add/2
*/


%%	server is det.
%%	server(?Port) is det.
%
%	Create the chat room and start the   server. The default port is
%	3050.

server :-
	server(3050).

server(Port) :-
	(   debugging(chat),
	    current_prolog_flag(gui, true)
	->  prolog_ide(thread_monitor)
	;   true
	),
	create_chat_room,
	http_server(http_dispatch, [port(Port)]).

% setup the HTTP location. The  first   (/)  loads  the application. The
% loaded application will create  a   websocket  using  /chat. Normally,
% http_upgrade_to_websocket/3 runs call(Goal, WebSocket)  and closes the
% connection if Goal terminates. Here, we use guarded(false) to tell the
% server we will take responsibility for the websocket.

:- http_handler(root(.),    chat_page,      []).
:- http_handler(root(chat),
		http_upgrade_to_websocket(
		    accept_chat,
		    [ guarded(false),
		      subprotocols([chat])
		    ]),
		[ id(chat_websocket)
		]).

chat_page(_Request) :-
	reply_html_page(
	    title('SWI-Prolog chat demo'),
	    \chat_page).

%%	chat_page//
%
%	Generate the web page.

chat_page -->
	style,
	html([
	       div(class('game-table'),
		   div([ id(chat),
		     class(boxed)
		   ], [])
	       ),
	       div(class('roll-area'), [
		       div(class(diesettings), [
			       div([
				   label(for(pool), 'Die Pool'),
				   label(for(hunger), 'Hunger')
			       ]),
			       div(class('little-numbers'), [
				       input([ id(pool), type(number), value(1), min(0), max(20)], []),
				       input([ id(hunger), type(number), value(1), min(0), max(20)], [])
				   ])
			   ]),
		       input([ placeholder('Set dice numbers, type in die roll reason and hit RETURN'),
			       id(input),
			       class([boxed]),
			       onkeypress('handleInput(event)')
			     ], [])
		   ])
	]),
	script.

%%	style//
%(
%	Emit the style sheet. Typically, this  comes from a static file.
%	We generate it inline  here  to   keep  everything  in one file.
%	Second best would be to use a   quasi quotation, but the library
%	does not provide a  CSS  quasi   quotation  (yet).  As  CSS does
%	contains few special characters, this is bearable.

style -->
	html(style([ 'body,html { height:100%; overflow: hidden; }\n',
		     '* { box-sizing: border-box; }\n',
		     '.game-table {  height: 100%; width: calc(50% - 12px); float: left; }\n',
		     '.roll-area { width: calc(50% - 12px); float: right; }\n',
		     '#chat { height: 100%; overflow-y:scroll; }\n',
		     '#input { width: 100%; \c
			       overflow-y: scroll;
			       float: right; }\n',
		     '.boxed {  padding: 5px; \c
				margin: 5px; \c
				border-radius: 5px;\c
				border: solid 1px black; padding:5px; }\n',
		     '.diesettings { float: left; }\n',
		     '.diesettings input[type=number] { width: 60px; }\n',
		     '.diesettings label:last-child { float: right; }\n'
		   ])).

%%	script//
%
%	Generate the JavaScript  that  establishes   the  websocket  and
%	handles events on the websocket.

script -->
	{ http_link_to_id(chat_websocket, [], WebSocketURL)
	},
	js_script({|javascript(WebSocketURL)||
function handleInput(e) {
  if ( !e ) e = window.event;  // IE
  if ( e.keyCode == 13 ) {
    var msg = document.getElementById("input").value;
    var pool = document.getElementById("pool").value;
    var hunger = document.getElementById("hunger").value;

    sendChat(JSON.stringify({pool:pool, hunger:hunger, reason:msg}));
    document.getElementById("input").value = "";
  }
}

var connection;

function openWebSocket() {
  connection = new WebSocket("ws://"+window.location.host+WebSocketURL,
			     ['chat']);

  connection.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  };

  connection.onmessage = function (e) {
    var chat = document.getElementById("chat");
    var msg = document.createElement("div");
    var payload = JSON.parse(e.data);
    msg.innerHTML = payload.html;
    var child = chat.appendChild(msg);
    child.scrollIntoView(false);
  };
}

function sendChat(msg) {
  connection.send(msg);
}

window.addEventListener("DOMContentLoaded", openWebSocket, false);
		  |}).


%%	accept_chat(+WebSocket) is det.
%
%	Normally,  the  goal  called    by   http_upgrade_to_websocket/3
%	processes all communication with the   websocket in a read/write
%	loop. In this case however,  we tell http_upgrade_to_websocket/3
%	that we will take responsibility for   the websocket and we hand
%	it to the chat room.

accept_chat(WebSocket) :-
	hub_add(chat, WebSocket, _Id).

%%	create_chat_room
%
%	Create our actual chat room.

:- dynamic
	utterance/1,			% messages
	visitor/1.			% joined visitors

create_chat_room :-
	hub_create(chat, Room, _{}),
	thread_create(chatroom(Room), _, [alias(chatroom)]).

%%	chatroom(+Room)
%
%	Realise the chatroom main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

chatroom(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	chatroom(Room).

:- use_module(library(http/json)).

handle_message(Message, Room) :-
	websocket{opcode:text} :< Message, !,
	debug(chat, '~w', [Message]),
	atom_json_dict(Message.data, Dict, []),
	debug(chat, 'dict is ~w', [Dict]),
	number_string(PoolCount, Dict.pool),
	number_string(HungerCount, Dict.hunger),
	roll_dice(PoolCount, HungerCount, Result),
	dice_roll_html(Result, Dict.reason, HTML),
	assertz(utterance(HTML)),
	atom_json_dict(NewData, Dict.put(_{html:HTML}), []),
	hub_broadcast(Room.name, Message.put(_{data: NewData})).
handle_message(Message, _Room) :-
	hub{joined:Id} :< Message, !,
	assertz(visitor(Id)),
	forall(utterance(Utterance),
	       hub_send(Id, Utterance)).
handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).

dice_roll_html(Rolls, Reason, HTML) :-
	rolls_termerized_html(Rolls, Reason, TERM),
	phrase(html(TERM), TOKEN),
	delete(TOKEN, nl(_), NNLTOKEN),
	with_output_to(string(HTML),
		       print_html(NNLTOKEN)
		      ).

rolls_termerized_html(Rolls, Reason, div([
				 div(class(reason), Reason),
				 div(class(dice), RollImages)
				     ])) :-
	maplist(roll_roll_image, Rolls, RollImages).

:- table roll_roll_image/2.

roll_roll_image(ImageName, img(src(ImageURI), [])) :-
	format(atom(ImageURI), 'https://partyserver.rocks/anniepoo/diceroller/~w', [ImageName]).

roll_dice(Pool, Hunger, Result) :-
	length(PoolRolls, Pool),
	poolsides(PoolSides),
	maplist({PoolSides}/[Roll]>>random_member(Roll, PoolSides), PoolRolls),
	length(HungerRolls, Hunger),
	hungersides(HungerSides),
	maplist({HungerSides}/[Roll]>>random_member(Roll, HungerSides), HungerRolls),
	append(PoolRolls, HungerRolls, Result).

poolsides([
    'bestial-fail.png',
    'normal-fail.png',
    'normal-fail.png',
    'normal-fail.png',
    'normal-fail.png',
    'normal-success.png',
    'normal-success.png',
    'normal-success.png',
    'normal-success.png',
    'normal-crit.png'
]).
hungersides([
    'red-fail.png',
    'red-fail.png',
    'red-fail.png',
    'red-fail.png',
    'red-fail.png',
    'red-success.png',
    'red-success.png',
    'red-success.png',
    'red-success.png',
    'red-crit.png'
]).



/*
 *
 *
M] DaniQuietNow: up to 5 hunger dice red one with the following symbols:
1. Scull with fangs
2. Empty side
3. Empty side
4. Empty side
5. Empty side
6. Dager
7.Dager
8. Dager
9. Dager
10. Dager with Fangs.

Up to 20 normal (black) dice with the following symbols:
1. EMpty Side
2. Empty side
3. Empty side
4. Empty side
5. Empty side
6. Dager
7.Dager
8. Dager
9. Dager
10. Dager with stars.
      */
