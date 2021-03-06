# some spec

all communication is done via a websocket from the browser to the server. The data format is JSON.

## receiving (client)

when receiving, the packets should look like:

    {
      event: <event>,
      headers: {},
      payload: <data>
    }

types of packets:

 * `user_list`
 * `user_join`
 * `user_part`
 * `draw`
 * `draw_end`

Headers should contain a `sequence` or other data.

*note*: Future versions of the protocol may shorten these names to `e`, `h`, and `p` or even treat the whole
structure as an array. Probably an array is best.

### user_list

Received to list all users on this whiteboard

Data:

    {
      users: [ { userId: <id>, nick: <nick> }, ... ]
    }

Where it's an array of user objects.

### user_join

Received when a user joins.

Data:

    {
      user: { userId: <id>, nick: <nick> }
    }

`<userId>` is a string that uniquely identifies an individual user.

`<nick>` is a string

### user_part

Received when a user disconnects from the session.

Data:

    {
      user: <id>
    }

Where `<id>` is the user's userId.

### draw

Received when a user is drawing.

Data:

    {
      userId: <id>,
      penWidth: <penWidth>,
      penColor: <penColor>,
      x: <x>,
      y: <y>
    }

 * `<id>` is the user who is drawing.
 * `<penWidth>` is an integer > 0 that is the thickness of the pen
 * `<penColor>` a hex value (without hash) which is the color of this pen as a string (eg: "ff0000")
 * `<x>` is the x coordinate
 * `<y>` is the y coordinate

#### Future

Future versions may optimize this structure so that it uses less data over the wire. For instance,
something like: `[ <userId>, [ <penWidth>, <penColor> ], [ x, y ] ]`.

Comparing:

    {"userId":"#PID<0.257.0>","penWidth":4,"penColor":"ff0000","x":500,"y":500}

to

    ["#PID<0.257.0>",[4,"ff0000"],[500,500]]

That's 53% of the size.


### draw_end

Received when a user picks up his pen

Data:

    {
      userId: <id>
    }

The user who lifted his pen.

## Sending (server)

This is the format of packets that are sent from the client to the server

Base structure (same as receiving, but no headers):

    {
      event: <event>,
      payload: <data>
    }

Where `<event>` is the name of the action and payload is packet-specific.

The types of events:

 * `pen_up`
 * `draw`
 * `user_list`

### pen_up

Sent when the user stops drawing.

no payload is sent with this. it just signals the server that the user stopped drawing.

### draw

sent with the current X/Y of the draw event.

Data:

    {
      x: <x>,
      y: <y>,
      penWidth: <penWidth>,
      penColor: <penColor>
    }

See description of received `draw` events.

### user_list

Client is requesting a user list. no payload. server should send the `user_list` packet back.

## internal processes

this is proof of concept

message comes in to websocket and is sent to message processor via `route/2`. It takes the parsed message along with `self`.

From there, the router will parse the content of the message and do the appropriate thing. For instance
if the message is a `draw` event, it will push it to the broadcaster; or if it's a `get_users` message,
it'll send out a `user_list` message.

## Whiteboards

A whiteboard has several properties. These include:

 * `name` -- the name of the whiteboard
 * `id` -- an identifier used for sending out invites
 * `data` -- the data in the board (a list of draw events)
 * `client_store` -- a pid to the client_store for this whiteboard.

A whiteboard should act as a supervisor to its `client_store` process.

A client can either create a new whiteboard or join an existing one. If creating a new one, it spins up a new
instance of the Whiteboard process, which spins up an associated `client_store`. The client_store then gets the
websocket pid added as a client, with any metadata that comes with it.

When "logging in," the client should have a `whiteboard_id` and a `nick`. The whiteboard should already exist.



Data comes in from a websocket (client) and is sent to the whiteboard `pid`, which then gets run through a router.
If the process says to 

## Message Bus

the message bus serves as a conduit for all inter-object communication, and in some cases, even intra-object.
An established message bus protocol will keep everything running smoothly and allow maintainable code in the frontend.

### Message types:

#### Websocket-related

 * `ws_connected` -- the websocket is connected
 * `ws_disconnected` -- the websocket lost connection
 * `ws_connection_error` -- the websocket has failed to reconnect 3x
 * `ws_message` -- new msg came in over websocket. so anything can process data as it comes in
 * `ws_send` -- send a message over the socket. this is so anything can send raw data

#### incomming server events

 * `user_list` -- received a full list of users
 * `user_join` -- a user joined the whiteboard
 * `user_part` -- a user left the board
 * `user_draw` -- received a user draw event
 * `user_pen_up` -- received a user pen-up event
 *
