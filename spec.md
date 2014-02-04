# some spec

all communication is done via a websocket from the browser to the server. The data format is JSON.

## receiving

when receiving, the packets should look like:

    {
      event: <event>,
      payload: <data>
    }

types of packets:

 * `user_list`
 * `user_join`
 * `user_part`
 * `draw`
 * `draw_end`

### user_list

Received to list all users on this whiteboard

Data:

    {
      users: [ <nick>, ... ]
    }

Where it's an array of nicks.

### user_join

Received when a user joins.

Data:

    {
      user: <nick>
    }

`<nick>` is a string

### user_part

Received when a user disconnects from the session.

Data:

    {
      user: <nick>
    }

Where `<nick>` is the user's nickname.

### draw

Received when a user is drawing.

Data:

    {
      user: <nick>,
      penWidth: <penWidth>,
      penColor: <penColor>,
      x: <x>,
      y: <y>
    }

 * `<nick>` is the user who is drawing.
 * `<penWidth>` is an integer > 0 that is the thickness of the pen
 * `<penColor>` a hex value (without hash) which is the color of this pen as a string (eg: "ff0000")
 * `<x>` is the x coordinate
 * `<y>` is the y coordinate

### draw_end

Received when a user picks up his pen

Data:

    {
      user: <nick>
    }

The user who lifted his pen.

## Sending

This is the format of packets that are sent from the client to the server

Base structure (same as receiving):

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

