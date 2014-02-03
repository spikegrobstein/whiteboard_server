# some spec

## receiving

when receiving, the packets should look like:

    {
      event: <event>,
      data: <data>
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

