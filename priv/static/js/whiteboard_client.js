(function( window, document ) {
  var WhiteboardClient = function( host, messageBus ) {
    this.messageBus = messageBus
                        .subscribe( 'draw', this.receiveDraw.bind(this) );
                        // .subscribe( 'update', this, this.receiveUpdate.bind(this) );

    this.currentSequence = 0; // sanity checking
    this.isReplaying = false; // set to true when processing bulk messages.
    this.replayQueue = [];    // the queue of packets to replay

    this.connectionCount = 0;
    this.connectionDelay = 0;

    this.host = host;
    this.connect( host );
  };

  WhiteboardClient.prototype.connect = function() {
    var ws = new WebSocket( this.host );

    ws.onopen    = this.handleWebsocketConnected.bind(this);
    ws.onclose   = this.handleWebsocketClose.bind(this);

    // parse the json before sending it to the handler.
    ws.onmessage = function(msg) {
      var data = JSON.parse( msg.data );
      this.handleWebsocketMessage( data );
    }.bind(this);

    this.ws = ws;
  };

  WhiteboardClient.prototype.handleWebsocketConnected = function() {
    console.log('connected websocket!');
    this.connectionCount += 1;
    this.connectionDelay = 0;

    this.messageBus.broadcast( 'ws_connected', {
      connectionCount: this.conectionCount
    } );

    this.sendHello();
  };

  WhiteboardClient.prototype.handleWebsocketClose = function() {
    this.messageBus.broadcast( 'ws_close', {} );

    this.connectionDelay += 1;

    if ( this.connectionDelay > 3 ) {
      this.connectionDelay = 3;

      this.messageBus.broadcast( 'ws_connection_error', {} );
    }

    // console.log('Lost connection, reconnecting... (' + this.connectionDelay + ')');
    setTimeout( this.connect.bind(this), this.connectionDelay * 1000 );
  };

  WhiteboardClient.prototype.handleWebsocketMessage = function( packet ) {
    var event = packet.event,
        payload = packet.payload,
        headers = packet.headers,
        sequence = headers.sequence;

    // a stream of data is coming in
    // if the sequence goes out of sync,
    //    throw packet in trash
    //    request bulk draw
    // bulk draw will have a start and stop sequence number
    // the stop will be whatever the current sequence is
    // flip a switch, and any events that come in should go into a bucket
    // replay the bucket
    // when reach the end, flip the switch back.

    if ( sequence ) {
      this.checkSequence( sequence ) || return;
      this.currentSequence = sequence;
    }

    switch (event) {
      // an info packet. contains some good info.
      case "hello":
        this.processHelloPacket( payload );
        break;

      case "user_list":
      case "user_join":
      case "user_part":
        this.messageBus.broadcast( event, payload );
        break;

      case "draw":
      case "pen_up":
        this.messageBus.broadcast( 'receive_' + event, payload );
        break;

      default:
        console.log("got unknown packet type: " + event);
    }
  };

  WhiteboardClient.prototype.sendHello = function() {
    this.send( 'hello', {} );
  };

  // hello packet contains the user list and current sequence of the board
  // so pull out the user list and trigger the checkSequence() function.
  WhiteboardClient.prototype.processHelloPacket = function( payload ) {
    var name = payload.name,
        sequence = payload.sequence, // the hello packet has the sequence in the payload.
        userList = payload.userList;

    console.log('got hello');
    console.log({client_user_list:userList});

    this.messageBus.broadcast( "user_list", userList );

    this.checkSequence( sequence );
  };

  WhiteboardClient.prototype.checkSequence = function( sequence ) {
    if ( sequence > this.currentSequence + 1 ) {
      // we have a problem
      console.log("Received a sequence that's out of range (" + sequence + ")");

      this.requestUpdateRange( this.currentSequence + 1, sequence );

      return false;
    }

    return true;
  };

  WhiteboardClient.prototype.requestUpdateRange = function( from, to ) {
    console.log("Requensting from " + from + ' to ' + to );
    this.send( 'get_range', { from: from, to: to } );
  };

  WhiteboardClient.prototype.send = function( event, payload) {
    delete payload.pointer;

    var packet = {
      event: event,
      payload: payload
    };

    this.ws.send( JSON.stringify(packet) );

    return this;
  };

  // received a draw event from the local whiteboard
  WhiteboardClient.prototype.receiveDraw = function( messageType, message ) {
    // console.log({ 'type': messageType, 'got message': message });

    this.sendDraw( message );
  };

  // send a draw message back up to the server
  WhiteboardClient.prototype.sendDraw = function( message ) {
    this.send( 'draw', message );

    return this;
  };

  window.WhiteboardClient = WhiteboardClient;

})(window, document);
