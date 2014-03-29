(function( window, document ) {
  var WhiteboardClient = function( host, messageBus ) {
    this.messageBus = messageBus
                        .subscribe( 'draw', this.receiveDraw.bind(this) )
                        .subscribe( 'pen_up', this.receivePenUp.bind(this) );
                        // .subscribe( 'update', this, this.receiveUpdate.bind(this) );

    this.currentSequence = 0; // sanity checking

    // tracking reconnects
    this.connectionCount = 0;
    this.connectionDelay = 0;

    this.host = host;
    this.connect( host );
  };

  /*  establish a connection to the server.
   */
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

  /*  yay! connected!
   */
  WhiteboardClient.prototype.handleWebsocketConnected = function() {
    console.log('connected websocket!');
    this.connectionCount += 1;
    this.connectionDelay = 0;

    this.messageBus.broadcast( 'ws_connected', {
      connectionCount: this.conectionCount
    } );

    this.sendHello();
  };

  /*  when connection closes, attempt to reconnect
   *  wait a second, retry... then wait longer. max out at 3 second interval
   */
  WhiteboardClient.prototype.handleWebsocketClose = function() {
    this.messageBus.broadcast( 'ws_close', {} );

    // increase the connection delay
    this.connectionDelay += 1;

    // but no more than 3 second delay
    if ( this.connectionDelay > 3 ) {
      this.connectionDelay = 3;

      this.messageBus.broadcast( 'ws_connection_error', {} );
    }

    // attempt to reconnect after a certain amount of sleep
    setTimeout( this.connect.bind(this), this.connectionDelay * 1000 );
  };

  /* Process incomming packets from the server
   * route them accordingly
   */
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

    if ( ! this.checkSequence( sequence ) ) { return; }

    switch (event) {
      // an info packet. contains some good info.
      case "hello":
        this.processHelloPacket( payload );
        break;

      case "user_list":
      case "user_join":
      case "user_leave":
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

  /* hello packet contains the user list and current sequence of the board
   * so pull out the user list and trigger the checkSequence() function.
   */
  WhiteboardClient.prototype.processHelloPacket = function( payload ) {
    var name = payload.name,
        sequence = payload.sequence, // the hello packet has the sequence in the payload.
        userList = payload.userList;

    // notify about an updated user_list
    this.messageBus.broadcast( "user_list", userList );

    // force-check the sequence
    this.checkSequence( sequence );
  };

  /* validate that an incomming packet fits our sequence
   * each packet should have an incremented sequence number
   * if sequence is undefined, ignore it; some packets just don't have sequence numbers
   * this will also have the side-effect of updating the internal sequence counter
   */
  WhiteboardClient.prototype.checkSequence = function( sequence ) {
    // if no sequence is passed, then just return true
    // this means that the packet didn't contain a sequence and this check is moot
    if ( typeof sequence === 'undefined' ) { return true; }

    if ( sequence > this.currentSequence + 1 ) {
      // we have a problem
      console.log("Received a sequence that's out of range (" + sequence + ")");

      this.requestUpdateRange( this.currentSequence + 1, sequence );

      return false;

    } else if ( this.currentSequence > 0 && sequence <= this.currentSequence ) {
      // this is very bad. we're ahead of the server somehow?
      alert('A totally fatal error has occurred. Our sequence is too new new. (' + sequence + ' <= ' + this.currentSequence + ')');
      return false;
    }

    this.currentSequence = sequence;

    return true;
  };

   /*  given a from and to sequence, request the data from the server
   */
  WhiteboardClient.prototype.requestUpdateRange = function( from, to ) {
    console.log("Requensting from " + from + ' to ' + to );

    this.send( 'get_range', { from: from, to: to } );
  };

  /*  send a packet to the server
   *  the packet should have an event and payload
   *  it'll be stripped of the pointer, if it exists
   */
  WhiteboardClient.prototype.send = function( event, payload) {
    delete payload.pointer;

    var packet = {
      event: event,
      payload: payload
    };

    this.ws.send( JSON.stringify(packet) );

    return this;
  };

  /*
   *  received a draw event from the local whiteboard
   */
  WhiteboardClient.prototype.receiveDraw = function( messageType, message ) {
    this.send( 'draw', message );
  };

  WhiteboardClient.prototype.receivePenUp = function( messageType, message ) {
    this.send( 'pen_up', message );
  }

  window.WhiteboardClient = WhiteboardClient;

})(window, document);
