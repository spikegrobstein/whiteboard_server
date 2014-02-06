(function( window, document ) {
  var WhiteboardClient = function( host, messageBus ) {
    this.messageBus = messageBus
                        .subscribe( 'draw', this.receiveDraw.bind(this) );
                        // .subscribe( 'update', this, this.receiveUpdate.bind(this) );

    this.connect( host );
  };

  WhiteboardClient.prototype.connect = function( host ) {
    var messageBus = this.messageBus;

    this.ws = new WebSocket( host );

    // handle the connection to the server
    // request the user list
    this.ws.onopen = function() {
      console.log('connected websocket!')
      this.requestUserList();
    }.bind(this);

    this.ws.onclose = function() { console.log('lost connection to websocket!') };

    // handle receiving a message.
    this.ws.onmessage = function(msg) {
      var data = JSON.parse(msg.data),
          event = data.event,
          payload = data.payload;

      switch (event) {
      case "user_list":
        this.messageBus.broadcast('receive_user_list', payload);
        break;

      case "draw":
        this.messageBus.broadcast('receive_draw', payload);
        break;

      case "pen_up":
        this.messageBus.broadcast('receive_pen_up', payload);
        break;

      case "user_join":
        this.messageBus.broadcast('receive_user_join', payload);
        break;

      case "user_part":
        this.messageBus.broadcast('receive_user_part', payload);
        break;

      default:
        console.log("got unknown packet type: " + event);
      }
    }.bind(this);
  };

  WhiteboardClient.prototype.requestUserList = function() {
    this.send( 'user_list', {} );
  }

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
