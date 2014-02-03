(function (global, document) {


  // Whiteboard -- a class that hooks into an element and handles input
  // it can also receive draw events and write to it
  //
  // WhiteboardClient -- connects to server and will send and receive messages. manages the whole communication thing
  // MessageBus -- brokers communication between  Whiteboard and WhiteboardClient.

  var MessageBus = function() {
    this.subscribers = {};
  };

  MessageBus.prototype.subscribe = function( messageType, subscriber, handler ) {
    if ( typeof this.subscribers[messageType] === 'undefined' ) {
      this.subscribers[messageType] = {};
    }

    this.subscribers[messageType][subscriber] = handler;

    return this;
  };

  MessageBus.prototype.broadcast = function( messageType, message ) {
    var subscribers = this.subscribers[messageType];

    // if there's no subscribers
    if ( typeof subscribers == 'undefined' || subscribers.length == 0 ) {
      console.log( 'not able to broadcast ' + messageType + ' because of no subscribers' );
      return;
    }

    for( subscriber in subscribers ) {
      // console.log({"broadcasting to": subscribers[subscriber], "message": message});
      subscribers[subscriber]( messageType, message );
    }
  }

  var WhiteboardClient = function( host, messageBus ) {
    this.messageBus = messageBus
                        .subscribe( 'draw', this, this.receiveDraw.bind(this) );
                        // .subscribe( 'update', this, this.receiveUpdate.bind(this) );

    this.connect( host );
  };

  WhiteboardClient.prototype.connect = function( host ) {
    var messageBus = this.messageBus;

    this.ws = new WebSocket( host );

    this.ws.onopen = function() { console.log('connected websocket!') };
    this.ws.onclose = function() { console.log('lost connection to websocket!') };
    this.ws.onmessage = function(msg) {
      var data = JSON.parse(msg.data);

      global.whiteboard.handleUpdate( 'draw', data );
    };
  };

  WhiteboardClient.prototype.send = function( messageType, message ) {
    message.type = messageType;
    delete message.pointer;

    this.ws.send( JSON.stringify(message) );

    return this;
  };

  // received a draw event from the local whiteboard
  WhiteboardClient.prototype.receiveDraw = function( messageType, message ) {
    // console.log({ 'type': messageType, 'got message': message });

    this.sendDraw( message );
  };

  WhiteboardClient.prototype.receiveUpdate = function( messageType, message ) {

    return this;
  };

  // send a draw message back up to the server
  WhiteboardClient.prototype.sendDraw = function( message ) {
    this.send( 'draw', message );

    return this;
  };

  var Whiteboard = function( host, element ) {
    this.whiteboard = element;
    this.messageBus = new MessageBus()
                            .subscribe( 'update', this.handleUpdate )
                            .subscribe( 'set_penColor', this.handleSetPenColor )
                            .subscribe( 'set_penWidth', this.handleSetPenWidth );

    this.client = new WhiteboardClient( host, this.messageBus );

    this.mouseDown = false;

    this.penWidth = 4;
    this.penColor = "FF0000";

    this.initialize();
  };

  Whiteboard.prototype.handle = function( eventName, callback ) {
    this.whiteboard.addEventListener( eventName, callback, false );
  };

  // hook into all the event listeners
  Whiteboard.prototype.initialize = function() {
    // touch
    this.handle( 'touchstart', this.handleTouchStart.bind(this) );
    this.handle( 'touchend',   this.handleTouchEnd.bind(this)   );
    this.handle( 'touchmove',  this.handleTouchMove.bind(this)  );

    // mouse
    this.handle( 'mousedown',  this.handleMouseDown.bind(this)  );
    this.handle( 'mouseup',    this.handleMouseUp.bind(this)    );
    this.handle( 'mousemove',  this.handleMouseMove.bind(this)  );
  };

  Whiteboard.prototype.handleTouchStart = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );
  };

  Whiteboard.prototype.handleTouchEnd = function( event ) {
    event.preventDefault();

  };

  Whiteboard.prototype.handleTouchMove = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );

  };

  Whiteboard.prototype.handleMouseDown = function( event ) {
    event.preventDefault();

    this.mouseDown = true;

    var x = event.x,
        y = event.y;

    this.sendDrawEvent( 'mouse', x, y );
  };

  Whiteboard.prototype.handleMouseUp = function( event ) {
    event.preventDefault();

    this.mouseDown = false;
  };

  Whiteboard.prototype.handleMouseMove = function( event ) {
    event.preventDefault();

    // if the mouse isn't down, it's not a drag event.
    if ( ! this.mouseDown ) { return; }

    var x = event.x,
        y = event.y;

    this.sendDrawEvent( 'mouse', x, y );
  };

  Whiteboard.prototype.sendDrawEvent = function( pointer, x, y ) {
    this.messageBus.broadcast( 'draw', {
      pointer: pointer,
      x: x,
      y: y,
      penWidth: this.penWidth,
      penColor: this.penColor
    } );
  };

  Whiteboard.prototype.handleUpdate = function( messageType, message ) {
    var ctx = this.whiteboard.getContext('2d');

    ctx.fillStyle = "#" + this.penColor;
    // ctx.fillRect( message.x, message.y, 2, 2 );

    ctx.beginPath();
    ctx.arc(message.x, message.y, this.penWidth, 0,2*Math.PI, false);
    ctx.fill();
  };

  global.MessageBus = MessageBus;
  global.WhiteboardClient = WhiteboardClient;
  global.Whiteboard = Whiteboard;

  global.whiteboard = new Whiteboard( 'ws://' + window.location.host + '/websocket' + window.location.search, document.getElementById('whiteboard') );


  return;

  // code:
  var whiteboard_ele = document.getElementById("whiteboard"),
      whiteboard = new Whiteboard( whiteboard_ele );


  var whiteboard = document.getElementById("whiteboard"),
      info = document.getElementById('info');

  whiteboard.addEventListener( 'touchstart', handleStart, false );
  whiteboard.addEventListener( 'touchend', handleEnd, false );
  whiteboard.addEventListener( 'touchmove', handleMove, false );
  whiteboard.addEventListener( 'touchleave', handleLeave, false )

  log("initialized");

  function handleStart( event ) {
    event.preventDefault();

    log("start");

    var el = whiteboard,
        ctx = el.getContext('2d'),
        touches = event.changedTouches,
        touch = touches[0];

    log("Touches: " + touches.length);
    log("Touch: " + touch.identifier);
  }

  function handleEnd( event ) {
    event.preventDefault();

    var el = whiteboard,
        ctl = el.getContext('2d'),
        touches = event.changedTouches,
        touch = touches[0];

    log("end: " + touch.identifier);
  }

  function handleMove( event ) {
    event.preventDefault();

    var touches = event.changedTouches;

    log("move: " + touches.length);
  }

  function handleLeave( event ) {
    log('left');
  }

  function log() {
    var a;
    for (a in arguments) {
      info.innerHTML += arguments[a] + "\n";
    }

    // cleanup
    var logOutput = info.innerHTML.split("\n");
    if (logOutput.length > 20) {
      logOutput = logOutput.slice(1, 20);
      info.innerHTML = logOutput.join("\n") + "\n";
    }
  }

})(window, document);
