(function (global, document) {


  // Whiteboard -- a class that hooks into an element and handles input
  // it can also receive draw events and write to it
  //
  // WhiteboardClient -- connects to server and will send and receive messages. manages the whole communication thing
  // MessageBus -- brokers communication between  Whiteboard and WhiteboardClient.

  var MessageBus = function() {
    this.subscribers = {};
  };

  MessageBus.prototype.subscribe = function( messageType, handler ) {
    if ( typeof this.subscribers[messageType] === 'undefined' ) {
      this.subscribers[messageType] = [];
    }

    this.subscribers[messageType].push(handler);

    return this;
  };

  MessageBus.prototype.broadcast = function( messageType, message ) {
    var subscribers = this.subscribers[messageType];

    // if there's no subscribers
    if ( typeof subscribers === 'undefined' || subscribers.length === 0 ) {
      // console.log( 'not able to broadcast ' + messageType + ' because of no subscribers' );
      return;
    }

    for( subscriber in subscribers ) {
      // console.log({"broadcasting to": subscribers[subscriber], "message": message});
      subscribers[subscriber]( messageType, message );
    }
  }

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

  var Whiteboard = function( host, element ) {
    this.whiteboard = element;
    this.messageBus = new MessageBus()
                            .subscribe( 'receive_draw',   this.handleUpdate.bind(this) )
                            .subscribe( 'receive_pen_up', this.handlePenUp.bind(this) );
                            // .subscribe( 'set_penColor',   this.handleSetPenColor.bind(this) )
                            // .subscribe( 'set_penWidth',   this.handleSetPenWidth.bind(this) );

    this.client = new WhiteboardClient( host, this.messageBus );

    this.mouseDown = false;

    this.penWidth = 4;
    this.penColor = "FF0000";

    // store where each user's pen is (for connecting lines)
    this.penStatuses = {}; // hash keyed by userId

    this.resizeWhiteboard();
    this.initializeListeners();
  };

  Whiteboard.prototype.resizeWhiteboard = function() {
    // resize the canvas a bit
    var body = document.getElementsByTagName('body')[0],
        fullsizeWidth = body.offsetWidth,
        fullsizeHeight = window.innerHeight;

    this.whiteboard.width = fullsizeWidth;
    this.whiteboard.height = fullsizeHeight;
  };

  Whiteboard.prototype.handle = function( eventName, callback ) {
    this.whiteboard.addEventListener( eventName, callback, false );
  };

  // hook into all the event handlers
  Whiteboard.prototype.initializeListeners = function() {
    // touch
    this.handle( 'touchstart', this.handleTouchStart.bind(this) );
    this.handle( 'touchend',   this.handleTouchEnd.bind(this)   );
    this.handle( 'touchmove',  this.handleTouchMove.bind(this)  );

    // mouse
    this.handle( 'mousedown',  this.handleMouseDown.bind(this)  );
    this.handle( 'mouseup',    this.handleMouseUp.bind(this)    );
    this.handle( 'mousemove',  this.handleMouseMove.bind(this)  );

    this.handle( 'mousewheel', function(event) {
      console.log(event);
    })
  };

  Whiteboard.prototype.handleTouchStart = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );
  };

  Whiteboard.prototype.handleTouchEnd = function( event ) {
    event.preventDefault();

    this.client.send( 'pen_up', {} );
  };

  Whiteboard.prototype.handleTouchMove = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );
  };

  Whiteboard.prototype.handleMouseDown = function( event ) {
    event.preventDefault();

    this.mouseDown = true;

    var x = event.clientX,
        y = event.clientY;

    this.sendDrawEvent( 'mouse', x, y );
  };

  Whiteboard.prototype.handleMouseUp = function( event ) {
    event.preventDefault();

    this.mouseDown = false;
    this.client.send( 'pen_up', {} );

  };

  Whiteboard.prototype.handleMouseMove = function( event ) {
    event.preventDefault();

    // if the mouse isn't down, it's not a drag event.
    if ( ! this.mouseDown ) { return; }

    var x = event.clientX,
        y = event.clientY;

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
    var ctx = this.whiteboard.getContext('2d'),
        last = this.penStatuses[message.userId];

    ctx.fillStyle = "#" + message.penColor;
    // ctx.fillRect( message.x, message.y, 2, 2 );

    ctx.beginPath();
    if ( last ) {
      ctx.moveTo(last.x, last.y);
      ctx.lineTo(message.x, message.y);
      ctx.lineWidth = message.penWidth;
      ctx.strokeStyle = "#" + message.penColor;
      ctx.stroke();
    }

    ctx.arc(message.x, message.y, message.penWidth / 2, 0,2*Math.PI, false);
    ctx.fill();

    this.penStatuses[message.userId] = { x: message.x, y: message.y };
  };

  Whiteboard.prototype.handlePenUp = function( messageType, message ) {
    var userId = message.userId;

    delete this.penStatuses[userId];
  };

  global.MessageBus = MessageBus;
  global.WhiteboardClient = WhiteboardClient;
  global.Whiteboard = Whiteboard;

  global.whiteboard = new Whiteboard( 'ws://' + window.location.host + '/websocket' + window.location.search, document.getElementById('whiteboard') );

  global.whiteboard.messageBus.subscribe( 'receive_user_list', function( _event, users ) {
    console.log({got_user_list: users});

    var i, user;

    for ( i in users ) {
      user = users[i];
      addUser( user );
    }
  })
  .subscribe( 'receive_user_join', function( _event, userInfo ) {
    addUser(userInfo);
  })
  .subscribe( 'receive_user_part', function( _event, userInfo) {
    removeUser(userInfo);
  });

  function addUser( user ) {
    var user_list_ele = document.getElementById('user-list');

    user_list_ele.innerHTML += '<li data-user-id="' + user.userId + '">' + user.nick + '</li>';
  }

  function removeUser( user ) {
    console.log("delete user: " + user.userId);
  }

  var control_toggle_btn = document.getElementById('hideshow'),
      tools_ele = document.getElementById('form');

  control_toggle_btn.addEventListener( 'click', function() {
    if ( this.innerHTML == 'X' ) {
      this.innerHTML = '&gt;';
      tools_ele.style.display = 'none';
    } else {
      this.innerHTML = 'X';
      tools_ele.style.display = 'block';
    }
  });

  // TODO: rewrite all this shit here. leverage some messageBus goodness.
  document.getElementById('pen-width-input').addEventListener( 'input', function() {
    var value = this.value;

    value = parseInt(value);

    if ( value > 0 ) {
      global.whiteboard.penWidth = value;
    }
  });

  document.getElementById('pen-color-input').addEventListener( 'input', function() {
    var value = this.value;

    if ( value.match(/^[a-f0-9]{6}/i) ) {
      global.whiteboard.penColor = value;
    }
  });

})(window, document);
