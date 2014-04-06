(function( window, document, Sizzle ) {

  var App = function( whiteboardEle, controlsEle ) {
    this.whiteboard = new Whiteboard( this.websocketURL(), whiteboardEle );
    this.controls = controlsEle;
    this.messageBus = this.whiteboard.messageBus;

    this.statusEle = Sizzle('#status-message')[0];
    this.userList = new UserList( Sizzle('#user-list')[0], this.messageBus );

    this.DEFAULT_COLORS = [
      '#FF0000', //red
      '#00FF00',
      '#0000FF',
      '#FFFF00',
      '#00FFFF',
      '#FF00FF',
      '#FFFFFF',
      '#000000'
    ];

    this.initializeColors( this.DEFAULT_COLORS );

    this.initializeControls();

    this.initializeMessaging();
  };

  App.prototype.websocketURL = function() {
    var pathItems = window.location.pathname.split('/'),
        boardKey = null,
        username = null,
        data = null,
        xhr = new XMLHttpRequest();

    pathItems.shift();

    boardKey = pathItems[1];

    xhr.open( 'GET', '/api/whoami', false );
    xhr.send( null );

    // FIXME: needs some kind of error detection
    data = xhr.response;
    data = JSON.parse( data );

    return "ws://" + window.location.host + '/websocket?user_id=' + data.id + '&board_key=' + boardKey;
  };

  App.prototype.initializeMessaging = function() {
    this.messageBus
      // whiteboard connection info
      .subscribe( 'ws_connected', function() {
        this.updateStatus('connected');
      }.bind(this))
      .subscribe( 'ws_disconnected', function() {
        this.updateStatus('disconnected');
      }.bind(this))
      .subscribe( 'ws_connection_error', function() {
        this.updateStatus('error');
      }.bind(this))
  };

  App.prototype.initializeListeners = function() {
    // TODO: rewrite all this shit here. leverage some messageBus goodness.
    Sizzle('#pen-width-input')[0].addEventListener( 'input', function() {
      var value = this.value;

      value = parseInt(value);

      if ( value > 0 ) {
        this.messageBus.broadcast( 'set_pen_width', value );
      }
    });

    Sizzle('#pen-color-input')[0].addEventListener( 'input', function() {
      var value = this.value;

      if ( value.match(/^[a-f0-9]{6}/i) ) {
        this.messageBus.broadcast( 'set_pen_color', value );
      }
    });
  };

  App.prototype.updateStatus = function( newStatus ) {
    if ( ! this.statusEle ) { return; }

    this.statusEle.innerHTML = newStatus;
  };

  App.prototype.initializeColors = function( colors ) {
    var colorsEle = Sizzle('#pen-colors')[0],
        c;

    for ( c in colors ) {
      c = colors[c];
      ele = document.createElement('li');

      ele.className = 'color';
      ele.setAttribute('data-color', c);
      ele.style.backgroundColor = c;
      ele.addEventListener( 'click', this.setColor.bind(this) );
      ele.addEventListener( 'touchstart', this.setColor.bind(this) );

      colorsEle.appendChild(ele);
    }
  };

  //TODO: this should probably be a module function, not a prototype function.
  App.prototype.setColor = function( event ) {
    var color = event.target.getAttribute('data-color');
    event.preventDefault();

    this.messageBus.broadcast( 'set_pen_color', color);
    Sizzle('#pen-color-input')[0].value = color;
  }

  App.prototype.initializeControls = function() {
    var hideshowBar = document.getElementById('control-box-bar'),
        hideshowButton = document.getElementById('control-box-button');

    hideshowBar.addEventListener( 'click', this.toggleControls.bind(this) );

    // move buton to correct spot
    hideshowButton.style.top = (hideshowBar.offsetHeight / 2 - hideshowButton.offsetHeight / 2) + 'px';
  }

  App.prototype.toggleControls = function(event) {
    var hideshowButton = document.getElementById('control-box-button'),
        controlContent = document.getElementById('control-container');

    console.log('click');
    event.preventDefault();
    if ( hideshowButton.getAttribute('href') == '#show-controls' ) {
      console.log('showing');
      hideshowButton.setAttribute('href', '#hide-controls');
      controlContent.style.display = 'block';
    } else {
      console.log('hiding');
      hideshowButton.setAttribute('href', '#show-controls');
      controlContent.style.display = 'none';
    }
  };

  window.app = new App( Sizzle('canvas#whiteboard')[0], Sizzle('#control-box')[0] );

})( window, document, Sizzle );
