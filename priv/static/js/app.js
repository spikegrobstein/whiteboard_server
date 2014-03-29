(function( window, document ) {

  // return the websocket URL to use
  function whiteboardWebsocketURL() {
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
  }

  window.whiteboard = new Whiteboard( whiteboardWebsocketURL(), document.getElementById('whiteboard') );
  window.userList = document.getElementById('user-list');

  window.whiteboard.messageBus
    // whiteboard connection info
    .subscribe( 'ws_connected', function() {
      var msg_ele = document.getElementById('message');

      msg_ele.innerHTML = 'connected';
    })
    .subscribe( 'ws_disconnected', function() {
      var msg_ele = document.getElementById('message');

      msg_ele.innerHTML = 'disconnected';
    })
    .subscribe( 'ws_connection_error', function() {
      var msg_ele = document.getElementById('message');

      msg_ele.innerHTML = 'error';
    })

    // userlist interaction
    .subscribe( 'user_list', function( _event, users ) {
      console.log({got_user_list: users});

      var i, user;

      clearUsers();

      for ( i in users ) {
        user = users[i];
        addUser( user );
      }
    })
    .subscribe( 'user_join', function( _event, userInfo ) {
      addUser(userInfo);
    })
    .subscribe( 'user_part', function( _event, userInfo ) {
      removeUser(userInfo);
    })
    .subscribe( 'receive_draw', function( _event, userInfo ) {
      highlightUser( userInfo.userId, true );
    })
    .subscribe( 'receive_pen_up', function( _event, userInfo ) {
      highlightUser( userInfo.userId, false )
    });

  var colorsEle = document.getElementById('pen-colors'),
      c,
      colors = [
        '#FF0000', //red
        '#00FF00',
        '#0000FF',
        '#FFFF00',
        '#00FFFF',
        '#FF00FF',
        '#FFFFFF',
        '#000000'
      ];

  for ( c in colors ) {
    c = colors[c];
    ele = document.createElement('li');

    ele.className = 'color';
    ele.setAttribute('data-color', c);
    ele.style.backgroundColor = c;
    ele.addEventListener('click', setColor.bind(ele));

    colorsEle.appendChild(ele);
  }

  function setColor() {
    var color = this.getAttribute('data-color');

    window.whiteboard.messageBus.broadcast( 'set_pen_color', color);
    document.getElementById('pen-color-input').value = color;
  }

  function clearUsers() {
    window.userList.innerHTML = '';
  }

  function addUser( user ) {
    var userId = user.userId.replace(/[^a-z0-9]/ig, ''),
        nick = user.nick;

    console.log("add user: " + nick );
    window.userList.innerHTML += '<li id="' + userId + '">' + nick + '</li>';
  }
  window.addUser = addUser;

  function removeUser( user ) {
    var userId = user.userId.replace(/[^a-z0-9]/ig, ''),
        userEle = document.getElementById(userId);

    window.userList.removeChild( userEle );
  }

  function highlightUser( id, enable ) {
    console.log('highlight user: ' + id + ' - ' + enable);
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
      window.whiteboard.messageBus.broadcast( 'set_pen_width', value );
    }
  });

  document.getElementById('pen-color-input').addEventListener( 'input', function() {
    var value = this.value;

    if ( value.match(/^[a-f0-9]{6}/i) ) {
      window.whiteboard.messageBus.broadcast( 'set_pen_color', value );
    }
  });

})( window, document );
