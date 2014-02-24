(function( window, document ) {
  window.whiteboard = new Whiteboard( 'ws://' + window.location.host + '/websocket' + window.location.search, document.getElementById('whiteboard') );
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
    .subscribe( 'user_part', function( _event, userInfo) {
      removeUser(userInfo);
    });

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
