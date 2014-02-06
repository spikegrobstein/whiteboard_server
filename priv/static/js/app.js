(function( window, document ) {
  window.whiteboard = new Whiteboard( 'ws://' + window.location.host + '/websocket' + window.location.search, document.getElementById('whiteboard') );

  window.whiteboard.messageBus.subscribe( 'receive_user_list', function( _event, users ) {
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
    var user_list_ele = document.getElementById('user-list'),
        userId = user.userId.replace(/[^a-z0-9]/ig, '');

    user_list_ele.innerHTML += '<li id="' + userId + '">' + user.nick + '</li>';
  }

  function removeUser( user ) {
    var userListEle = document.getElementById('user-list'),
        userId = user.userId.replace(/[^a-z0-9]/ig, ''),
        userEle = document.getElementById(userId);

    userListEle.removeChild( userEle );
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
      window.whiteboard.penWidth = value;
    }
  });

  document.getElementById('pen-color-input').addEventListener( 'input', function() {
    var value = this.value;

    if ( value.match(/^[a-f0-9]{6}/i) ) {
      window.whiteboard.penColor = value;
    }
  });

})( window, document );
