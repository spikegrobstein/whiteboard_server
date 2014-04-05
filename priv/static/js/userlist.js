(function( window, document, Sizzle) {

  var UserList = function( userListEle, messageBus ) {
    this.userList = userListEle;
    this.messageBus = messageBus;

    this.initializeMessaging();
  };

  UserList.prototype.initializeMessaging = function() {
    this.messageBus
    // userlist interaction
    .subscribe( 'user_list', function( _event, users ) {
      console.log({got_user_list: users});

      var i, user;

      this.clearUsers();

      for ( i in users ) {
        user = users[i];
        this.addUser( user );
      }
    }.bind(this))
    .subscribe( 'user_join', function( _event, userInfo ) {
      this.addUser(userInfo);
    }.bind(this))
    .subscribe( 'user_leave', function( _event, userInfo ) {
      this.removeUser(userInfo);
    }.bind(this))
    .subscribe( 'receive_draw', function( _event, userInfo ) {
      this.highlightUser( userInfo, true );
    }.bind(this))
    .subscribe( 'receive_pen_up', function( _event, userInfo ) {
      this.highlightUser( userInfo, false )
    }.bind(this));
  };

  UserList.prototype.userElementIdFor = function( user ) {
    return "user_" + user.userId;
  }

  UserList.prototype.clearUsers = function() {
    this.userList.innerHTML = '';
  }

  UserList.prototype.addUser = function( user ) {
    var userId = this.userElementIdFor( user ),
        nick = user.nick;

    console.log("add user: " + nick );
    this.userList.innerHTML += '<li id="' + userId + '">' + nick + '</li>';
  }

  UserList.prototype.removeUser = function( user ) {
    var userId = userElementIdFor( user ),
        userEle = document.getElementById(userId);

    this.userList.removeChild( userEle );
  }

  UserList.prototype.highlightUser = function( user, enable ) {
    var userId = this.userElementIdFor( user ),
        color = user.penColor,
        userElement = Sizzle('#' + userId)[0];

    if ( enable ) {
      // highlight
      userElement.style.backgroundColor = color;
    } else {
      // unhighlight
      userElement.style.backgroundColor = 'transparent';
    }
  };

  window.UserList = UserList;

})( window, document, Sizzle );
