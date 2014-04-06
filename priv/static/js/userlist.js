(function( window, document, Sizzle) {

  var UserList = function( userListEle, messageBus ) {
    this.userListEle = userListEle;
    this.messageBus = messageBus;

    this.users = {}; // hash of users, keyed by id

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
        this.addUser( new User(user) );
      }
    }.bind(this))
    .subscribe( 'user_join', function( _event, userInfo ) {
      this.addUser( new User(userInfo) );
    }.bind(this))
    .subscribe( 'user_leave', function( _event, userInfo ) {
      this.removeUser( userInfo.userId );
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
    this.users = {};
    this.userListEle.innerHTML = '';
  }

  UserList.prototype.addUser = function( user ) {
    console.log("add user: " + user.nick );

    this.users[user.id] = user;

    this.userListEle.appendChild( user.createElement() );
  }

  UserList.prototype.removeUser = function( userId ) {
    this.userListEle.removeChild( this.users[userId].element() );
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

  var User = function( userObject ) {
    this.nick = userObject.nick;
    this.id = userObject.userId;

    this.originalObject = userObject;

    this._element = null;
  };

  User.prototype.elementId = function() {
    return "user_" + this.id;
  };

  User.prototype.element = function() {
    if ( ! this._element ) {
      this._element = document.getElementById( this.elementId() );
    }

    return this._element;
  }

  User.prototype.createElement = function() {
    var ele = document.createElement('li');

    ele.id = this.elementId();
    ele.innerHTML = this.nick;

    this._element = ele;

    console.log( ele );

    return ele;
  }

  window.User = User;
  window.UserList = UserList;

})( window, document, Sizzle );
