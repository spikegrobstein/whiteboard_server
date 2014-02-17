(function( window, document ) {
  var MessageBus = function() {
    this.subscribers = {};
    this.debugMode = false;
  };

  MessageBus.prototype.subscribe = function( messageType, handler ) {
    if ( typeof this.subscribers[messageType] === 'undefined' ) {
      this.subscribers[messageType] = [];
    }

    this.debug( 'subscribe', messageType )

    this.subscribers[messageType].push(handler);

    return this;
  };

  MessageBus.prototype.broadcast = function( messageType, message ) {
    var subscribers = this.subscribers[messageType];

    // if there's no subscribers
    if ( typeof subscribers === 'undefined' || subscribers.length === 0 ) {
      // console.log( 'not able to broadcast ' + messageType + ' because of no subscribers' );
      this.debug( 'no subscribers ', { messageType: messageType, message: message } );
      return;
    }

    this.debug( 'broadcast', { messageType: messageType, message: message, count: subscribers.length } );

    for( subscriber in subscribers ) {
      subscribers[subscriber]( messageType, message );
    }
  }

  MessageBus.prototype.debug = function( type, obj ) {
    if ( ! this.debugMode ) { return; }

    console.log({ type: type, obj: obj });
  }

  window.MessageBus = MessageBus;

})( window, document );
