(function( window, document ) {
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

  window.MessageBus = MessageBus;

})( window, document );
