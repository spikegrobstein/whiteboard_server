window.requestAnimFrame = function(){
    return (
            window.requestAnimationFrame       ||
            window.webkitRequestAnimationFrame ||
            window.mozRequestAnimationFrame    ||
            window.oRequestAnimationFrame      ||
            window.msRequestAnimationFrame     ||
            function(/* function */ callback){
                window.setTimeout(callback, 1000 / 60);
            }
        );
}();

(function( window, document) {
  var Whiteboard = function( host, element ) {
    this.whiteboard = element;
    this.messageBus = new MessageBus()
                            .subscribe( 'receive_draw',   this.handleUpdate.bind(this) )
                            .subscribe( 'receive_pen_up', this.handlePenUp.bind(this) );
                            // .subscribe( 'set_penColor',   this.handleSetPenColor.bind(this) )
                            // .subscribe( 'set_penWidth',   this.handleSetPenWidth.bind(this) );

    this.client = new WhiteboardClient( host, this.messageBus );

    // sizing ( 2x larger than 1080 )
    this.width = 1920 * 2;
    this.height = 1080 * 2;

    // initialize offscreen context
    this.image = this.createImage( this.width, this.height );
    this.imageCtx = this.image.getContext('2d');
    this.whiteboardCtx = this.whiteboard.getContext('2d');

    // for pinch-to-zoom
    // this stores the size of the last box, formed by taking the 2 finger
    // touchpoints and creating a rectangle and calculating its area
    // lastZoomCenter stores the x/y (as an array) of the center of the zoom box.
    // this allows the ability to scroll around, too
    this.lastZoomBoxSize = null; // an integer
    this.lastZoomCenter = null;  // to be populated with array of [ x, y ]

    // properties of the local client
    this.zoomRatio = 1;

    // the x/y of the view's upper-left corner compared to the main image
    // this is actual X/Y, in pixels of the source image at actual size.
    // by default , we want to be centered.
    this.scrollX = ( this.width / 2 ) - ( this.translateZoomFromLocalToFullsize(this.whiteboard.width) / 2 );
    this.scrollY = ( this.height / 2 ) - ( this.translateZoomFromLocalToFullsize(this.whiteboard.height) / 2 );

    // initialize some of our state
    this.penWidth = 4;
    this.penColor = "0000FF";

    // local state of the pen
    // (used for tracking the difference between mousedrag and mousemove)
    this.penDown = false;

    // store where each user's pen is (for connecting lines)
    this.penStatuses = {}; // hash keyed by userId

    // some initialization functions
    this.resizeCanvasToWindow();
    this.initializeListeners();
  };

  // initialize the offscreen image whiteboard and return it
  Whiteboard.prototype.createImage = function( width, height ) {
    var buffer = document.createElement('canvas'),
        ctx = buffer.getContext('2d');

    buffer.width = width;
    buffer.height = height;

    // fill with white
    ctx.fillStyle = '#ffffff';
    ctx.fillRect(0, 0, width, height);

    return buffer;
  }

  // resizes the onscreen canvas in the DOM to the size of the window
  Whiteboard.prototype.resizeCanvasToWindow = function() {
    // resize the canvas a bit
    var body = document.getElementsByTagName('body')[0],
        fullsizeWidth = body.offsetWidth,
        fullsizeHeight = window.innerHeight;

    this.whiteboard.width = fullsizeWidth;
    this.whiteboard.height = fullsizeHeight;
  };

  // convenience method for adding event listener to the whiteboard canvas element
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
      event.preventDefault();

      var dx = event.wheelDeltaX,
          dy = event.wheelDeltaY,
          inverted = event.webkitDirectionInvertedFromDevice,
          scrollDx = this.translateZoomFromLocalToFullsize( dx ),
          scrollDy = this.translateZoomFromLocalToFullsize( dy );

      this.scroll( scrollDx, scrollDy, inverted );
    }.bind(this))
  };

  Whiteboard.prototype.translateZoomFromLocalToFullsize = function( d ) {
    return d / this.zoomRatio;
  };

  Whiteboard.prototype.translateZoomFromFullsizeToLocal = function( d ) {
    return d * this.zoomRatio;
  };

  // return an object with x and y keys
  // translates from canvas x/y to fullsize image x/y
  // takes scrolling and zooming into account
  Whiteboard.prototype.translateFromLocalToFullsize = function( x, y ) {
    return {
      x: this.scrollX + ( x / this.zoomRatio ),
      y: this.scrollY + ( y / this.zoomRatio )
    };
  };

  Whiteboard.prototype.translateFromFullsizeToLocal = function( x, y ) {
    return {
      x: ( x * this.zoomRatio ) - this.scrollX,
      y: ( y * this.zoomRatio ) - this.scrollY
    };
  };

  // given a width and height of the viewport, get width and height in fullsize
  Whiteboard.prototype.translateDimensionsFromLocalToFullsize = function( w, h ) {
    return {
      width: w / this.zoomRatio,
      height: h / this.zoomRatio
    };
  };

  Whiteboard.prototype.translateDimensionsFromFullsizeToLocal = function( w, h ) {
    return {
      width: w * this.zoomRatio,
      height: h * this.zoomRatio
    };
  };

  /*
   *  scroll deltas
   *  this uses actual image size values
   */
  Whiteboard.prototype.scroll = function( dx, dy, is_inverted, doRedraw ) {
    if ( typeof is_inverted === 'undefined' ) { is_inverted = false; }
    if ( typeof doRedraw === 'undefined' ) { doRedraw = true; }

    if (is_inverted) {
      dx = -dx;
      dy = -dy;
    }

    this.scrollX -= dx;
    this.scrollY -= dy;

    this.scrollTo( undefined, undefined, doRedraw );
  };

  Whiteboard.prototype.scrollTo = function( x, y, doRedraw ) {
    if ( typeof x === 'undefined' ) { x = this.scrollX; }
    if ( typeof y === 'undefined' ) { y = this.scrollY; }
    if ( typeof doRedraw === 'undefined' ) { doRedraw = true; }

    var whiteboardDimensions = this.translateDimensionsFromLocalToFullsize( this.whiteboard.width, this.whiteboard.height ),
        maxXScroll = this.image.width - whiteboardDimensions.width,
        maxYScroll = this.image.height - whiteboardDimensions.height;

    this.scrollX = x;
    this.scrollY = y;

    if (this.scrollX < 0) {
      this.scrollX = 0;
    } else if ( this.scrollX > maxXScroll ) {
      this.scrollX = maxXScroll;
    }

    if (this.scrollY < 0) {
      this.scrollY = 0;
    } else if ( this.scrollY > maxYScroll ) {
      this.scrollY = maxYScroll;
    }

    if ( doRedraw ) {
      this.redraw();
    }
  };

  Whiteboard.prototype.redraw = function() {
    if ( this.currentlyDrawing ) { return; }

    this.currentlyDrawing = true;
    window.requestAnimFrame(function() {

      // the width/height of the visible section of the source image
      // based on the whiteboard's dimensions (with zoom taken into account)
      var whiteboardDimensions = this.translateDimensionsFromLocalToFullsize( this.whiteboard.width, this.whiteboard.height ),
          sourceWidth = whiteboardDimensions.width,
          sourceHeight = whiteboardDimensions.height;

      this.whiteboardCtx.drawImage(
        this.image,

        // where on the source image to draw from
        this.scrollX,
        this.scrollY,
        sourceWidth,
        sourceHeight,

        // where on the local canvas to draw to
        0,
        0,
        this.whiteboard.width,
        this.whiteboard.height
      );

      this.currentlyDrawing = false;
    }.bind(this));
  };

  Whiteboard.prototype.setZoom = function( newZoom ) {
    this.zoomRatio = newZoom;

    // this.client.send('console', "newZoom: " + newZoom);

    // restrict to minimum and maximum zoom levels.
    if ( isNaN(this.zoomRatio) ) { this.zoomRatio = 1; }
    if ( this.zoomRatio < .1 ) { this.zoomRatio = .1; }
    if ( this.zoomRatio > 2 ) { this.zoomRatio = 2; }

    return this.zoomRatio;
  };

  // functions for sending things to the server
  // TODO: this should probably be part of the client itself.

  Whiteboard.prototype.sendDrawEvent = function( pointer, x, y ) {
    var translatedCoordinates = this.translateFromLocalToFullsize( x, y );

    this.messageBus.broadcast( 'draw', {
      pointer: pointer,
      x: translatedCoordinates.x,
      y: translatedCoordinates.y,
      penWidth: this.penWidth,
      penColor: this.penColor
    } );
  };

  Whiteboard.prototype.sendPenUp = function() {
    this.client.send( 'pen_up', {} );
  };

  // event receivers
  // for how we handle incoming things

  // receive pen event from server
  Whiteboard.prototype.handleUpdate = function( messageType, message ) {
    var ctx         = this.imageCtx,
        last        = this.penStatuses[message.userId],
        userId      = message.userId.replace(/[^a-z0-9]/ig, ''),
        userEle     = document.getElementById(userId);
        onscreenCtx = this.whiteboardCtx;

    // set background on this user
    userEle.style.backgroundColor = "#" + message.penColor;

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

    // render offscreen image to onscreen canvas
    this.redraw();
  };

  // receive pen-up from server
  Whiteboard.prototype.handlePenUp = function( messageType, message ) {
    var userId = message.userId
        userEleId = message.userId.replace(/[^a-z0-9]/ig, ''),
        userEle = document.getElementById(userEleId);

    userEle.style.backgroundColor = "transparent";

    delete this.penStatuses[userId];
  };


  Whiteboard.prototype.handlePinchToZoom = function( touches ) {

    // calculate zoomRatio by using the box formed by t1 and t2
    // the ratio is the local size / fullsize size
    var vt1        = { x: touches[0].clientX, y: touches[0].clientY }, // t1, view scale
        vt2        = { x: touches[1].clientX, y: touches[1].clientY }, // t2, view scale
        ft1        = this.translateFromLocalToFullsize( vt1.x, vt1.y ), // t1 full scale
        ft2        = this.translateFromLocalToFullsize( vt2.x, vt2.y ), // t2 full scale
        oft1       = this.zoomTouches[0], // original t1, full scale
        oft2       = this.zoomTouches[1], // original t2, full scale
        vboxWidth  = Math.abs( vt1.x - vt2.x ),
        vboxHeight = Math.abs( vt1.y - vt2.y ),
        fboxWidth  = Math.abs( oft1.x - oft2.x ),
        fboxHeight = Math.abs( oft1.y - oft2.y ),
        vboxDiag   = Math.sqrt( Math.pow( vboxWidth, 2 ) + Math.pow( vboxHeight, 2 ) ),
        fboxDiag   = Math.sqrt( Math.pow( fboxWidth, 2 ) + Math.pow( fboxHeight, 2 ) ),
        zoomRatio  = this.setZoom( vboxDiag / fboxDiag ),

        // scroll values:
        // we have original x/y on fullsize
        // lock touchpoint 1's X from original location to that point on the screen
        dx    = oft1.x - ft1.x,
        dy    = oft1.y - ft1.y;

    this.scroll( dx, dy, true );
  };

  // interaction event handers
  // this is stuff like mouse-move, touch events and keyboard events
  // before they get filtered through our internal API.

  Whiteboard.prototype.handleTouchStart = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    if ( event.touches.length == 1 ) {
      this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );
    } else if ( event.touches.length == 2 ) {
      this.client.send('console', 'doing pinch to zoom');

      this.zoomTouches = [
        this.translateFromLocalToFullsize( event.touches[0].clientX, event.touches[0].clientY ),
        this.translateFromLocalToFullsize( event.touches[1].clientX, event.touches[1].clientY )
      ];

      // this.handlePinchToZoom( event.touches );
    }

  };

  Whiteboard.prototype.handleTouchEnd = function( event ) {
    event.preventDefault();

    if ( event.touches.length != 2 ) {
      this.client.send('console', 'stopping pinch to zoom');

      this.zoomTouches = null;
      this.lastZoomBoxSize = null;
      this.lastZoomCenter = null;
    }

    this.sendPenUp();
  };

  Whiteboard.prototype.handleTouchMove = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    if ( event.touches.length == 1 ) {
      this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );
    } else if ( event.touches.length == 2 ) {
      this.client.send('console', 'doing pinch to zoom');
      this.handlePinchToZoom( event.touches );
    }
  };

  Whiteboard.prototype.handleMouseDown = function( event ) {
    event.preventDefault();

    this.penDown = true;

    var x = event.clientX,
        y = event.clientY;

    this.sendDrawEvent( 'mouse', x, y );
  };

  Whiteboard.prototype.handleMouseUp = function( event ) {
    event.preventDefault();

    this.penDown = false;

    this.sendPenUp();
  };

  Whiteboard.prototype.handleMouseMove = function( event ) {
    event.preventDefault();

    // if the mouse isn't down, it's not a drag event.
    if ( ! this.penDown ) { return; }

    var x = event.clientX,
        y = event.clientY;

    this.sendDrawEvent( 'mouse', x, y );
  };

  window.Whiteboard = Whiteboard;

})( window, document );
