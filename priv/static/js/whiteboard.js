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

    // default sizing ( 2x larger than 1080 )
    this.defaultWidth = 1920 * 2;
    this.defaultHeight = 1080 * 2;

    // initialize offscreen context
    this.image = this.createImage( this.defaultWidth, this.defaultHeight );
    this.imageCtx = this.image.getContext('2d');
    this.whiteboardCtx = this.whiteboard.getContext('2d');

    this.zoomRatio = 0.5; // 50% zoom

    // for pinch-to-zoom
    // this stores the size of the last box, formed by taking the 2 finger
    // touchpoints and creating a rectangle and calculating its area
    // lastZoomCenter stores the x/y (as an array) of the center of the zoom box.
    // this allows the ability to scroll around, too
    this.lastZoomBoxSize = null; // an integer
    this.lastZoomCenter = null;  // to be populated with array of [ x, y ]

    // the x/y of the view's upper-left corner compared to the main image
    // this is actual X/Y, in pixels of the source image at actual size.
    this.scrollX = 0;
    this.scrollY = 0;

    // initialize some of our state
    this.penWidth = 4;
    this.penColor = "FF0000";

    // local state of the pen
    // (used for tracking the difference between mousedrag and mousemove)
    this.penDown = false;

    // store where each user's pen is (for connecting lines)
    this.penStatuses = {}; // hash keyed by userId

    // some initialization functions
    this.resizeWhiteboard();
    this.initializeListeners();
  };

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
      event.preventDefault();

      var dx = event.wheelDeltaX,
          dy = event.wheelDeltaY,
          inverted = event.webkitDirectionInvertedFromDevice;

      this.scroll( dx, dy, inverted );
    }.bind(this))
  };

  /*
   *  scroll deltas
   *  this uses real screen values and handles scaling based on zoomRatio
   */
  Whiteboard.prototype.scroll = function( dx, dy, is_inverted, doRedraw ) {
    if ( typeof is_inverted === 'undefined' ) { is_inverted = false; }
    if ( typeof doRedraw === 'undefined' ) { doRedraw = true; }

    if (is_inverted) {
      dx = -dx;
      dy = -dy;
    }

    // translate based on zoomRatio
    dx /= this.zoomRatio;
    dy /= this.zoomRatio;

    this.scrollX -= dx;
    this.scrollY -= dy;

    // the max boundries of the scroll, with zoom taken into account
    var maxXScroll = this.image.width - this.whiteboard.width * ( 1 / this.zoomRatio ),
        maxYScroll = this.image.height - this.whiteboard.height * ( 1 / this.zoomRatio );

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
    window.requestAnimFrame(function() {

      // the width/height of the visible section of the source image
      // based on the whiteboard's dimensions (with zoom taken into account)
      var sourceWidth = this.whiteboard.width * ( 1 / this.zoomRatio ),
          sourceHeight = this.whiteboard.height * ( 1 / this.zoomRatio );

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
    }.bind(this));
  };

  Whiteboard.prototype.setZoom = function( newZoom ) {
    this.zoomRatio = newZoom;

    if ( this.zoomRatio < .25 ) { this.zoomRatio = .25; }
    if ( this.zoomRatio > 2 ) { this.zoomRatio = 2; }
  };

  // functions for sending things to the server
  // TODO: this should probably be part of the client itself.

  Whiteboard.prototype.sendDrawEvent = function( pointer, x, y ) {
    this.messageBus.broadcast( 'draw', {
      pointer: pointer,
      x: this.scrollX + x * ( 1 / this.zoomRatio ),
      y: this.scrollY + y * ( 1 / this.zoomRatio ),
      penWidth: this.penWidth,
      penColor: this.penColor
    } );
  };

  Whiteboard.prototype.sendPenUp = function() {
    this.client.send( 'pen_up', {} );
  };

  // event receivers
  // for how we handle incoming things

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

  Whiteboard.prototype.handlePenUp = function( messageType, message ) {
    var userId = message.userId
        userEleId = message.userId.replace(/[^a-z0-9]/ig, ''),
        userEle = document.getElementById(userEleId);

    userEle.style.backgroundColor = "transparent";

    delete this.penStatuses[userId];
  };

  Whiteboard.prototype.handlePinchToZoom = function( touches ) {

    var touchA    = touches[0],
        touchB    = touches[1],
        boxWidth  = Math.abs( touchA.clientX - touchB.clientX ),
        boxHeight = Math.abs( touchA.clientY - touchB.clientY ),
        boxArea   = boxWidth * boxHeight,
        boxCenter = [
          Math.max( touchA.clientX, touchB.clientX ) - ( boxWidth / 2 ),
          Math.max( touchA.clientY, touchB.clientY ) - ( boxHeight / 2 )
        ];

    // if we're already in zooming mode...
    if ( this.lastZoomBoxSize ) {
      var scaleRatio = boxArea / this.lastZoomBoxSize;

      this.setZoom( this.zoomRatio * scaleRatio );
      this.scroll( boxArea - this.lastZoomBoxSize / 2 * ( 1 / this.zoomRatio), boxArea - this.lastZoomBoxSize / 2 * ( 1 / this.zoomRatio), false, false );
    }

    if ( this.lastZoomCenter ) {
      var dx = boxCenter[0] - this.lastZoomCenter[0],
          dy = boxCenter[1] - this.lastZoomCenter[1];

      this.scroll( dx, dy, true, false );
    }

    this.lastZoomBoxSize = boxArea;
    this.lastZoomBoxCenter = boxCenter;

    this.redraw();
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
      this.handlePinchToZoom( event.touches );
    }

  };

  Whiteboard.prototype.handleTouchEnd = function( event ) {
    event.preventDefault();

    if ( event.touches.length != 2 ) {
      this.client.send('console', 'stopping pinch to zoom');

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
