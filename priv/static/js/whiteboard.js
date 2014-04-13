window.requestAnimFrame = function(){
    return (
            window.webkitRequestAnimationFrame ||
            window.requestAnimationFrame       ||
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
    var whiteboardDimensions;

    this.whiteboard = element;
    this.messageBus = this.initializeMessageBus();

    this.client = new WhiteboardClient( host, this.messageBus );

    this.DEFAULT_WIDTH = 1920;
    this.DEFAULT_HEIGHT = 1080;

    this.MAX_ZOOM = 1.58;
    this.MIN_ZOOM = 0.25;

    this.ZOOM_INCREMENT = 0.1; // for keyboard zoom.

    // the zoom factor
    this.zoomRatio = 1;

    // sizing ( 1080 HDTV size )
    // this has to be < 3MP to work properly on iOS devices
    this.fullsizeImage = this.createImage( this.DEFAULT_WIDTH, this.DEFAULT_HEIGHT );
    this.updateScaledImage();

    this.whiteboardCtx = this.whiteboard.getContext('2d');

    this.pattern = this.createPattern();
    this.whiteboardCtx.fillStyle = this.pattern; // only need to set this once. OPTIMIZATION

    // the scroll offsets in screen dimensions
    this.scrollX = 0;
    this.scrollY = 0;

    // initialize some of our state
    this.penWidth = 4;
    this.penColor = "#FF0000";

    // local state of the pen
    // (used for tracking the difference between mousedrag and mousemove)
    this.penDown = false;

    // store where each user's pen is (for connecting lines)
    this.penStatuses = {}; // hash keyed by userId

    this.dirtyBuffer = true; // for deciding whether screen needs to update
    this.batchDrawing = false; // don't slow down while doing mass update.

    // some initialization functions
    this.resizeCanvasToWindow();

    this.initializeListeners();
    this.drawLoop();
  };

  Whiteboard.prototype.initializeMessageBus = function() {
    var messageBus = new MessageBus()
                          .subscribe( 'receive_draw',   this.handleUpdate.bind(this) )
                          .subscribe( 'receive_pen_up', this.handlePenUp.bind(this) )
                          .subscribe( 'keyboard_zoom', this.handleKeyboardZoom.bind(this) )

                          .subscribe( 'set_pen_color', function( _event, penColor ) {
                            this.penColor = penColor;
                          }.bind(this))
                          .subscribe( 'set_pen_width', function( _event, penWidth ) {
                            this.penWidth = penWidth;
                          }.bind(this))
                          .subscribe( 'batch.start', function() {
                            this.batchDrawing = true;
                          }.bind(this))
                          .subscribe( 'batch.finish', function() {
                            this.batchDrawing = false;
                          }.bind(this));

   return messageBus;
  };

  /*
   * given a width and height, create a new image and return that image
   * optionally pass it a replaceImage which will prevent the creation of a new
   * canvas element with a new imageContext.
   */
  Whiteboard.prototype.createImage = function( width, height, replaceImage ) {
    var image, ctx;

    // if a replaceImage was passed, update it in place
    // this will only happen if the replaceImage is the same width/height of the image we're creating
    if ( replaceImage && replaceImage.width == width && replaceImage.height == height ) {
      image = replaceImage.image;
      ctx = replaceImage.ctx;
    } else {
      image = document.createElement('canvas'),
      ctx = image.getContext('2d');

      image.width = width;
      image.height = height;
    }

    // fill it with white
    ctx.fillStyle = '#ffffff';
    ctx.fillRect(0, 0, width, height);

    return {
      width: width,
      height: height,
      image: image,
      ctx: ctx
    };
  };

  /*
   * Given an image and zoom ratio, return a new image object scaled accordingly
   * if passing an oldImage object, it will pass that to this.createImage().
   */
  Whiteboard.prototype.scaleImage = function( imageStruct, zoomRatio, oldImage ) {
    var image = this.createImage( Math.round(imageStruct.width * zoomRatio), Math.round(imageStruct.height * zoomRatio), oldImage );

    image.ctx.drawImage(
      imageStruct.image,

      // source
      0,
      0,
      imageStruct.width,
      imageStruct.height,

      // destination
      0,
      0,
      image.width,
      image.height
    );

    return image;
  };

  /*
   * Take this.fullsizeImage and scale it down and set this.scaledImage
   */
  Whiteboard.prototype.updateScaledImage = function() {
    this.scaledImage = this.scaleImage( this.fullsizeImage, this.zoomRatio, this.scaledImage );
  }


  /*
   * Create a pattern using the main whiteboard context.
   * this is used when drawing the grey background.
   */
  Whiteboard.prototype.createPattern = function() {
    var patternImg = document.getElementById('undefined-background'),
        pattern = this.whiteboardCtx.createPattern( patternImg, 'repeat' );

    return pattern;
  };

  /*
   * resize the whiteboard canvas to the size of the window.
   */
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
    this.handle( 'mousewheel', this.handleMouseWheel.bind(this) );

    window.addEventListener( 'keydown', this.handleKeyDown.bind(this) );

    // resize the canvas when the window is resized.
    window.onresize = function() {
      this.resizeCanvasToWindow();
      this.dirtyBuffer = true;
      return true;
    }.bind(this);
  };

  Whiteboard.prototype.drawLoop = function() {
    window.requestAnimFrame( this.drawLoop.bind(this) );

    // if the buffer is not dirty, then no need to redraw anything.
    if ( ! this.dirtyBuffer || this.batchDrawing ) { return; }

    // mark buffer as not dirty
    // do this as soon as possible so a future update doesn't mark this as dirty before the screen is updated
    // if a bug crops up where there are race-conditions, then it might be better to use an int rather than a bool
    // to ensure that we can keep things in sync.
    // better to accidently do 1 more frame update than to skip one and miss data on the frontend
    this.dirtyBuffer = false;

    this.redraw(); // update the screen
  };

  Whiteboard.prototype.redraw = function() {
    // the width/height of the visible section of the source image
    // based on the whiteboard's dimensions (with zoom taken into account)

    this.whiteboardCtx.fillStyle = this.pattern; // only need to set this once. OPTIMIZATION
    this.whiteboardCtx.fillRect( 0, 0, this.whiteboard.width, this.whiteboard.height );
    this.updateScaledImage();

    this.whiteboardCtx.drawImage(
      this.scaledImage.image,

      0,
      0,
      this.scaledImage.width,
      this.scaledImage.height,

      this.scrollX,
      this.scrollY,
      this.scaledImage.width,
      this.scaledImage.height
    )

  };

  Whiteboard.prototype.translateFromLocalToFullsize = function( x, y ) {
    return {
      x: ( x - this.scrollX ) / this.zoomRatio,
      y: ( y - this.scrollY ) / this.zoomRatio
    };
  };

  /*
   *  scroll deltas
   *  this uses actual image size values
   */
  Whiteboard.prototype.scroll = function( dx, dy ) {
    this.scrollX += dx;
    this.scrollY += dy;

    this.scrollTo( );
  };

  Whiteboard.prototype.scrollTo = function( x, y ) {
    if ( typeof x === 'undefined' ) { x = this.scrollX; }
    if ( typeof y === 'undefined' ) { y = this.scrollY; }

    this.scrollX = Math.ceil( x );
    this.scrollY = Math.ceil( y );

    this.dirtyBuffer = true;
  };

  /*
   * set the zoomRatio to newZoom
   * originPoint is the center point for the zoom
   * if it's omitted, just zoom from the center of the canvas.
   */
  Whiteboard.prototype.setZoom = function( newZoom, originPoint ) {
    var scaledOrigin = { x: 0, y: 0 }, // the origin point on the scaled image
        zoomDelta = null;              // the % difference between current zoom and the new zoom

    if ( typeof originPoint === 'undefined' ) { originPoint = this.centerPoint(); }

    // restrict to minimum and maximum zoom levels.
    if ( isNaN( newZoom ) ) { newZoom = 1; } // fail-safe; if zoom is bad, set it to 1.0
    if ( newZoom > this.MAX_ZOOM ) { newZoom = this.MAX_ZOOM; }      // max zoom is 2
    if ( newZoom < this.MIN_ZOOM ) { newZoom = this.MIN_ZOOM; }  // min zoom is .1

    zoomDelta = newZoom / this.zoomRatio;

    // translate the origin point to the point on the new image
    scaledOrigin.x = originPoint.x * zoomDelta;
    scaledOrigin.y = originPoint.y * zoomDelta;

    this.zoomRatio = newZoom;

    this.scrollX -= scaledOrigin.x - originPoint.x;
    this.scrollY -= scaledOrigin.y - originPoint.y;

    this.dirtyBuffer = true;

    return this.zoomRatio;
  };

  /*
   * return the center of the whiteboard in coordinates of the scaledImage
   * returned as a {x: X, y: Y} object
   */
  Whiteboard.prototype.centerPoint = function() {
    var cx = this.whiteboard.width / 2,
        cy = this.whiteboard.height / 2,
        ix = cx - this.scrollX,
        iy = cy - this.scrollY;

    return { x: ix, y: iy };
  };

  // functions for sending things to the server
  // TODO: this should probably be part of the client itself.

  Whiteboard.prototype.sendDrawEvent = function( pointer, x, y ) {
    var translatedCoordinates = this.translateFromLocalToFullsize( x, y );

    this.messageBus.broadcast( 'draw', {
      pointer:  pointer,
      x:        translatedCoordinates.x,
      y:        translatedCoordinates.y,
      penWidth: this.penWidth,
      penColor: this.penColor
    } );
  };

  Whiteboard.prototype.sendPenUp = function() {
    this.messageBus.broadcast( 'pen_up', {} );
  };

  // event receivers
  // for how we handle incoming things

  Whiteboard.prototype.handleKeyboardZoom = function( _messageType, zoomDirection ) {
    var zoomAmount = 0;

    switch( zoomDirection ) {
      case 'in':
        zoomAmount = this.ZOOM_INCREMENT;
        break;

      case 'out':
        zoomAmount = -this.ZOOM_INCREMENT;
        break;

      default:
        console.log('unknown zoom direction: ' + zoomDirection);
        // do nothing
    }

    this.setZoom( this.zoomRatio + zoomAmount );
  };

  // receive pen event from server
  Whiteboard.prototype.handleUpdate = function( messageType, message ) {

    // draw on fullsize, then scale down.
    var ctx = this.fullsizeImage.ctx,
        last = this.penStatuses[message.userId];

    ctx.beginPath();
    if ( last ) {
      ctx.moveTo(last.x, last.y);
      ctx.lineTo(message.x, message.y);
      ctx.lineCap = "round";
      ctx.lineWidth = message.penWidth;
      ctx.strokeStyle = message.penColor;
      ctx.stroke();
    }

    this.penStatuses[message.userId] = { x: message.x, y: message.y };

    this.dirtyBuffer = true;
  };

  // receive pen-up from server
  Whiteboard.prototype.handlePenUp = function( messageType, message ) {
    var userId = message.userId;

    delete this.penStatuses[userId];
  };


  Whiteboard.prototype.handlePinchToZoom = function( touches ) {

    var t1        = { x: touches[0].clientX, y: touches[0].clientY },
        t2        = { x: touches[1].clientX, y: touches[1].clientY },
        dt1       = { x: t1.x - this.scrollX, y: t1.y - this.scrollY },
        dt2       = { x: t2.x - this.scrollX, y: t2.y - this.scrollY },

        boxWidth  = Math.abs(t1.x - t2.x),
        boxHeight = Math.abs(t1.y - t2.y),
        boxDiag   = Math.sqrt( Math.pow( boxWidth, 2 ) + Math.pow( boxHeight, 2 ) ),

        ot        = this.lastPinchTouches;

    this.lastPinchTouches = [ t1, t2 ];

    // if there were no old touches, then just return.
    if ( ! ot ) { return; }

    var ot1           = ot[0],
        ot2           = ot[1],
        oldWidth      = Math.abs( ot1.x - ot2.x ),
        oldHeight     = Math.abs( ot1.y - ot2.y ),
        oldDiag       = Math.sqrt( Math.pow( oldWidth, 2 ) + Math.pow( oldHeight, 2 ) ),

        newZoom       = this.zoomRatio * ( boxDiag / oldDiag ),

        upperLeftBoxX = Math.min( t1.x, t2.x ),
        upperLeftBoxY = Math.min( t1.y, t2.y ),
        upperLeftOldX = Math.min( ot1.x, ot2.x ),
        upperLeftOldY = Math.min( ot1.y, ot2.y );

    this.scroll( upperLeftBoxX - upperLeftOldX, upperLeftBoxY - upperLeftOldY );
    this.setZoom( newZoom, dt1 );
  };

  // interaction event handers
  // this is stuff like mouse-move, touch events and keyboard events
  // before they get filtered through our internal API.

  Whiteboard.prototype.handleTouchStart = function( event ) {
    event.preventDefault();

    if ( event.touches.length == 2 ) {
      // if there are 2 fingers down, let's save the 2 points that we're at.
      this.zoomTouches = [
        this.translateFromLocalToFullsize( event.touches[0].clientX, event.touches[0].clientY ),
        this.translateFromLocalToFullsize( event.touches[1].clientX, event.touches[1].clientY )
      ];
    }

  };

  Whiteboard.prototype.handleTouchEnd = function( event ) {
    event.preventDefault();

    if ( event.touches.length < 2 ) {
      // there aren't 2 fingers down, clear the cached zoom stuff
      this.zoomTouches = null;
    }

    if ( event.touches.length == 0 ) {
      this.lastPinchTouches = null;
      this.sendPenUp();
    }

  };

  Whiteboard.prototype.handleTouchMove = function( event ) {
    event.preventDefault();

    var touch = event.changedTouches[0];

    if ( event.touches.length == 1 ) {
      this.sendDrawEvent( 'touch', touch.clientX, touch.clientY );
    } else if ( event.touches.length == 2 ) {
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


  Whiteboard.prototype.handleMouseWheel = function( event ) {
    event.preventDefault();

    var dx = event.wheelDeltaX,
        dy = event.wheelDeltaY;

    this.scroll( dx, dy );
  };


  Whiteboard.prototype.handleKeyDown = function( event ) {

    var zoomInCode = 221,  // ]
        zoomOutCode = 219, // [
        panCode = 32;      // space

    if ( event.keyCode === zoomInCode ) { // zoom in
      event.preventDefault();
      this.messageBus.broadcast( 'keyboard_zoom', 'in' );
    } else if ( event.keyCode == zoomOutCode ) { // zoom out
      event.preventDefault();
      this.messageBus.broadcast( 'keyboard_zoom', 'out' );
    }
  };

  window.Whiteboard = Whiteboard;

})( window, document );
