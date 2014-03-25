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

    // the zoom factor
    this.zoomRatio = 1;

    // sizing ( 1080 HDTV size )
    // this has to be < 3MP to work properly on iOS devices
    this.fullsizeImage = this.createImage( this.DEFAULT_WIDTH, this.DEFAULT_HEIGHT );
    this.updateScaledImage();

    this.whiteboardCtx = this.whiteboard.getContext('2d');

    this.pattern = this.createPattern();

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
                          }.bind(this));

                          // .subscribe( 'set_penColor',   this.handleSetPenColor.bind(this) )
                          // .subscribe( 'set_penWidth',   this.handleSetPenWidth.bind(this) );
   return messageBus;
  };

  Whiteboard.prototype.createImage = function( width, height ) {
    var image = document.createElement('canvas'),
        ctx = image.getContext('2d');

    image.width = width;
    image.height = height;

    ctx.fillStyle = '#ffffff';
    ctx.fillRect(0, 0, width, height);

    return {
      width: width,
      height: height,
      image: image,
      ctx: ctx
    };
  };

  Whiteboard.prototype.scaleImage = function( imageStruct, zoomRatio ) {
    var image = this.createImage( imageStruct.width * zoomRatio, imageStruct.height * zoomRatio );

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

  Whiteboard.prototype.updateScaledImage = function() {
    this.scaledImage = this.scaleImage( this.fullsizeImage, this.zoomRatio );
  }


  Whiteboard.prototype.createPattern = function() {
    var patternImg = document.getElementById('undefined-background'),
        pattern = this.whiteboardCtx.createPattern( patternImg, 'repeat' );

    return pattern;
  };

  // resizes the onscreen canvas in the DOM to the size of the window
  Whiteboard.prototype.resizeCanvasToWindow = function() {
    // resize the canvas a bit
    var body = document.getElementsByTagName('body')[0],
        fullsizeWidth = body.offsetWidth,
        fullsizeHeight = window.innerHeight;

    this.whiteboard.width = fullsizeWidth;
    this.whiteboard.height = fullsizeHeight;

    // this.cacheFullsizeDimensions();
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
  };

  Whiteboard.prototype.drawLoop = function() {
    window.requestAnimFrame( this.drawLoop.bind(this) );

    // if the buffer is not dirty, then no need to redraw anything.
    if ( ! this.dirtyBuffer ) { return; }

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

    this.whiteboardCtx.fillStyle = this.pattern;
    this.whiteboardCtx.fillRect( 0, 0, this.whiteboard.width, this.whiteboard.height );

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

    // this.whiteboardCtx.drawImage(
      // this.scaledImage,

      // // where on the source image to draw from
      // this.scrollX < 0 ? 0 : this.scrollX, // ios can't draw negative
      // this.scrollY < 0 ? 0 : this.scrollY,
      // this.fullsizeWidth,
      // this.fullsizeHeight,
      // // (this.width + this.scrollX >= this.width) ? this.width - this.scrollX : this.fullsizeWidth,
      // // (this.height + this.scrollY >= this.height) ? this.height - this.scrollY : this.fullsizeHeight,

      // // where on the local canvas to draw to
      // this.scrollX < 0 ? Math.abs(this.scrollX) : 0,
      // this.scrollY < 0 ? Math.abs(this.scrollY) : 0,
      // this.whiteboard.width,
      // this.whiteboard.height
      // // this.scrollX < 0 ? -(this.scrollX) : 0,
      // // this.scrollY < 0 ? -(this.scrollY) : 0,
      // // (this.width + this.scrollX >= this.width) ? this.width - this.scrollX : this.whiteboard.width,
      // // (this.height + this.scrollY >= this.height) ? this.height - this.scrollY : this.whiteboard.height
    // );

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

    // if ( this.scrollX < -200 ) { this.scrollX = -200 }
    // if ( this.scrollY < -200 ) { this.scrollY = -200 }
    // if ( this.scrollX > this.width - 400 ) { this.scrollX = this.width - 400 }
    // if ( this.scrollY > this.height - 400 ) { this.scrollY = this.height - 400 }

    this.dirtyBuffer = true;
  };

  Whiteboard.prototype.setZoom = function( newZoom ) {
    this.zoomRatio = newZoom;

    // restrict to minimum and maximum zoom levels.
    if ( isNaN(this.zoomRatio) ) { this.zoomRatio = 1; } // fail-safe; if zoom is bad, set it to 1.0
    if ( this.zoomRatio > 2 ) { this.zoomRatio = 2; } // max zoom is 2
    if ( this.zoomRatio < .25 ) { this.zoomRatio = .25; } // min zoom is .1

    this.updateScaledImage();

    this.dirtyBuffer = true;

    return this.zoomRatio;
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
    var zoomAmount = 0,
        centerViewX = this.whiteboard.width / 2,
        centerViewY = this.whiteboard.height / 2,
        preCenterFull  = this.translateFromLocalToFullsize( centerViewX, centerViewY ), // fullsize coordinates before zoom
        postCenterFull = null, // fullsize coordinates after zoom
        dx = null, // scroll amount to re-center
        dy = null; // scroll amount to re-center


    switch( zoomDirection ) {
      case 'in':
        zoomAmount = 0.1;
        break;

      case 'out':
        zoomAmount = -0.1;
        break;

      default:
        console.log('unknown zoom direction: ' + zoomDirection);
        // do nothing
    }

    this.setZoom( this.zoomRatio + zoomAmount );

    postCenterFull = this.translateFromLocalToFullsize( centerViewX, centerViewY );
    dx = preCenterFull.x - postCenterFull.x;
    dy = preCenterFull.y - postCenterFull.y;

    this.scroll( -dx, -dy );
  };

  // receive pen event from server
  Whiteboard.prototype.handleUpdate = function( messageType, message ) {

    // draw on fullsize, then scale down.
    var ctx = this.fullsizeImage.ctx,
        last = this.penStatuses[message.userId];

    // ctx.fillStyle = message.penColor;
    // ctx.fillRect( message.x, message.y, 2, 2 );

    ctx.beginPath();
    if ( last ) {
      ctx.moveTo(last.x, last.y);
      ctx.lineTo(message.x, message.y);
      ctx.lineCap = "round";
      ctx.lineWidth = message.penWidth;
      ctx.strokeStyle = message.penColor;
      ctx.stroke();
    }

    // cap it off
    // ctx.arc(message.x, message.y, message.penWidth / 2, 0,2*Math.PI, false);
    // ctx.fill();

    this.penStatuses[message.userId] = { x: message.x, y: message.y };

    this.updatescaledImage();

    this.dirtyBuffer = true;
  };

  // receive pen-up from server
  Whiteboard.prototype.handlePenUp = function( messageType, message ) {
    var userId = message.userId;

    delete this.penStatuses[userId];
  };


  Whiteboard.prototype.handlePinchToZoom = function( touches ) {

    // calculate zoomRatio by using the box formed by t1 and t2
    // the ratio is the local size / fullsize size
    var vt1        = { x: touches[0].clientX, y: touches[0].clientY },                  // t1, view scale
        vt2        = { x: touches[1].clientX, y: touches[1].clientY },                  // t2, view scale
        ft1        = this.translateFromLocalToFullsize( vt1.x, vt1.y ),                 // t1 full scale
        ft2        = this.translateFromLocalToFullsize( vt2.x, vt2.y ),                 // t2 full scale
        oft1       = this.zoomTouches[0],                                               // original t1, full scale
        oft2       = this.zoomTouches[1],                                               // original t2, full scale
        vboxWidth  = Math.abs( vt1.x - vt2.x ),                                         // width of the box at screen size
        vboxHeight = Math.abs( vt1.y - vt2.y ),                                         // height of box at screen size
        fboxWidth  = Math.abs( oft1.x - oft2.x ),                                       // width of original box at fullsize
        fboxHeight = Math.abs( oft1.y - oft2.y ),                                       // height of original box at full size
        vboxDiag   = Math.sqrt( Math.pow( vboxWidth, 2 ) + Math.pow( vboxHeight, 2 ) ), // distance between screen-size points
        fboxDiag   = Math.sqrt( Math.pow( fboxWidth, 2 ) + Math.pow( fboxHeight, 2 ) ), // distance between original fullsize points
        zoomRatio  = this.setZoom( vboxDiag / fboxDiag ),                               // ratio of screen size to full size (calculate zoom)

        // scroll values:
        // we have original x/y on fullsize
        // lock touchpoint 1's X from original location to that point on the screen
        dx         = oft1.x - ft1.x,
        dy         = oft1.y - ft1.y;

    this.scroll( -dx, -dy );
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
