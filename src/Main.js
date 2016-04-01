// module Main

exports.getFFTSize = function (analyser)
{
	return function() {
		console.log("JS: ", analyser.fftSize);
		return analyser.fftSize;
	}
	
}

exports.getByteFrequencyData = function (analyser)
{
	return function() {
		var array = new Uint8Array(analyser.frequencyBinCount);
		console.log("JS: Getting byte frequency data");
		analyser.getByteFrequencyData(array);
		return array;
	}
}

exports.fillArray = function (value, len) {
  if (len == 0) return [];
  var a = [value];
  while (a.length * 2 <= len) a = a.concat(a);
  if (a.length < len) a = a.concat(a.slice(0, len - a.length));
  return a;
}

exports.constNum = function()
{
	return 1;
}

exports.constArray = function()
{
	console.log("Here");
 	return [1, 2, 3];
}

exports.onDOMContentLoaded =
  function onDOMContentLoaded(action) {
    return function() {
      if (document.readyState === "interactive") {
        action();
      } else {
        document.addEventListener("DOMContentLoaded", action);
      }
      return {};
    };
  };

exports.getAudioContext = function()
{
	console.log("Getting audio context");
	var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
	return audioCtx;
	
}

exports.getAnalyserNode = function(ctx)
{
	return function() {
		console.log("Getting analyser");
		return ctx.createAnalyser();	
	}
	
}

exports.microphoneInputFrameP =
  function microphoneInputFrameP(constant) {
  	return function(context) {
	    return function(analyser) {
		    return function() {
		  	  	navigator.getUserMedia({audio: true},
			    connectMediaStreamToAnalyser(constant, context, analyser, drawOntoCanvas),
			    noUserMedia);
		    };
	    };
  	};
};

var WIDTH = 400;
var HEIGHT = 300;

function drawOntoCanvas(constant, analyser){
    var requestAnimFrame, cancelAnimFrame;
    if (window.requestAnimationFrame) {
      requestAnimFrame = window.requestAnimationFrame;
      cancelAnimFrame = window.cancelAnimationFrame;
    } else if (window.mozRequestAnimationFrame) {
      requestAnimFrame = window.mozRequestAnimationFrame;
      cancelAnimFrame = window.mozCancelAnimationFrame;
    } else if (window.webkitRequestAnimationFrame) {
      requestAnimFrame = window.webkitRequestAnimationFrame;
      cancelAnimFrame = window.webkitCancelAnimationFrame;
    } else if (window.msRequestAnimationFrame) {
      requestAnimFrame = window.msRequestAnimationFrame;
      cancelAnimFrame = window.msCancelAnimationFrame;
    } else if (window.oRequestAnimationFrame) {
      requestAnimFrame = window.oRequestAnimationFrame;
      cancelAnimFrame = window.oCancelAnimationFrame;
    } else {
      requestAnimFrame = function(cb) {setTimeout(function() {cb(now())}, 1000/60)};
      cancelAnimFrame = window.clearTimeout;
    }

    function getData(a){
        var array = new Uint8Array(a.frequencyBinCount);
		console.log("JS: Getting byte frequency data");
		a.getByteFrequencyData(array);
		return array;
	}

	var out = constant(getData(analyser))
    requestAnimFrame(function tick(t) {
      out.set(t); requestAnimFrame(tick);
    });
    return out;
};

function ready(fn) {
  if (document.readyState != 'loading'){
    fn();
  } else {
    document.addEventListener('DOMContentLoaded', fn);
  }
}

navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia;

function noUserMedia() {
  console.error("User did not allow media");
}

function connectMediaStreamToAnalyser(constant, audioCtx, analyser, callback) {
  return function(mediaStream) {

    var source = audioCtx.createMediaStreamSource(mediaStream);
    source.connect(analyser);
    callback(constant, analyser);
  }
}

function drawOntoCanvas(analyser) {
  var bufferLength = analyser.frequencyBinCount;
  var canvas = document.getElementById("canvas");
  var canvasCtx = canvas.getContext('2d');
  var dataArray = new Uint8Array(bufferLength);

  function drawAudioData() {
    requestAnimationFrame(drawAudioData);
    analyser.getByteFrequencyData(dataArray);

    canvasCtx.fillStyle = 'rgb(200, 200, 200)';
    canvasCtx.fillRect(0, 0, WIDTH, HEIGHT);
    canvasCtx.lineWidth = 2;
    canvasCtx.strokeStyle = 'rgb(0, 0, 0)';
    canvasCtx.beginPath();
    var sliceWidth = WIDTH * 1.0 / bufferLength;
    var x = 0;

    for(var i = 0; i < bufferLength; i++) {

      var v = dataArray[i] / 128.0;
      var y = v * HEIGHT/2;

      if(i === 0) {
        canvasCtx.moveTo(x, y);
      } else {
        canvasCtx.lineTo(x, y);
      }

      x += sliceWidth;
    }
    canvasCtx.lineTo(canvas.width, canvas.height/2);
    canvasCtx.stroke();
  };

  drawAudioData();
}


// ready(function() {

//   var canvas = document.getElementById("canvas");
//   var canvasCtx = canvas.getContext('2d');
//   canvasCtx.clearRect(0, 0, WIDTH, HEIGHT);


//   if (!navigator.getUserMedia) {
//     console.error("No support for getUserMedia");
//     return;
//   }

//   // Create context + analyser
//   var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
//   var analyser = audioCtx.createAnalyser();
//   analyser.fftSize = 4096;

//   // Connect microphone to analyser


// });