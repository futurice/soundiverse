var WIDTH = 400;
var HEIGHT = 300;

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

function connectMediaStreamToAnalyser(audioCtx, analyser, callback) {
  return function(mediaStream) {
    console.log("jumbo")
    var source = audioCtx.createMediaStreamSource(mediaStream);
    source.connect(analyser);
    callback(analyser);
  }
}

function drawOntoCanvas(analyser) {
  var bufferLength = analyser.frequencyBinCount;
  var canvas = document.getElementById("canvas");
  var canvasCtx = canvas.getContext('2d');
  var dataArray = new Uint8Array(bufferLength);

  function drawAudioData() {
    requestAnimationFrame(drawAudioData);
    analyser.getByteTimeDomainData(dataArray);

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
//
//   var canvas = document.getElementById("canvas");
//   var canvasCtx = canvas.getContext('2d');
//   canvasCtx.clearRect(0, 0, WIDTH, HEIGHT);
//
//
//   if (!navigator.getUserMedia) {
//     console.error("No support for getUserMedia");
//     return;
//   }
//
//   // Create context + analyser
//   var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
//   var analyser = audioCtx.createAnalyser();
//   analyser.fftSize = 2048;
//
//   // Connect microphone to analyser
//   navigator.getUserMedia({audio: true},
//     connectMediaStreamToAnalyser(audioCtx, analyser, drawOntoCanvas),
//     noUserMedia);
//
// });
