// module Main

exports.audioStreamP =
  function audioStreamP(constant) {

		// Create context + analyser
	  var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
	  var analyser = audioCtx.createAnalyser();
	  analyser.fftSize = 4096;

		navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia;
		

	  // Connect microphone to analyser
	  navigator.getUserMedia(
			{audio: true},
	    connect,
	    () => console.error('doh')
		);

		var dataArray = new Uint8Array(0);
		var out = constant(dataArray);

		function connect (mediaStream) {
			var source = audioCtx.createMediaStreamSource(mediaStream);
			source.connect(analyser);

			var bufferLength = analyser.frequencyBinCount;


			function feedSignal () {
			  dataArray = new Uint8Array(bufferLength);
				analyser.getByteFrequencyData(dataArray);

				out.set(dataArray);
				window.requestAnimationFrame(feedSignal);
			}
			window.requestAnimationFrame(feedSignal);
		}


    return function() {
      return out;
    };
  };
