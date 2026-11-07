//https://doc.sccode.org/Reference/EmacsEditor.html


(
var ioDevice = ServerOptions.inDevices.asSet.findMatch(thisProcess.argv[0] ?? "18i20+BH64");

// if I wanted to configure this in the future via the command line I can get the argv from thisProcess.argv

if(ioDevice.notNil,{
	Server.default.options.inDevice_(ioDevice);
	Server.default.options.outDevice_(ioDevice);
}, {
	"ioDevice not found, using system selected ioDevice".postln;
});

Can.init;

o = Server.default.options;
o.memSize = 512000*20;
o.maxNodes = 128*1024;
o.numBuffers = 20000;
s.options.maxLogins = 8;
o.numInputBusChannels = 32;
o.numOutputBusChannels = 84;
s.latency = 0.01;
// s.options.sampleRate = 48000; // would be nice to use but increases latency to a more noticeable level
s.waitForBoot({
	().play;
	s.makeGui; // `l` to show meter https://doc.sccode.org/Classes/Server.html
    // TODO figure out a way to mix in all outs for the spectrogram... or something
//    {In.ar(0)}.spectrogram;
});

)
