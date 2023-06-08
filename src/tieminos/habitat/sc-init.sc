//https://doc.sccode.org/Reference/EmacsEditor.html
(
Can.init;
// Server.default.options.inDevice_("Scarlett 18i20 USB");
// Server.default.options.outDevice_("Scarlett 18i20 USB");
o = Server.default.options;
o.memSize= 512000*20;
o.maxNodes=128*1024;
s.options.maxLogins = 8;
o.numInputBusChannels = 32;
o.numOutputBusChannels = 58;
s.latency = 0.01;
s.waitForBoot({
	().play;
	s.makeGui; // `l` to show meter https://doc.sccode.org/Classes/Server.html
    // TODO figure out a way to mix in all outs for the spectrogram... or something
//    {In.ar(0)}.spectrogram;
});
)
