//https://doc.sccode.org/Reference/EmacsEditor.html
(
Can.init;
// Server.default.options.inDevice_("Scarlett 18i20 USB");
// Server.default.options.outDevice_("Scarlett 18i20 USB");
o = Server.default.options;
o.numInputBusChannels = 4;
o.numOutputBusChannels = 14;
s.waitForBoot({
	().play;
	s.makeGui; // `l` to show meter https://doc.sccode.org/Classes/Server.html
});
)
