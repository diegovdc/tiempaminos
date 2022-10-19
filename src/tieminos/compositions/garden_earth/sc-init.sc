//https://doc.sccode.org/Reference/EmacsEditor.html
(
Can.init;
// Server.default.options.inDevice_("Scarlett 18i20 USB");
// Server.default.options.outDevice_("Scarlett 18i20 USB");
o = Server.default.options;
s.options.maxLogins = 8;
o.numInputBusChannels = 4;
o.numOutputBusChannels = 24;
s.waitForBoot({
	().play;
	s.makeGui; // `l` to show meter https://doc.sccode.org/Classes/Server.html
});
)
