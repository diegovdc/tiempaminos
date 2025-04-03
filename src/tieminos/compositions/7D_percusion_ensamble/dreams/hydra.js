
// You can either use `@latest` or load a specific version with, for example, `@0.4.0`.
await loadScript(
  'https://cdn.jsdelivr.net/npm/hydra-midi@latest/dist/index.js'
)

// Use midi messages from all channels of all inputs.
await midi.start({ channel: '*', input: '*' })
// Show a small midi monitor (similar to hydra's `a.show()`).
midi.show()

noise()
  .kaleid(10)
  .mult(osc(5,0.1,3).kaleid())
  .modulateRotate(noise(), 0.2)
  .out()


// https://hydra.ojack.xyz/?sketch_id=nesso_0
// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/

//clouds of passage
//by Nesso
//www.nesso.xyz

shape([4,5,6].fast(0.1).smooth(1),0.000001,[0.2,0.7].smooth(1))
.color(0.2,0.4,0.3)
.scrollX(()=>Math.sin(time*0.27))
.add(
  shape([4,5,6].fast(0.1).smooth(1),0.000001,[0.2,0.7,0.5,0.3].smooth(1))
  .color(0.6,0.2,0.5)
  .scrollY(0.35)
  .scrollX(()=>Math.sin(time*0.33)))
.add(
  shape([4,5,6].fast(0.1).smooth(1),0.000001,[0.2,0.7,0.3].smooth(1))
  .color(0.2,0.4,0.6)
  .scrollY(-0.35)
  .scrollX(()=>Math.sin(time*0.41)*-1))
.add(
      src(o0).shift(0.001,0.01,0.001)
      .scrollX([0.05,-0.05].fast(0.1).smooth(1))
      .scale([1.05,0.9].fast(0.3).smooth(1),[1.05,0.9,1].fast(0.29).smooth(1))
      ,0.85)
.modulate(voronoi(10,2,2))
.out()


// testing midi
// You can either use `@latest` or load a specific version with, for example, `@0.4.0`.
await loadScript(
  'https://cdn.jsdelivr.net/npm/hydra-midi@latest/dist/index.js'
)

// Use midi messages from all channels of all inputs.
await midi.start({ channel: '*', input: '*' })
// Show a small midi monitor (similar to hydra's `a.show()`).
midi.show()

// Use any note to control the red amount of hydra's `solid()` function.
//solid(note('*'), 0, 1).out()

// Or, if you are using a midi controller and not a keyboard:
// Use a control change value to control the red amount.
 solid(cc(74), 0, 1).out()


//////////////////////
//
//
// You can either use `@latest` or load a specific version with, for example, `@0.4.0`.
await loadScript(
	'https://cdn.jsdelivr.net/npm/hydra-midi@latest/dist/index.js'
)

// Use midi messages from all channels of all inputs.
await midi.start({
	channel: '*',
	input: '*'
})
// Show a small midi monitor (similar to hydra's `a.show()`).
midi.show()

//s0.initScreen()


// cc
blackFade = 74

noise()
	.kaleid(10)
	.mult(osc(5, 0.1, 3)
		.kaleid())
	.modulateRotate(noise(), 0.2)
	.out(o1)

src(o1)

	.add(src(o2)
		.scale(() => Math.sin(time * 0.7) * 0.1 + 1, () => Math.sin(time * 0.8) * 0.1 + 1), 0.1)
	.blend(src(o2), 0.5)
	.modulate(voronoi()
		.saturate(1.2), 0)
// 	.add(src(o2)
// 		.saturate(5)
// 		.modulateRotate(noise()), 0.01)
// 	.add(shape(7)
// 		.mult(osc(1, 1.5, 0.9)
// 			.kaleid())
// 		.scrollX(0, 0.1)
// 		.scrollY(0, 0.1)
// 		.modulateRotate(noise(1)), 0.1)
// 	.add(shape(7)
// 		.scale(0.5)
// 		.mult(osc(1, 0.5, 0.9)
// 			.kaleid())
// 		.scrollX([0, 0.5, 0.3], 0.1)
// 		.scrollY(0, 0.1)
// 		.modulateRotate(noise(1)), 0.1)
// 	.add(shape(7)
// 		.repeat([3, 1, 5, 10])
// 		.mult(osc(1, 0.5, 0.9)
// 			.kaleid())
// 		.scrollX(0, 0.03)
// 		.scrollY(0, -0.1), 0.1)

.modulate(src(o2), 0.2)

.hue()
.saturate(1.5)
.add(src(o2).scale(2), 1.2)
  .mult(solid(.7, 0.5, 0.5))

// 	.modulate(src(o2), 0.2)
	//.mult(solid(1, 0.5, 1))
	//.mult(solid(), cc(blackFade))
	.out(o2)

src(o2)
	.add(src(s0)
		.modulate(o0, 0.01)
		.scale(0.9 * 16 / 9, 0.9), 0.3)

	.out()


//shape(7).repeat().out()
