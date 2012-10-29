---
title:  A beginner’s experience with iOS audio programming
author: Jonas Westerlund
tags:   ios, c, objective-c, audio, fm, synthesis
---

For some time, I've been wanting to create a musical synthesizer of some kind, for iOS.
Ideally for the iPad, since it makes a perfect control surface and might even be playable as a serious instrument.
Alas, I only have an iPhone, so that's where I had to start.

Being new to iOS development, and to any non-trivial audio programming, a bit of initial research was needed.
There are quite a few audio frameworks available, each of them focusing on a particular domain.

The [Audio Unit framework](http://developer.apple.com/library/ios/#documentation/AudioUnit/Reference/AudioUnit_Framework/_index.html#//apple_ref/doc/uid/TP40007295) is the best fit for writing a synthesizer.
It is the lowest-level framework and gives you all the control and performance you need for real-time audio synthesis.
I read many blog posts that made this framework sound really scary and difficult, but it is actually very straightforward.

## Getting started

Setting up the Audio Unit requires a bit of boring boilerplate, but it's very easy to understand.
I had some trouble finding a minimal working example, so if you are lazy like me and just want to get up and running, I have included mine here.
This is from a method that initialises an Audio Unit, most of it is self-explanatory or uninteresting, so comments are sparse.

```c
AudioComponent outputComponent;
OSStatus status;
char enableIO = 1;

AudioComponentDescription desc = {
  .componentType          = kAudioUnitType_Output,
  .componentSubType       = kAudioUnitSubType_RemoteIO,
  .componentManufacturer  = kAudioUnitManufacturer_Apple,
  .componentFlags         = 0,
  .componentFlagsMask     = 0
};

outputComponent = AudioComponentFindNext(NULL, &desc);
NSAssert(outputComponent, @"Can't find default output", NULL);

status = AudioComponentInstanceNew(outputComponent, &audioComponent);
NSAssert(audioComponent, @"Error creating unit: %li", status);

status = AudioUnitSetProperty(audioComponent,
                              kAudioOutputUnitProperty_EnableIO,
                              kAudioUnitScope_Output,
                              0,
                              &enableIO,
                              sizeof(enableIO));

NSAssert(audioComponent, @"Error enabling output: %li", status);

// This is the audio format that we want to use, more on that later
AudioStreamBasicDescription format = {
  .mSampleRate        = 44100,
  .mBytesPerPacket    = sizeof(int),
  .mFramesPerPacket   = 1,
  .mBytesPerFrame     = sizeof(int),
  .mChannelsPerFrame  = 1,
  .mBitsPerChannel    = 8 * sizeof(int),
  .mReserved          = 0,
  .mFormatID          = kAudioFormatLinearPCM,
  .mFormatFlags       = kAudioFormatFlagsAudioUnitCanonical
};

status = AudioUnitSetProperty(audioComponent,
                              kAudioUnitProperty_StreamFormat,
                              kAudioUnitScope_Input,
                              0,
                              &format,
                              sizeof(AudioStreamBasicDescription));

NSAssert(status == noErr, @"Error setting stream format: %li", status);

AURenderCallbackStruct callback = {
  // This function receives a list of audio buffers to be filled with samples
  .inputProc        = &synth_produce_samples
  // This is a pointer to some object you want to use in aforementioned function
  .inputProcRefCon  = synthesizer
};

status = AudioUnitSetProperty(audioComponent,
                              kAudioUnitProperty_SetRenderCallback,
                              kAudioUnitScope_Global,
                              0,
                              &callback,
                              sizeof(AURenderCallbackStruct));

NSAssert(status == noErr, @"Error setting callback: %li", status);

status = AudioUnitInitialize(audioComponent);
NSAssert(status == noErr, @"Error initializing unit: %li", status);

status = AudioOutputUnitStart(audioComponent);
NSAssert(status == noErr, @"Error starting unit: %li", status);
```

That's it, now all you need to do is generate some sample data in the `RenderTone` function.
To illustrate the fundamental idea, here's an example that outputs silence:

```c
OSStatus synth_produce_samples(void *irc, AudioUnitRenderActionFlags *flags,
                               const AudioTimeStamp *ts, unsigned long bus,
                               unsigned long framec, AudioBufferList *data) {
  // `irc` is the value of `.inputProcRefCon` from before
  int *buf = data->mBuffers[0].mData;
  // We are being asked to put `framec` samples in the audio buffer
  for (int i = 0; i < framec; ++i)
    buf[i] = 0;
  return noErr;
}
```

This is more or less the only thing the framework asks of you; what you put in the buffers is entirely up to you.
Instead of silence, you can now implement some fun synthesis algorithm.

## What about the audio format?

You may be wondering what format we are actually using now, `kAudioFormatFlagsAudioUnitCanonical` doesn't disclose much.
Well, unless you already ⌘‑clicked on it in Xcode, you impatient reader you.

The iPhone's native audio format uses 8.24 fixed-point integers.
This designation is a bit misleading, as it is signed and the most significant bit is the sign bit.
So you have 7 bits for the integer part, and 24 bits for the fractional part.
In other words, `1 << 24` is 1. Just like with floating point samples, the final values for output should stay between -1 and 1 to avoid clipping.

While it is possible to set the audio format up for floating point processing, this comes with severe performance penalties:

* The iPhone hardware is simply much better at crunching integers, despite fancy [NEON](http://www.arm.com/products/processors/technologies/neon.php) extensions.
* Using floating point samples means that each sample must be converted before output, adding quite a bit of overhead.

You will be generating at least 44100 samples per second, so these things add up quickly.
Just stick with the native format, and enjoy the huge performance advantage.
Ideally, you should avoid floating point completely in your audio processing code, as mixing them will cause costly stalls.

## Tips and tricks

I make no claims that any of these tricks are new, clever, or optimal.
But being a novice, I had to come up with them anyway.
I mention them here in the hope that they help or inspire other newcomers, and maybe more or better tricks will appear in the comments.

First of all, cheat as much and as often as possible.
My particular project is an [FM](http://en.wikipedia.org/wiki/Frequency_modulation_synthesis) synthesizer, so naturally there are sine waves all over the place.
The period of `sin x` is usually `2π`, and that's what I used first of all, when I was still using floating point.
But it doesn't really matter, so just pick something that lets you cheat more.
I ended up using an `unsigned short` for the oscillator phase, which can just be incremented until it wraps, and that is the cycle.
It still has adequate resolution even for the lowest audible frequencies.

The previous trick makes custom `sin` functions a little bit easier to implement, and you will need at least one such function.
There is no optimal function; you will have to decide which tradeoffs are acceptable to you.
It might be beneficial to have several of them, and use a faster, less accurate one for things that do not affect audio quality so much.

Representing frequencies in hertz feels like a natural thing to do, but you might as well define them in samples per `1 / samplerate` second.
Incrementing the phase is then just `phase += frequency`.

Using a high sample rate is obviously good, giving you more headroom before things start to fold and cause [aliasing](http://en.wikipedia.org/wiki/Aliasing#Sample_frequency) and trouble in general.
But not everything in the synthesizer needs it.
For example, human ears can't detect tiny changes in amplitude, so 12 bits is more than enough for that.
With 12 bits of amplitude, updating it 44100 times per second seems a bit excessive.
I have not yet made any attempt to find the optimal rate, so I currently update it every 256th sample, which sounds good and is easy to implement.
[Low-frequency oscillators](http://en.wikipedia.org/wiki/Low-frequency_oscillation) and other components can also run at a much slower rate without degrading perceived sound quality.

After a few slow and messy envelope implementations, I came up with a pretty nice reformulation.
For a standard 4-stage [ADSR envelope](http://en.wikipedia.org/wiki/Synthesizer#ADSR_envelope), I used 4 pairs of `unsigned short`, for each pair of level and rate.
For the rate, I made sure that `0 < rate <= MAX`, where `MAX` is the maximum level (`1 << 12` in my representation).
This lets them perform everything from super slow to instant transitions.
Updating the envelope then just involves incrementing the current value with the difference of the target level and itself, multiplied by the rate.
This works for both positive and negative slopes, and when the envelope goes from a partial release to attack and things like that.
When the increment is 0, the envelope proceeds to the next stage, with the exception of sustain, which only proceeds when the key is released.

To get a nice and fast logarithmic curve, you can make use of the ARM [CLZ](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0068b/CIHJGJED.html) instruction, available in both Clang and GCC in the form of `int __builtin_clz(unsigned int x)`.

My initial floating point prototype, which did very little, used around 12% of the CPU.
After implementing some of these tricks, it dropped to something barely detectable (between 0% and 1%, using the Activity Monitor template in Instruments, which is quite coarse).
While 12% might not sound like much, that was for a single voice; polyphony would be quite limited with such terrible performance, and it would quickly gulp down battery power.

## Concluding thoughts

Don't believe anyone who says the Audio Unit framework is difficult or complicated or frighteningly low-level,
it does what is needed and no more, letting you focus on your audio adventures with few restrictions.
After the somewhat tedious setup, you will forget that it's even there.
Of course, it offers a lot more beyond what I have used and covered here, but the rest of it seems just as straightforward and thoughtfully designed.

I was not totally convinced that using fixed-point would be worth the trouble, initially.
Observing the performance improvements even after a quick, unoptimized port from floating point removed all doubt.
While it is a bit tricky at first, it gives you a lot of flexibility, letting you mix and match different types and precisions based on the characteristics and requirements of specific components.
It's almost like it's made for cheating.
