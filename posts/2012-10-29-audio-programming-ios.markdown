---
title:  A beginner’s experience with iOS audio programming
author: Jonas Westerlund
tags:   audio, c, fixed-point, fm, ios, objective-c, synthesis
---

I have long wanted to create a musical synthesizer of some kind, for iOS.
For the iPad, more specifically, since it makes for a perfect control surface, and might even be playable as a serious instrument.
But, since I don't own an iPad, I made an iPhone app instead.
It makes no difference for the audio code, and that is what this post is about.

Being new to iOS development, and to any non-trivial audio programming, I had a brief look at the various audio-related frameworks available.

For writing a synthesizer, the [Audio Unit framework](http://developer.apple.com/library/ios/#documentation/AudioUnit/Reference/AudioUnit_Framework/_index.html) seems to fit the bill.
It is the lowest-level framework and imposes few restrictions, so we have complete freedom when designing the synthesizer.
I read a few blog posts that made this framework sound really scary and difficult.
As we will see, it's actually very straightforward.

## Getting started

Setting up the Audio Unit requires a bit of boring boilerplate, which is fortunately easy to understand.
I had some trouble finding a minimal working example, so in case you are lazy like me, I have included the relevant parts of my initialisation code.
Most of it is self-explanatory or uninteresting, so comments are sparse.

```c
AudioComponent outputComponent;
OSStatus status;
int enableIO = 1;

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

// This is the audio format that will be used, more on that later
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
  // This is the function that will be called by the framework to produce samples
  .inputProc        = &synth_produce_samples
  // This is a pointer to some object we might want to use in the aforementioned function
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

That's it, now we only have to write the `synth_produce_samples` function, and generate some sample data.
To illustrate the fundamental idea, here's a function that outputs silence:

```c
OSStatus synth_produce_samples(void *irc, AudioUnitRenderActionFlags *flags,
                               const AudioTimeStamp *ts, unsigned long bus,
                               unsigned long framec, AudioBufferList *data) {
  // The `irc` parameter is the `.inputProcRefCon` from before
  int *buf = data->mBuffers[0].mData;
  // The `framec` parameter is the number of samples we must produce
  for (int i = 0; i < framec; ++i)
    buf[i] = 0;
  return noErr;
}
```

This is more or less the only thing the framework asks of us; what we put in the buffers is entirely up to us.
Instead of silence, we can now implement some fun synthesis algorithm.

## Audio format

We still don't know what format our samples should use, and `kAudioFormatFlagsAudioUnitCanonical` doesn't disclose much.
Well, unless you already ⌘‑clicked on it in Xcode.

The iPhone native audio format uses 8.24 fixed-point integers.
It is signed, leaving us 7 bits for the integer part, and 24 bits for the fractional part.
In other words, `1 << 24` is 1. Just like with floating point samples, the final values for output should stay between -1 and 1 to avoid clipping.

While it is possible to set the audio format up for floating point processing, this comes with severe performance penalties:

* The iPhone hardware is simply much better at crunching integers, despite fancy [NEON](http://www.arm.com/products/processors/technologies/neon.php) extensions.
* Using floating point samples means that each sample must be converted before output, adding quite a bit of overhead.

We will be generating at least 44100 samples per second, so these things add up quickly.
For these reasons, we will stick to the native format, and enjoy the performance advantage.
Ideally, the audio processing code should be completely free of floating point, to avoid costly stalls.

## Tips and tricks

For programmers more experienced in audio synthesis, most of these tricks are likely either obvious or obviously suboptimal.
I will publish them anyway, in the hope that they will help or at least inspire other newcomers.

If you can think of any improvements, or other useful tricks, I would love to know!
Either submit a comment, or [edit this post](https://github.com/nlogax/home/blob/master/posts/2012-10-29-audio-programming-ios.markdown) and submit a pull request.

### Sine wave oscillators

My particular project is an [FM](http://en.wikipedia.org/wiki/Frequency_modulation_synthesis) synthesizer, so naturally there are sine waves all over the place.
The period of `sin x` is usually `2π`, and that's what I used first of all, when I was still using floating point.
However, since it is only used for keeping track of the oscillator phase, we can use something that gives us more opportunities for cheating.
I ended up using an `unsigned short` for the phase, which is incremented until it wraps, signifying a cycle.
It still has adequate resolution even for the lowest audible frequencies.

The previous trick makes custom `sin` functions a little bit easier to implement, and we'll need at least one such function.
There is no one optimal function; we will have to decide which tradeoffs to make for specific components.
It might be beneficial to have several of them, and use a faster, less accurate one for components that do not affect audio quality very much.

### Frequencies

Representing frequencies in hertz feels like a natural thing to do, but we might as well define them in samples per `1 / samplerate` second.
Incrementing the phase is then simplified to `phase += frequency`.

### Sample rates and bit depths

Using a high sample rate is obviously good, giving us more headroom before things start to fold, causing [aliasing](http://en.wikipedia.org/wiki/Aliasing#Sample_frequency) and trouble in general.
A high bit depth is also desirable, letting us represent a wider number of values.
But not every component in a synthesizer needs it.
For example, human ears can't detect tiny changes in amplitude, so 12 bits is more than enough for that.
With 12 bits of amplitude, updating it 44100 times per second seems a bit excessive.
[Low-frequency oscillators](http://en.wikipedia.org/wiki/Low-frequency_oscillation) and other components can also run at a much slower rate without degrading perceived sound quality.

### Envelopes

After a few slow and messy [ADSR envelope](http://en.wikipedia.org/wiki/Synthesizer#ADSR_envelope) implementations, I came up with a pretty nice reformulation.
Instead of each stage having a duration, it has a rate of change.
Yes, it's the same thing, but at least for me it helped to forget about the actual durations.
With this implementation, we only need to ensure that `0 < rate <= MAX`.
This lets the envelope perform everything from super slow to instant transitions, and updating it just involves incrementing the current value with the rate, until it reaches the target level.
This works for both positive and negative slopes, and when the envelope goes from a partial release to attack and things like that.
When the target level is reached, the envelope proceeds to the next stage, with the exception of sustain, which only proceeds once the key is released.

<span class="updated">Updated <time datetime="2012-11-06">november 6, 2012</time>:</span>
I came up with another implementation that I like better.
Rather than incrementing the level by rate, always increment by 1, and let the rate be the number of samples between envelope updates.
This way, slower transitions will cause the envelope to update less frequently, while fast ones will update often and sound better.
This will also ensure that the envelope always hits the target level exactly, simplifying conditionals.
With a minimum rate of 1 and a maximum amplitude of 4096, this means we must special-case the very highest rates.
To maintain the desirable properties, these can be implemented by shifting the increment one bit for each rate, so that the fastest rate increments the level by 4096, giving a 1-sample transition.

### ARM instructions

Have a glance at the [ARM instruction set](http://infocenter.arm.com/help/topic/com.arm.doc.qrc0001m/QRC0001_UAL.pdf) when there's no obvious way to implement something efficiently.
There might not be an instruction that will solve the problem, but maybe one that will make us approach the problem from another angle.

For example, we can make use of the [CLZ](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0068b/CIHJGJED.html) instruction to implement a fast `log2` function.
This instruction is available in both Clang and GCC in the form of `int __builtin_clz(unsigned int x)`, which is preferable over inline assembly, as it will generate code that will run in the simulator as well.

### Fixed-point everywhere

My initial floating point prototype, which did very little, used around 12% of the CPU.
After implementing some of these tricks, CPU usage dropped to something barely detectable (between 0% and 1% using the Activity Monitor template in Instruments, which is quite coarse).
While 12% might not sound like much, that was for a single voice; polyphony would be quite limited with such terrible performance, and it would quickly gulp down battery power.

## Concluding thoughts

Don't believe anyone who says the Audio Unit framework is difficult or complicated or frighteningly low-level.
After the somewhat tedious setup, you will forget that it's even there.
Of course, it offers a lot more beyond what I have used and covered here, but the rest of it seems just as straightforward and thoughtfully designed.

I was not totally convinced that using fixed-point would be worth the trouble, initially.
After observing the performance improvements even after a quick, unoptimized port from floating point, I no longer had any doubts.
While it is a bit tricky at first, it gives us a lot of flexibility, letting us mix and match different types and precisions based on the characteristics and requirements of specific components.
It's almost like it's made for cheating.
