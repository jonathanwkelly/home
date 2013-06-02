---
title:        Mac OS X MIDI mischief
description:  Exploring the MIDI protocol for fun and profit
author:       Jonas Westerlund
tags:         audio, c, c++, mac-os-x, midi, objective-c
---

The <abbr title="Musical Instrument Digital Interface">[MIDI](http://en.wikipedia.org/wiki/MIDI)</abbr> protocol was created in the early eighties,
as digital musical instruments were becoming increasingly common.
Earlier, analog devices often used a simplistic method of control called [<abbr title="Control Voltage">CV</abbr>/gate](http://en.wikipedia.org/wiki/CV/Gate).
The new, digital instruments were more complex, and a standardized way for these devices to communicate was needed.
MIDI filled this need, and did it so well that it is still in use today, more than 30 years later.
<abbr title="Open Sound Control">[OSC](http://en.wikipedia.org/wiki/Open_Sound_Control)</abbr> can be used as an alternative to MIDI,
but is not nearly as universal.

In this post, I will demonstrate the basics of [CoreMIDI](https://developer.apple.com/library/mac/documentation/MusicAudio/Reference/CACoreMIDIRef/MIDIServices/),
using it to let a MIDI keyboard control a basic software synthesizer written in C++.
In doing so, we'll come across a couple of standard MIDI messages, and learn how to interpret them.
The [source code is available on GitHub](https://github.com/nlogax/phase-osx/tree/polyblep),
and is being used as a testbed for synthesizer algorithms.
It is not pretty or well-organized, but contains everything needed to get up and running.

## Connecting the MIDI devices

We will need a MIDI client with an input port.
The appropriately named `MIDIClientCreate`{.c} function will create the client for us:

```c
MIDIClientRef   midiClient;
MIDIPortRef     inputPort;
OSStatus        status;

status = MIDIClientCreate(CFSTR("MIDI client"), NULL, NULL, &midiClient);
NSCAssert(status == noErr, @"Error creating MIDI client: %d", status);
```

Now we use `MIDIInputPortCreate`{.c} to create an input port through which our client can receive incoming MIDI messages:

```c
status = MIDIInputPortCreate(midiClient, CFSTR("MIDI input"), midiInputCallback, NULL, &inputPort);
NSCAssert(status == noErr, @"Error creating MIDI input port: %d", status);
```

With those two pieces in place, we can now connect the external MIDI keyboard to out MIDI client.
For brevity, let's just connect every device we can get our hands on:

```c
ItemCount numOfDevices = MIDIGetNumberOfDevices();

for (ItemCount i = 0; i < numOfDevices; i++) {
  MIDIEndpointRef src = MIDIGetSource(i);
  MIDIPortConnectSource(inputPort, src, NULL);
}
```

## Decoding the MIDI messages

You may have noticed an argument named `midiInputCallback`{.c} in the call to `MIDIInputPortCreate`{.c}.
This parameter has the type `MIDIReadProc`{.c}, which is defined as `typedef void
(*MIDIReadProc)(const MIDIPacketList *pktlist, void *readProcRefCon, void *srcConnRefCon)`{.c}.
In other words, a function pointer pointing to whatever function you would like to be responsible for incoming MIDI messages.

Here's an example of what such a function might look like:

```c
void midiInputCallback(const MIDIPacketList *list,
                       void *procRef,
                       void *srcRef) {
  for (UInt32 i = 0; i < list->numPackets; i++) {
    const MIDIPacket *packet = &list->packet[i];
```

The function receives a list of MIDI packets, and `list->numPackets`{.c} tells us how many there are.
Next, we loop over the packets and inspect each packet individually:

```c
    for (UInt16 j = 0, size = 0; j < packet->length; j += size) {
      UInt8 status = packet->data[j];

      if      (status <  0xC0)  size = 3;
      else if (status <  0xE0)  size = 2;
      else if (status <  0xF0)  size = 3;
      else if (status <  0xF3)  size = 3;
      else if (status == 0xF3)  size = 2;
      else                      size = 1;
```

The first byte of a MIDI message is called the *status* byte.
It is usually followed by one or more *data* bytes,
the exact number depends on the type of message.
For playing an instrument, *note on* and *note off* are probably the most important messages,
here's how they are structured:

```c
      switch (status & 0xF0) {
        case 0x80:
          NSLog(@"Note off %d %d", packet->data[j+1], packet->data[j+2]);
          break;

        case 0x90:
          NSLog(@"Note on %d %d", packet->data[j+1], packet->data[j+2]);
          break;
      }
```

Each of these messages have two data bytes,
in both cases the first data byte is the MIDI note number,
and the second byte is the velocity.

To convert a note number, which ranges from 0 to 127,
to a frequency, you can simply do something like:

```c
sample_t freq = 440.0 * std::pow(2.0, (key - 69) / 12.0) / kSynthesizerSampleRate
```

With these two messages, we can already do what CV/gate usually does,
with the addition of both initial and release velocity.
There are many more MIDI messages, but they all follow the same pattern: a status byte optionally followed by data bytes.

## Further reading

- [An overview of the most common MIDI messages](http://www.midi.org/techspecs/midimessages.php)
- [CoreMIDI reference documentation](https://developer.apple.com/library/mac/documentation/MusicAudio/Reference/CACoreMIDIRef/MIDIServices/)
- [Example analog-style synthesizer in C++](https://github.com/nlogax/phase-osx/tree/polyblep/lib)
