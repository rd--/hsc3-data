# img-to-sf

Convert an `RGB` image to a 32-bit floating point `NeXT/AU` sound file.

The sound file will have as many channels as there are rows in the
image, and as many frames as there are columns.

There are a variety of reduction rules to translate the `RGB` pixel
values to `GS` values.
