# HRIR data — MIT KEMAR

`kemar-compact.bin` / `.json` are packed from the **MIT KEMAR compact
HRIR set** by Bill Gardner and Keith Martin (MIT Media Laboratory, 1994):

> Gardner, W. G., and Martin, K. D. "HRTF measurements of a KEMAR
> dummy-head microphone." MIT Media Lab Perceptual Computing Technical
> Report #280, 1994. <https://sound.media.mit.edu/resources/KEMAR.html>

The data is provided free of charge with no restrictions on use,
provided the authors are cited when it is used. Cite them in any paper,
release note, or program that ships sound made with this spatializer.

128 taps, 44.1 kHz, 368 measurements: elevations −40°…+90°, azimuths
sweeping the right hemisphere (the left is the same data with the ears
swapped). ITD and head shadow are in the measurements themselves.

Repack from the original archive with:

    curl -O https://sound.media.mit.edu/resources/KEMAR/compact.tar.Z
    uncompress -c compact.tar.Z | tar x
    node ../hrir-pack.mjs compact .
