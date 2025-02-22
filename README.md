# gmsv_surffix
A ~~copypaste~~ port of Momentum Mod's surf/ramp fix to Garry's Mod.

## Isn't this redundant when its already apart of [holylib](https://github.com/RaphaelIT7/gmod-holylib)?
Yes, but not everyone should have to install a massive library module (that requires extra setup) for one fix.

It gives me room to add things that are removed compared to the original code, if not add more that isn't already added
(e.g. more rngfix/slopefix fixes).

I also am not a fan of trying to contribute to something with no clearly defined license, as it implies all rights
reserved, whereas this project just uses the Source SDK License.

## TODO
> [!CAUTION]
> This is by far not production ready.

- [sourcesdk-minimal fix PR](https://github.com/danielga/sourcesdk-minimal/pull/49) (cannot compile unless manually fixed)
- Windows 32bit and Linux signatures
- Figure out why cvars are invisible (and also force sv_bounce to be visible)
- See if noclip workaround from SM plugin is needed
