# gmsv_surffix
A ~~copypaste~~ port of Momentum Mod's surf/ramp fix to Garry's Mod.

## Isn't this redundant when its already apart of [holylib](https://github.com/RaphaelIT7/gmod-holylib)?
Yes, but not everyone should have to install a massive library module (that requires extra setup) for one fix. Along
with the fact that it currently only supports Linux 32bit for its `surffix` module.

It gives me room to add things that are removed compared to the original code, if not add more that isn't already added
(e.g. more rngfix/slopefix fixes).

I also am not a fan of trying to contribute to something with no clearly defined license, as it implies all rights
reserved, whereas this project just uses the Source SDK License.

## TODO
- Linux signatures
- Figure out why cvars are invisible (and also force sv_bounce to be visible)
- See if noclip workaround from SM plugin is needed

## Building
### Requirements
- [Premake](https://premake.github.io/)
- [garrysmod_common](https://github.com/danielga/garrysmod_common) (use `x86-64-support-sourcesdk` branch for 64bit)
- [Visual Studio Build Tools 2022](https://visualstudio.microsoft.com/downloads/) on Windows (MinGW untested)
  - "MSVC v143 - VS 2022 C++ x64/x86 build tools"
  - "C++/CLI support for v143 build tools"
- GNU make on Linux

### Windows 64bit
> [!NOTE]
> Currently requires manually updating `sourcesdk-minimal` in `garrysmod_common` to latest commit on `x86-64-branch`

```
premake5 vs2022
cd projects/windows/vs2022
msbuild surffix.sln /p:Configuration=ReleaseWithSymbols
```

### Windows 32bit
```
set BUILD_32BIT=1
premake5 vs2022
cd projects/windows/vs2022
msbuild surffix.sln /p:Configuration=ReleaseWithSymbols /p:Platform=Win32
```

### Linux 64bit
TODO

### Linux 32bit
TODO
