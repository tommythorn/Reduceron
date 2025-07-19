# Blink ULX3S LED

## Connecting ULX3S

You need to connect to USB1 (US1) connector to the host computer in order to program the board.

<img src="https://raw.githubusercontent.com/ulx3s/quick-start/master/images/ulx3s-usb1.jpg?raw=true" width="400">

## Download programmer

Download fujprog for your OS [latest version of fujprog](https://github.com/kost/fujprog/releases).

Extract fujprog to "any-folder" on your drive

```
MacOS: export PATH=[path-to-fujprog]:$PATH
Linux: export PATH=[path-to-fujprog]:$PATH
Windows Powershell: $ENV:PATH = "[path-to-fujprog];" + $ENV:PATH
Windows cmd.exe: PATH=[path-to-fujprog];%PATH%
```

You may want to rename fujprog name, for example on linux just rename fujprog-v46-linux-x64 to fujprog


```
cd path-to-fujprog
mv fujprog-v46-linux-x64 fujprog
```

## Uploading

Upload blink led for your board version

```
git clone https://github.com/ulx3s/blink.git
cd blink
fujprog blink_85f.bit
```

On linux you may need to add udev rule

https://github.com/emard/ulx3s/blob/master/doc/MANUAL.md#programming-over-usb-port-us1

## Build your own blink LED

This blinky is based on the [ULX3S-Blinky project](https://github.com/DoctorWkt/ULX3S-Blinky) from @Doctorwkt. This example blinks an LED using FPGA code.

It is assumed the yosys, nextpnr toolchain has been already installed. If not, see [instructions here](https://github.com/emard/ulx3s/blob/master/doc/MANUAL.md#precompiled-opensource-tools-for-all-platforms)

Default size in Makefile is 85F - you can change it here:

https://github.com/ulx3s/blink/blob/main/Makefile#L69

```
make clean
make ulx3s.bit
make prog
```

# Next steps

Now, you're ready for the next steps, we suggest following:

  - https://github.com/emard/ulx3s/blob/master/doc/MANUAL.md
  - https://github.com/emard/ulx3s-misc
  - https://github.com/ulx3s/fpga-odysseus
  - https://github.com/emard/ulx3s-examples
  - https://github.com/emard/ulx3s-bin
  - https://github.com/kost/ulx3s-ghdl-examples/

Or explore different projects available at: https://ulx3s.github.io/

# Chat and support

Discord chanel

  - https://discord.gg/qwMUk6W (problems/question/general chat)

Gitter chanel

  - https://gitter.im/ulx3s/Lobby (Focused on development)
