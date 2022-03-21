nasm -g -fwin64 output.nasm

link /LARGEADDRESSAWARE:NO /ENTRY:main /SUBSYSTEM:CONSOLE output.obj ..\lib\kernel32.lib