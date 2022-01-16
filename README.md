# Axle
Compiler written in C++

Meant to be a low level systems/games language

Main aims:
- Out of order compilation
- Compile time execution
- As little undefined behaviour as possible
- Nice syntax
- Simple dependency management


Long term aims:
- Remove standard library usage (eventually remove all of it)
- Remove dynamic memory allocation (currently most of the compiler is based on this and it needs to be changed)
- Flatten call hierarchy (a lot of tree-based things at the moment)
- Multithreadding
