# Architectural Changes/Ideas
- [x] Compile time executed sections of the code need to be done in order
  - Could be better but it works

- [x] IR/Bytecode re-write is mostly done
  - Need to implement register spills
  - Next steps are to add some level of optimizations

- [x] Operator code is still ugly, but better

- [x] Should probably split compiler loop and per-ast dispatches into separate files
  - Compiler.cpp needs to shrink (too much going on)
  - Needs to be more obvious what code already exists

- [ ] Clean up the headers
  - Currently compiler.h is massive (can be unclear which systems are related)
  - Splitting utility is likely a good idea (if you need to include more things to do more things then its more obvious when code is creating a lot of baggage, currently utility just includes a bunch of stuff and you can do a lot memory-wise without really thinking about it)

- [ ] Re-consider how constant values are passed around/calculated
  - [ ] What type is a function. We need to make sure the function type itself is passed around as a constant. This might just be a label for now but passing a "structure" around will mean if we want to add more stuff later (like an overload set) we can.
  - [x] currently trying to remove the concept of each ast node holding constants. It makes sense for some nodes but not others
  - [x] Ideally any node should be able to ping off a constant to be evaluated and then be able to receive that value

- [ ] Memory
  - The system heap allocates all over the place, it would be nice to get all our memory sorted nicely into sections
  - Will need to be done on a system-by-system basis since changing the `Array` structure itself would pose issues for things relying on the pointer arithmetic
  - [x] Array view have helped

- [ ] AST Iteration
  - Recursion works but iteration could be better.
  - is 2 passes enough? Forward and backward dfs iteration? AST_LOCAL is somewhat setup for this since it could be an offset
    - Idea: doubly linked list built into the ast that we create during parsing (tried this before and I cant remember why it didnt work)
    - Idea: allocate an array of all nodes as we parse and then only iterate over that
  - if 2 passes is not enough then what is enough? Are we stuck with recursion in some cases?
  - Recursion will always be fastests but it has a memory limit. Could we do some recursion and then "save" and return up again