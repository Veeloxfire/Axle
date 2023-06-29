# Architectural Changes/Ideas
- Compile time executed sections of the code need to be done in order
  - Idea: Have an queue asssociated with each section being compiled
  - Idea: Add a dependency for the previous execution unit if it exists

- IR/Bytecode is going through a rewrite
  - Making the IR simpler by moving all of the memory/register allocation to the backend
  - IR is now only values and constants (and views into them)
  - Its likely not going to compile for a while
  - Value Tree + The graph colouring algorithm needs to be looked over. It seems like a mess but it also works
  - Idea: SSA? Currently values can be modified but what if we changed that so they can't
  - Capturing Values needs to be checked to make sure loops work! (was a bug a long time ago which I never fixed)
  - Idea: putting all the control flow as metadata in the IR serializeation 

- Operator code is ugly
  - Need to remove the weird member function call stuff, it was a good idea but its just annoying to read
  - Also move that code nearer to the other compiling code. its harder to see how much something breaks if some of the changes are hidden in various files

- Should probably split compiler loop and per-ast dispatches into separate files
  - Compiler.cpp needs to shrink (too much going on)
  - Needs to be more obvious what code already exists

- Clean up the headers
  - Currently compiler.h is massive (can be unclear which systems are related)
  - Splitting utility is likely a good idea (if you need to include more things to do more things then its more obvious when code is creating a lot of baggage, currently utility just includes a bunch of stuff and you can do a lot memory-wise without really thinking about it)

- Re-consider how constant values are passed around/calculated
  - What type is a function. We need to make sure the function type itself is passed around as a constant. This might just be a label for now but passing a "structure" around will mean if we want to add more stuff later (like an overload set) we can.
  - currently trying to remove the concept of each ast node holding constants. It makes sense for some nodes but not others
  - Ideally any node should be able to ping off a constant to be evaluated and then be able to receive that value
    - Idea: associate the value with the calling node instead of the called node, this would put the inefficiency only when something may need to make a constant

- Memory
  - The system heap allocates all over the place, it would be nice to get all our memory sorted nicely into sections
  - Will need to be done on a system-by-system basis since changing the `Array` structure itself would pose issues for things relying on the pointer arithmetic

- AST
  - It works but iteration could be better.
  - is 2 passes enough? Forward and backward dfs iteration? AST_LOCAL is somewhat setup for this since it could be an offset
    - Idea: doubly linked list built into the ast that we create during parsing (tried this before and I cant remember why it didnt work)
    - Idea: allocate an array of all nodes as we parse and then only iterate over that
  - if 2 passes is not enough then what is enough? Are we stuck with recursion in some cases?
  - Recursion will always be fastests but it has a memory limit. Could we do some recursion and then "save" and return up again