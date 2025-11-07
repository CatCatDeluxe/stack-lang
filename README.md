# stack_lang
(better name pending)

This project is an interpreter for my own stack-based language. The "specification" can be found here: https://esolangs.org/wiki/FUnctional_staCK.

The only control flow features of the language are pattern matching and recursion, making it almost like a functional language.

## Main features

- VS Code extension for syntax highlighting
- Nice error messages
- Functional-style pattern matching
- Easy way to register Zig functions as globals
- REPL and debugger

Some example code:

```lua
-- Hello world

-- (the language does not have strings, for now)
72 101 108 108 111 32 87 111 114 108 100 33 10

{ -- start a function
| char: -- capture the top of the stack as the local `char`
  @ -- push the currently executing function to the stack
  ! -- call the top of the stack
  char putch! -- write the character to stdout
| : -- if there is no value, just end the function
}!
```

More examples are in `examples/`.

## How to use it

After you build the project with `zig build`, the binary is located in `zig-out/bin/stack_lang`. To build an optimized binary (the optimized version runs the benchmark in about 250ms on my machine, while the unoptimized one takes probably 200 seconds), use `zig build -Doptimize=ReleaseFast`.

To run a file, simply use `stack_lang <file>`. To open the REPL run it without any arguments. You can also open the debugger on a file with `stack_lang -d <file>`.

## Current issues/limitations

- Cyclic references are not detected. However, I haven't found a scenario in which one is created yet.
- Probably lots of memory leaks I haven't found yet
- Once the interpreter encounters a type error or the last branch of a match fails, it may be put into an unrecoverable execution state. This is fine for running regular files (the interpreter exits on error anyways) but it isn't very good still. Eventually I'll add better ways to recover the state.
- Many runtime errors don't include proper line information. This is just a matter of passing more information to more functions.

## Future plans

- Better user experience for the debugger/repl (show symbol names, view code of functions)
- Better error messages
- A way to import/export definitions between files
- String support (maybe just syntax sugar over linked lists)
- Option to disable ANSI coloring the output, for old/bad terminal emulators
- More compiler optimizations
- Cache return values for function matches

## Dependencies

- [Zig](https://ziglang.org/) 0.15.1
- [flags](https://github.com/joegm/flags) Zig package (should be autodownloaded on build)
- A terminal emulator capable of reading ANSI escape codes
- Linux (other operating systems may work, but have not been tested.)