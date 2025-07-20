# Zag Compiler

**Zag** is a compiler for the experimental programming language **Zag**, features a native x86-64 Linux backend.
It is meant to integrate with the C runtime library, so a C compiler is required.

## Building

```bash
./build.sh
```
will compile the release build into `build/release/zag`.
Use the `debug` command with the build script for a debug build.
Set the `CC` environment for your C compiler of choice, defaults to `cc`.

### QBE

**Zag** also supports [QBE](https://c9x.me/compile/) as a lightweight backend.
To build with QBE support, one can modify the `ZAG_CFLAGS` environment variable when building with
```bash
ZAG_CFLAGS="-DQBE_BUILD" ./build.sh
```
By default, the compiler will default to the `qbe` executable unless otherwise specified
by the `ZAG_QBE` environment variable or `--qbe-binary` flag.

The compiler will default to a qbe target specific to the host platform.
To view available targets and the default target for your platform, use the `-t list` flag.

## Usage

Once built, you can compile a Zag source file with
```bash
zag my_program.zag
```
This produces a relocatable object file and an executable.

Since the compiler depends on the C runtime, it requires a C compiler.
It defaults to the `cc` executable if the `CC` environment variable or `--cc-binary` flag are not set.

Use the `-h` flag for more options.

## The language

```zag
// this is a comment

// external declarations
// only external function declarations are allowed to have variadic parameters
extern fn printf(fmt: *u8, ...);

// top level declarations live for the life of the program
var g = 100;

// the entry point, just like C, is the main function
fn main() i32 {
    
    // here, `a` is a signed 32-bit integer
    var a: i32;

    // we can define `a` naturally
    a = 1;

    // definition with a declaration
    var b: i32 = 2;

    // here the type of `c` is inferred to be i32
    var c = b;

    // integer literals default to signed 32-bit integers
    var d = 4;

    // however, they are coerced based on their context
    var e: i64 = 5;
    // note that this is not casted, the full signed 64-bit range is allowed
    
    // character literals, follow same type rules as integer literals
    var ch = 'a';
    var newline = '\n';
    
    // the builtin integer types are i8, u8, i16, u16, i32, u32, i64, u64

    // a pointer
    var p: *i64 = &e;
    
    // an array
    // the type of `arr` evaluates to [3]i32
    var arr = []i32{1, 2, 3};
    
    // a string is null turminated and has type [<len>]u8
    var str = "Hello, World!\n";    // in this case, [15]u8

    // arrays do not implicitly decay to pointers,
    // instead, use the reference operator
    var arr_ptr = &arr;     // has type *i32

    // arrays and pointers can be indexed
    var x1 = arr[0];
    var x2 = arr_ptr[0];
    
    // pointers can be dereferenced
    var x3 = *arr_ptr;

    // the following are valid operators
    //
    // for now the resulting type of each expression 
    // is the type of the operands, which must match
    
    // math
    a + b;
    a - b;
    a * b;
    a / b;
    a % b;
    ++a;
    --a;
    a++;
    a--;
    -a;
    
    // comparison
    a == b;
    a != b;
    a < b;
    a > b;
    a <= b;
    a >= b;
    
    // bitwise
    a | b;
    a & b;
    a ^ b;
    ~a;

    // shift
    a << b;
    a >> b;

    // logical
    !a;

    // blocks
    {
        // `zz` only exists within this scope
        var zz: i64;
    }
    // `zz` can no longer be used here

    // control flow

    if a == b {
        // this section executes when the condition
        // is not zero
    } else {
        // this section executes when the condition
        // is zero
    }

    while a < b {
        ++a;
        // this section executes as long as the condition
        // is not zero
    }

    // finally, we print hello world with printf externed earlier
    printf(&"Hello, world!\n");
    
    // exit code
    return 0;
}
```

---

Zag is a work in progress, expect sharp edges.
