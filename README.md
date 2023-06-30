
# iovis

iovis is a fast compiled language with a strong focus on performance and convenience.

## Hello World

A simple Hello World program can be written as such

```rust
fn main() {
    printf("Hello World");
}
```
## Usage/Examples

lib.iov
```rust
extern {
    fn printf(format: string, ..);
    fn scanf(format: string, ..): i32;
    fn fgets(format: string, n: i32, stream: i32): string;
}
```

main.iov

```rust
import lib;

fn main() {
    let input: i32;

    printf("What's your favorite number?\n");
    scanf("%d", &input);

    if input != 12 {
        printf("Wrong");
    } else {
        printf(":-)");
    }
}
```

## SDL2 Bindings

SDL2 Bindings exist and can be found in `test/` along with a simple window application.

![Simple SDL2 App Example](https://xirei.moe/uploader/?f=ajkeex8y.png)
## Features
- [x] Lexer
- [x] Parser
- [x] Rust compiler
- [x] LLVM-IR CodeGen
- [x] Dependency Analysis
- [x] Type Inferencing
- [ ] Arrays
- [ ] Multi-dimensional arrays
- [ ] Standard Library
- [ ] Bug fixes 🐛
## WASM

You can try out a live demo of the interpreted version [here](https://xirei.moe/iovis/). It uses WASM compiled using `wasm-pack` which allows it to parse and interpret at blazingly fast speeds. (not benchmarked)