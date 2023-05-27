# Thursday Painter

## Backend

Run on Apple Silicon:

```bash
export C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi"
cabal run --ghc-option="`pkg-config --cflags libffi`" CabalExample
```
