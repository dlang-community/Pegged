# Folder `pegged/dev`

The files used for Pegged development. Also, some files for possible, yet-to-come, extensions.

## `regenerate.d`

Regenerates `parser.d` according to the Pegged grammar in `examples/peggedgrammar.d`. Depends on an existing and functioning `parser.d`.

```
rdmd -I../.. regenerate.d
```
