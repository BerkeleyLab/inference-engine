Deep learning benchmarks
========================

This subdirectory contains stripped-down, stand-alone versions the inference and training algorithms employed in inference-engine.

Building
--------

### GNU
```
fpm test
```

### NAG
```
fpm test --compiler --flag -fpp
```

### LLVM Flang
```
fpm test --compiler flang-new
```
