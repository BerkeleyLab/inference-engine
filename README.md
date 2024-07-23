
```ascii
  _        __                                                     _            
 (_)      / _|                                                   (_)           
  _ _ __ | |_ ___ _ __ ___ _ __   ___ ___         ___ _ __   __ _ _ _ __   ___ 
 | | '_ \|  _/ _ \ '__/ _ \ '_ \ / __/ _ \  __   / _ \ '_ \ / _` | | '_ \ / _ \
 | | | | | ||  __/ | |  __/ | | | (_|  __/ |__| |  __/ | | | (_| | | | | |  __/
 |_|_| |_|_| \___|_|  \___|_| |_|\___\___|       \___|_| |_|\__, |_|_| |_|\___|
                                                             __/ |             
                                                            |___/              
```

![GitHub manifest version](https://img.shields.io/github/manifest-json/v/BerkeleyLab/inference-engine)
![GitHub branch checks state](https://img.shields.io/github/checks-status/BerkeleyLab/inference-engine/main)
[![GitHub issues](https://img.shields.io/github/issues/BerkeleyLab/inference-engine)](https://github.com/BerkeleyLab/inference-engine/issues)
[![GitHub license](https://img.shields.io/github/license/BerkeleyLab/inference-engine)](https://github.com/BerkeleyLab/inference-engine)
![GitHub watchers](https://img.shields.io/github/watchers/BerkeleyLab/inference-engine?style=social)

Inference-Engine
================

Table of contents
-----------------

- [Overview](#overview)
- [Downloading, Building and testing](#downloading-building-and-testing)
- [Examples](#examples)
- [Documentation](#documentation)

Overview
--------

Inference-Engine supports research in concurrent, large-batch inference and training of deep, feed-forward neural networks. Inference-Engine targets high-performance computing (HPC) applications with performance-critical inference and training needs.  The initial target application is _in situ_ training of a cloud microphysics model proxy for the Intermediate Complexity Atmospheric Research ([ICAR]) model.  Such a proxy must support concurrent inference at every grid point at every time step of an ICAR run.  For validation purposes, Inference-Engine also supports the export and import of neural networks to and from Python by the companion package [nexport]. 

The features of Inference-Engine that make it suitable for use in HPC applications include

1. Implementation in Fortran 2018.
2. Exposing concurrency via 
  - `Elemental`, implicitly `pure` inference procedures,
  - An `elemental` and implicitly `pure` activation strategy, and
  - A `pure` training subroutine,
2. Gathering network weights and biases into contiguous arrays for efficient memory access patterns, and
3. User-controlled mini-batch size facilitating `in situ` training at application runtime.
  
Making Inference-Engine's `infer` functions and `train` subroutines `pure` facilitates invoking those procedures inside Fortran `do concurrent` constructs, which some compilers can offload automatically to graphics processing units (GPUs).  The use of contiguous arrays facilitates spatial locality in memory access patterns.  User control of mini-batch size facilitates in-situ training at application runtime.

The available optimizers for training neural networks are
1. Stochastic gradient descent
2. Adam (recommended)

Build and Test
--------------
With the [Fortran Package Manager] (`fpm`) and a recent version of a Fortran compiler installed, enter one of the commmands below to build the Inference-Engine library and run the test suite:

### GNU (`gfortran`) 13 or higher required
```
fpm test --profile release
```

### Intel (`ifx`)
```
fpm test --compiler ifx --profile release --flag -O3
```

#### _Experimental:_ Automatic offloading of `do concurrent` to GPUs
This capability is under development with the goal to facilitate automatic GPU offloading via the following command:
```
fpm test --compiler ifx --profile releae --flag "-fopenmp-target-do-concurrent -qopenmp -fopenmp-targets=spir64 -O3"
```

### LLVM (`flang-new`)
Building with `flang-new` requires passing flags to enable the compiler's experimental support for assumed-rank entities:
```
fpm test --compiler flang-new --flag "-mmlir -allow-assumed-rank -O3"
```
A script that might help with building `flang-new` from source is in the [handy-dandy] repository.


### NAG (`nagfor`)
```
fpm test --compiler nagfor --flag -fpp --profile release
```

### HPE (`crayftn.sh`) -- under development
Support for the Cray Compiler Environment (CCE) Fortran compiler is under development.
Building with the CCE `ftn` compiler wrapper requires an additional trivial wrapper
shell script. For example, create a file `crayftn.sh` with the following contents and
place this file's location in your `PATH`:
```
#!/bin/bash

ftn "$@"
```
Then execute
```
fpm test --compiler crayftn.sh
```

Examples
--------
The [example](./example) subdirectory contains demonstrations of several intended use cases.

Configuring a Training Run
--------------------------
To see the format for a [JSON] configuration file that defines the hyperparameters and a new network configuration for a training run, execute the provided training-configuration output example program:
```
% ./build/run-fpm.sh run --example print-training-configuration
Project is up to date
 {
     "hyperparameters": {
         "mini-batches" : 10,
         "learning rate" : 1.50000000,
         "optimizer" : "adam"
     }
 ,
     "network configuration": {
         "skip connections" : false,
         "nodes per layer" : [2,72,2],
         "activation function" : "sigmoid"
     }
 }
```
As of this writing, the JSON file format is fragile.  Because an Intel `ifx` compiler bug prevents using our preferred JSON interface, [rojff], Inference-Engine currently uses a very restricted JSON subset written and read by the [sourcery] utility's `string_t` type-bound procedures.  For this to work, it is important to keep input files as close as possible to the exact form shown above.  In particular, do not split, combine or reorder lines. Adding or removing whitespace should be ok.

Documentation
-------------
Please see the Inference-Engine GitHub Pages [site] for HTML documentation generated by [`ford`].

[site]: https://berkeleylab.github.io/inference-engine/ 
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[nexport]: https://go.lbl.gov/nexport
[ICAR]: https://github.com/NCAR/icar
[JSON]: https://www.json.org/json-en.html
[sourcery]: https://github.com/sourceryinstitute/sourcery
[rojff]: https://gitlab.com/everythingfunctional/rojff
[install `fpm`]: https://fpm.fortran-lang.org/install/index.html#install
[Fortran Package Manager]: https://github.com/fortran-lang/fpm
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/main/src/fresh-llvm-build.sh
