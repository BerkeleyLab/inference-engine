
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

Inference-Engine supports research in the training and deployment of neural-network surrogates for physics-based models in computational science.
Inference-Engine also provides a platform for exploring language-based parallel programming in the context of deep learning.
This repository's [demo] subdirectory demonstrates
1. Training a cloud microphysics model for the Intermediate Complexity Atmospheric Research ([ICAR]) package,
2. Performing inference using a pretrained model for aerosol dynamics in the Energy Exascale Earth System ([E3SM]) package, and
3. Calculating ICAR cloud microphysics tensor component statics that provide useful insights for training-data reduction.
Inference-Engine leverages loop-level parallelism via the Fortran 2023 `do concurrent` construct, which several compilers automatically parallelize on processors or accelerators, including Graphics Processing Units (GPUs).
Toward this end, most procedures in Inference-Engine are `pure`, which facilitates invocing Inference-Engine procedures inside Fortran's `do concurrent` construct
Several compilers can automatically parallelize `do concurrent` on processors and accelerators, including graphics processing units (GPUs).

Additionally, ongoing research investigates leveraging Fortran's native Single-Program, Multiple-Data (SPMD) parallel programming modle in the form of a Partitioned Global Address Space (PGAS) often termed "Coarray Fortran."

Inference-Engine can import and export neural networks to and from a JSON file format that the comanion package [nexport] can generate from PyTorch models. 

The available optimizers for training neural networks are
1. Stochastic gradient descent
2. Adam (recommended)

Build and Test
--------------
With the [Fortran Package Manager] (`fpm`) and a recent version of a Fortran compiler installed, enter one of the commmands below to build the Inference-Engine library and run the test suite:

### LLVM (`flang-new`)
Building with `flang-new` requires passing flags to enable the compiler's experimental support for assumed-rank entities:
```
fpm test --compiler flang-new --flag "-mmlir -allow-assumed-rank -O3"
```
A script that might help with building `flang-new` from source is in the [handy-dandy] repository.

#### _Experimental:_ Automatic parallelization of `do concurrent` on CPUs
With the `amd_trunk_dev` branch of the AMD ROCm LLVM fork, this capability currently works for inference, e.g.
```
fpm run \
  --example concurrent-inferences \
  --compiler flang-new \
  --flag "-mmlir -allow-assumed-rank -O3 -fopenmp -fdo-concurrent-parallel=host" \
  -- --network model.json

```
where `model.json` must be a neural network in the JSON format used by Inference-Engine and [nexport].

Automatic parallelization for training is under development.

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

[demo]: ./demo
[site]: https://berkeleylab.github.io/inference-engine/ 
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[nexport]: https://go.lbl.gov/nexport
[ICAR]: https://github.com/NCAR/icar
[E3SM]: https://e3sm.org
[JSON]: https://www.json.org/json-en.html
[sourcery]: https://github.com/sourceryinstitute/sourcery
[rojff]: https://gitlab.com/everythingfunctional/rojff
[install `fpm`]: https://fpm.fortran-lang.org/install/index.html#install
[Fortran Package Manager]: https://github.com/fortran-lang/fpm
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/main/src/fresh-llvm-build.sh
