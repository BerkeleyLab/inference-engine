
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

[Overview](#overview) | [Getting Started](#getting-started) | [Documentation](#documentation)

Overview
--------
Inference-Engine supports research in the training and deployment of neural-network surrogate models for computational science.
Inference-Engine also provides a platform for exploring and advancing the native parallel programming features of Fortran 2023 in the context of deep learning.
The language features of interest facilitate loop-level parallelism via the `do concurrent` construct and Single-Program, Multiple Data (SMPD) parallelism via "multi-image" (e.g., multithreaded or multiprocess) execution.
Toward these ends,

* Most Inference-Engine procedures are `pure` and thus satisfy a language requirement for invocation inside `do concurrent`,
* The network training procedure uses `do concurrent` to expose automatic parallelization opportunities to compilers, and
* Exploiting multi-image execution to speedup training is under investigation.

To broaden support for the native parallel features, Inference-Engine's contributors also write compiler tests, bug reports, and patches; develop a parallel runtime library ([Caffeine]); participate in the language standardization process; and provide example inference and training code for exercising and evaluating compilers' automatic parallelization capabilities on processors and accelerators, including Graphics Processing Units (GPUs).

Available optimizers:
* Stochastic gradient descent and
* Adam (recommended).

Supported network types:
* Feed-forward networks and
* Residual networks (for inference only).

Supported activation functions:
* Sigmoid,
* RELU,
* GELU,
* Swish, and
* Step (for inference only).

Please submit a pull request or an issue to add or request other optimizers, network types, or activation functions.

Getting Started
---------------

### Examples and demonstration applications
The [example](./example) subdirectory contains demonstrations of several relatively simple use cases.
We recommend reviewing the examples to see how to handle basic tasks such as configuring a network training run or reading a neural network and using it to perform inference.

The [demo](./demo) subdirectory contains demonstration applications that depend on Inference-Engine but build separately due to requiring additional prerequisites such as NetCDF and HDF5.
The demonstration applications
 - Train a cloud microphysics model surrogate for the Intermediate Complexity Atmospheric Research ([ICAR]) package,
 - Perform inference using a pretrained model for aerosol dynamics in the Energy Exascale Earth System ([E3SM]) package, and
 - Calculate ICAR cloud microphysics tensor component statistics that provide useful insights for training-data reduction.

### Building and Testing
Because this repository supports programming language research, the code exercises new language features in novel ways.
We recommend using any compiler's latest release or even building open-source compilers from source.
The [handy-dandy] repository contains scripts capturing steps for building the [llvm-project] compiler.
The remainder of this section contains commands for building Inference-Engine with a recent Fortran compiler and the Fortran Package Manager ([`fpm`]) in your PATH.

#### GNU (`gfortran`) 13 or higher required
```
fpm test --compiler gfortran --profile release
```

#### NAG (`nagfor`)
```
fpm test --compiler nagfor --flag -fpp --profile release
```

#### LLVM (`flang-new`)
Building with `flang-new` requires passing flags to enable the compiler's experimental support for assumed-rank entities:
```
fpm test --compiler flang-new --flag "-mmlir -allow-assumed-rank -O3"
```
We recommend building `flang-new` from source.

##### _Experimental:_ Automatic parallelization of `do concurrent` on CPUs
With the `amd_trunk_dev` branch of the [ROCm fork] fork of LLVM, automatic parallelization currently works for inference, e.g.
```
fpm run \
  --example concurrent-inferences \
  --compiler flang-new \
  --flag "-mmlir -allow-assumed-rank -O3 -fopenmp -fdo-concurrent-parallel=host" \
  -- --network model.json

```
where `model.json` must be a neural network in the [JSON] format used by Inference-Engine and the companion [nexport] package.
Automatic parallelization for training is under development.

#### Intel (`ifx`)
```
fpm test --compiler ifx --profile release --flag -O3
```

##### _Experimental:_ Automatic offloading of `do concurrent` to GPUs
This capability is under development with the goal to facilitate automatic GPU offloading via the following command:
```
fpm test --compiler ifx --profile releae --flag "-fopenmp-target-do-concurrent -qopenmp -fopenmp-targets=spir64 -O3"
```

#### HPE (`crayftn.sh`) -- under development
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

### Configuring a training run
Inference-Engine imports hyperparameters and network configurations to and from JSON files.
To see the expected file format, run [example/print-training-configuration.F90] as follows:
```
% fpm run --example print-training-configuration --compiler gfortran
```
which should produce output like the following:
```
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
Inference-Engine's JSON file format is fragile: splitting or combining lines breaks the file reader.
Files with added or removed white space or reordered whole objects ("hyperparameters" or "network configuration") should work.
A future release will leverage the [rojff] JSON interface to allow for more flexible file formatting.

### Training a neural network
Executing the command below will first build the named example program will train a neural network to learn the saturated mixing ratio function that is one component of the ICAR SB04 cloud microphysics model:
```
 fpm run --example learn-saturated-mixing-ratio --compiler gfortran --profile release -- --output-file sat-mix-rat.json
```
The latter command first builds and runs the named example program.
The following is representative output after 3000 epochs:
```
 Initializing a new network
         Epoch | Cost Function| System_Clock | Nodes per Layer
         1000    0.79896E-04     4.8890      2,4,72,2,1
         2000    0.61259E-04     9.8345      2,4,72,2,1
         3000    0.45270E-04     14.864      2,4,72,2,1
```
The above example program will halt execution either after a threshold cost function of `1.E-08`, which requires many million of epochs, or if the program detects a file named `stop` in the source tree root directory.
Before halting, the program will print a table of saturated mixing ratio values across a range of input pressures and temperatures wherein both inputs have been mapped to the unit interval [0,1].
The program also writes the resulting neural network to the named file `sat-mix-rat.json`.

### Performing inference
Users with a PyTorch model may use [nexport] to export the model to JSON files that Inference-Engine can read.
Examples of performing inference using a neural-network JSON file are in [example/concurrent-inferences].

Documentation
-------------
Please see our [GitHub Pages site] for HTML documentation generated by [`ford`] or generate documentaiton locally by installing `ford` and executing `ford ford.md`.


[E3SM]: https://e3sm.org
[example/print-training-configuration.F90]: example/print-training-configuration.F90
[example/concurrent-inferences]: example/concurrent-inferences
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[`fpm`]: https://github.com/fortran-lang/fpm
[GitHub Pages site]: https://berkeleylab.github.io/inference-engine/ 
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/main/src
[ICAR]: https://github.com/NCAR/icar
[JSON]: https://www.json.org/json-en.html
[nexport]: https://go.lbl.gov/nexport
[ROCm fork]:
[rojff]: https://gitlab.com/everythingfunctional/rojff
[Overview]: #overview)
[Getting Started]: #getting-started
[Documentation]: #documentation
[Building and testing]: #building-and-testing
