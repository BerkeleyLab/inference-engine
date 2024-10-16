```ascii
___________.__        __          
\_   _____/|__|____ _/  |_  ______
 |    __)  |  \__  \\   __\/  ___/
 |     \   |  |/ __ \|  |  \___ \ 
 \___  /   |__(____  /__| /____  >
     \/            \/          \/ 
```
Fiats: Functional inference and training for surrogates
=======================================================
Alternatively, _Fortran inference and training for science_.

[Overview](#overview) | [Getting Started](#getting-started) | [Documentation](#documentation)

Overview
--------
Fiats supports research on the training and deployment of neural-network surrogate models for computational science.
Fiats also provides a platform for exploring and advancing the native parallel programming features of Fortran 2023 in the context of deep learning.
The design of Fiats centers around functional programming patterns that facilitate concurrency, including loop-level parallelism via the `do concurrent` construct and Single-Program, Multiple Data (SMPD) parallelism via "multi-image" (e.g., multithreaded or multiprocess) execution.
Towards these ends,

* Most Fiats procedures are `pure` and thus satisfy a language requirement for invocation inside `do concurrent`,
* The network training procedure use `do concurrent` to expose automatic parallelization opportunities to compilers, and
* Exploiting multi-image execution to speedup training is under investigation.

To broaden support for the native parallel features, the Fiats contributors also write compiler tests, bug reports, and patches; develop a parallel runtime library ([Caffeine]); participate in the language standardization process; and provide example inference and training code for exercising and evaluating compilers' automatic parallelization capabilities on processors and accelerators, including Graphics Processing Units (GPUs).

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
The [example] subdirectory contains demonstrations of several relatively simple use cases.
We recommend reviewing the examples to see how to handle basic tasks such as configuring a network training run or reading a neural network and using it to perform inference.

The [demo] subdirectory contains demonstration applications that depend on Fiats but build separately due to requiring additional prerequisites such as [NetCDF] and [HDF5].
The demonstration applications
 - Train a cloud microphysics model surrogate for the Intermediate Complexity Atmospheric Research ([ICAR]) package,
 - Perform inference using a pretrained model for aerosol dynamics in the Energy Exascale Earth System ([E3SM]) package, and
 - Calculate ICAR cloud microphysics tensor component statistics that provide useful insights for training-data reduction.

### Building and Testing
Because this repository supports programming language research, the code exercises new language features in novel ways.
We recommend using any compiler's latest release or even building open-source compilers from source.
The [handy-dandy] repository contains scripts capturing steps for building the [LLVM] compiler suite.
The remainder of this section contains commands for building Fiats with a recent Fortran compiler and the Fortran Package Manager ([`fpm`]).

#### Supported Compilers
##### LLVM (`flang-new`)
With LLVM `flang` 20 installed in your `PATH`, build and test Fiats with the installed `flang-new` symlink in order for `fpm` to correctly identify the compiler:
```
fpm test --compiler flang-new --flag "-O3"
```
With LLVM `flang` 19, enable the compiler's experimental support for assumed-rank entities:

```
fpm test --compiler flang-new --flag "-mmlir -allow-assumed-rank -O3"
```

###### _Experimental:_ Automatic parallelization of `do concurrent` on CPUs
With the `amd-trunk-dev` branch of the [ROCm fork] of LLVM, automatically parallelize inference calculations inside `do concurrent` constructs:
```
fpm run \
  --example concurrent-inferences \
  --compiler flang-new \
  --flag "-mmlir -allow-assumed-rank -O3 -fopenmp -fdo-concurrent-parallel=host" \
  -- --network model.json

```
where `model.json` must be a neural network in the [JSON] format used by Fiats and the companion [nexport] package.

Automatic parallelization for training neural networks is under development.

#### Partially Supported Compilers

Fiats release 0.14.0 and earlier support the use of the NAG, GNU, and Intel Fortran compilers.
We are corresponding with these compilers' developers about addressing the compiler issues preventing building newer Fiats releases.

##### NAG (`nagfor`)
```
fpm test --compiler nagfor --flag -fpp --profile release
```

##### GNU (`gfortran`)
Compiler bugs related to parameterized derived types currently prevent `gfortran` from building Fiats versions 0.15.0 or later.
Test and build earlier versions of Fiats build with the following command:
```
fpm test --compiler gfortran --profile release
```

##### Intel (`ifx`)
Compiler bugs related to parameterized derived types currently prevent `gfortran` from building Fiats versions 0.15.0 or later.
Test and build earlier versions of Fiats build with the following command:
```
fpm test --compiler ifx --profile release --flag -O3
```

##### _Experimental:_ Automatic offloading of `do concurrent` to GPUs
This capability is under development with the goal to facilitate automatic GPU offloading via the following command:
```
fpm test --compiler ifx --profile release --flag "-fopenmp-target-do-concurrent -qopenmp -fopenmp-targets=spir64 -O3"
```

#### Under Development
We are corresponding with the developers of the compiler(s) below developers about addressing the compiler issues preventing building newer Fiats releases.

#### HPE Cray Compiler Environment (CCE) (`crayftn.sh`)
Building with the CCE `ftn` compiler wrapper requires an additional trivial wrapper.
For example, create a file `crayftn.sh` with the following contents and place this file's location in your `PATH`:
```
#!/bin/bash

ftn "$@"
```
Then execute
```
fpm test --compiler crayftn.sh
```

### Configuring a training run
Fiats imports hyperparameters and network configurations to and from JSON files.
To see the expected file format, run the [print-training-configuration] example as follows:
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
The Fiats JSON file format is fragile: splitting or combining lines breaks the file reader.
Files with added or removed white space or reordered whole objects ("hyperparameters" or "network configuration") should work.
A future release will leverage the [rojff] JSON interface to allow for more flexible file formatting.

### Training a neural network
Running the following command will train a neural network to learn the saturated mixing ratio function that is one component of the ICAR SB04 cloud microphysics model (see the [saturated_mixing_ratio_m] module for an implementation of the involved function):
```
 fpm run --example learn-saturated-mixing-ratio --compiler gfortran --profile release -- --output-file sat-mix-rat.json
```
The following is representative output after 3000 epochs:
```
 Initializing a new network
         Epoch | Cost Function| System_Clock | Nodes per Layer
         1000    0.79896E-04     4.8890      2,4,72,2,1
         2000    0.61259E-04     9.8345      2,4,72,2,1
         3000    0.45270E-04     14.864      2,4,72,2,1
```
The example program halts execution after reaching a cost-function threshold (which requires millions of epochs) or a maximum number of iterations or if the program detects a file named `stop` in the source-tree root directory.
Before halting, the program will print a table of expected and predicted saturated mixing ratio values across a range of input pressures and temperatures, wherein two the inputs have each been mapped to the unit interval [0,1].
The program also writes the neural network initial condition to `initial-network.json` and the final (trained) network to the file specified in the above command: `sat-mix-rat.json`.

### Performing inference
Users with a PyTorch model may use [nexport] to export the model to JSON files that Fiats can read.
Examples of performing inference using a neural-network JSON file are in [example/concurrent-inferences].

Documentation
-------------
### HTML
Please see our [GitHub Pages site] for Hypertext Markup Languge (HTML) documentation generated by [`ford`] or generate documentation locally by installing `ford` and executing `ford ford.md`.

### UML
Please see the `doc/uml` subdirectory for Unified Modeling Language (UML) diagrams such as a comprehensive Fiats [class diagram] with human-readable [Mermaid] source that renders graphically when opened by browsing to the document on GitHub.

[Building and testing]: #building-and-testing
[Caffeine]: https://go.lbl.gov/caffeine
[class diagram]: ./doc/uml/class-diagram.md
[Documentation]: #documentation
[demo]: demo
[E3SM]: https://e3sm.org
[example]: example
[example/print-training-configuration.F90]: example/print-training-configuration.F90
[example/concurrent-inferences]: example/concurrent-inferences.f90
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[`fpm`]: https://github.com/fortran-lang/fpm
[Getting Started]: #getting-started
[GitHub Pages site]: https://berkeleylab.github.io/fiats/ 
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/main/src
[HDF5]: https://www.hdfgroup.org/solutions/hdf5/
[ICAR]: https://github.com/BerkeleyLab/icar/tree/neural-net
[JSON]: https://www.json.org/json-en.html
[LLVM]: https://github.com/llvm/llvm-project
[Mermaid]: https://mermaid.js.org
[NetCDF]: https://www.unidata.ucar.edu/software/netcdf/
[nexport]: https://go.lbl.gov/nexport
[Overview]: #overview
[ROCm fork]: https://github.com/ROCm/llvm-project
[rojff]: https://gitlab.com/everythingfunctional/rojff
[saturated_mixing_ratio_m]: example/supporting-modules/saturated_mixing_ratio_m.f90
