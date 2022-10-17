```
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

> :memo: **Note:** The badges above will work once the repository goes public.

# Overview
`inference-engine` is a Fortran 2018 library which facillitates the loading of neural network weight and bias arrays and the instantiation of _fixed-sized_ feed-forward neural networks, enabling the user to perform inference on data within Fortran.

Since `inference-engine` is written in Fortran 2018, it is optimized for scientific applications which can benefit from the performance gains associated with the use of accelerated machine learning.

# Table of contents
- [Overview](#overview)
- [Table of contents](#table-of-contents)
- [Usage](#usage)
  - [Required data structure](#required-data-structure)
    - [Example data structure](#example-data-structure)
- [Prerequisites](#prerequisites)
- [Downloading](#downloading)
- [Building and testing](#building-and-testing)
- [Examples](#examples)

# Usage

## Required data structure
`inference-engine` can extrapolate the architecture of the intended neural network from the input file. The compilation of the network requires the weight and bias arrays to be constructed using the following data structure:

> :construction: **Architecture:** 4 x (3x5) x 1 network

```
# input layer
[[weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]]

[bias bias bias bias bias]

# hidden layer 1
[[weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]]

[bias bias bias bias bias]

# hidden layer 2
[[weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]
 [weight weight weight weight weight]]

[bias bias bias bias bias]

# output layer
[[weight]
 [weight]
 [weight]
 [weight]
 [weight]

[bias]
```

### Example data structure
The following data structure represents the first hidden layer of the neural network.
```
[[7.618639323e-09 -2

]]
```

# Prerequisites

# Downloading

# Building and testing

# Examples
