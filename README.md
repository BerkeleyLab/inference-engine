# inference-engine

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

> :memo: **Note:** The badges above will work once the repository goes public.

## Overview

`inference-engine` is a Fortran 2018 library which facillitates the loading of neural network weight and bias arrays and the instantiation of _fixed-sized_ feed-forward neural networks, enabling the user to perform inference on data within Fortran.

Since `inference-engine` is written in Fortran 2018, it is optimized for scientific applications which can benefit from the performance gains associated with the use of accelerated machine learning.

## Table of contents

- [inference-engine](#inference-engine)
  - [Overview](#overview)
  - [Table of contents](#table-of-contents)
  - [Copyright notice](#copyright-notice)

<!-- ## Usage -->

<!-- ## Prerequisites -->

<!-- ## Downloading -->

<!-- ## Building and testing -->

<!-- ## Examples -->

## Copyright notice

inference-engine Copyright (c) 2022, The Regents of the University of
California, through Lawrence Berkeley National Laboratory (subject to
receipt of any required approvals from the U.S. Dept. of Energy).
All rights reserved.

If you have questions about your rights to use or distribute this software,
please contact Berkeley Lab's Intellectual Property Office at
[IPO@lbl.gov](mailto:IPO@lbl.gov).

NOTICE.  This Software was developed under funding from the U.S. Department
of Energy and the U.S. Government consequently retains certain rights.  As
such, the U.S. Government has been granted for itself and others acting on
its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
Software to reproduce, distribute copies to the public, prepare derivative 
works, and perform publicly and display publicly, and to permit others to do so.