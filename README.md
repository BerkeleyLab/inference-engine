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

# Overview
`inference-engine` is a Fortran 2018 library which facillitates the loading of neural network weight and bias arrays and the instantiation of _fixed-sized_ feed-forward neural networks, enabling the user to perform inference on data.

Since `inference-engine` is written in Fortran 2018, it is optimized for scientific applications which can benefit from the performance gains associated with the use of accelerated machine learning.

## Usage

### Required data structure
`inference-engine` can extrapolate the architecture of the intended neural network from the input file. The compilation of the network requires the weight and bias arrays to be constructed using the following data structure:
`4 x (3x5) x 1 network`
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

#### Example data structure
The following data structure represents the first hidden layer of the neural network.
```
[[7.618639323e-09 -2

]]
```

## Prerequisites

## Downloading

## Building and testing

## Examples