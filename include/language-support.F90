! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef F2023_LOCALITY
#if defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 202400)
# define F2023_LOCALITY 1
#endif
#endif

#ifndef F2018_LOCALITY
#if defined(_CRAYFTN)
# define F2018_LOCALITY 1
#endif
#endif

#ifndef MULTI_IMAGE_SUPPORT
#if defined(_CRAYFTN) || defined(__GFORTRAN__) || defined(__INTEL_COMPILER) || defined(NAGFOR)
# define MULTI_IMAGE_SUPPORT 1
#endif
#endif
