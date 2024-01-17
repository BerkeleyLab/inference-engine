#!/bin/sh

set -e # exit on error

usage()
{
  echo "Inference Engine Setup Script"
  echo ""
  echo "USAGE:"
  echo "./setup.sh [--help|-h]"
  echo ""
  echo " --help             Display this help text"
  echo " --prefix=PREFIX    Install any binaries needed to build inference-engine in 'PREFIX/bin'"
  echo "                    Default prefix='\$HOME/.local/bin'"
  echo ""
}

PREFIX="$HOME/.local"

while [ "$1" != "" ]; do
  PARAM=$(echo "$1" | awk -F= '{print $1}')
  VALUE=$(echo "$1" | awk -F= '{print $2}')
  case $PARAM in
    -h | --help)
      usage
      exit
      ;;
    -p | --prefix)
      PREFIX=$VALUE
      ;;
    *)
      echo "ERROR: unknown parameter \"$PARAM\""
      usage
      exit 1
      ;;
  esac
  shift
done

set -u # error on use of undefined variable

if ! command -v gfortran > /dev/null ; then
  echo "This script assumes usage of gfortran to compile and build"
  echo "When using other Fortran compilers, please ensure you have fpm downloaded and in your path"
  echo "Then use the following command to install inference-engine: fpm build --flag \"-fcoarray=single\"."
  echo "Please replace the coarray flag with appropriate coarray flag for your compiler"
  exit 1
fi

install_fpm_from_source()
{
  echo "Installing fpm in following location: $PREFIX/bin"
  echo "Ensure $PREFIX/bin is in your path or rerun script with the --prefix=PREFIX flag"
  if ! command -v curl > /dev/null ; then
    echo "Need curl to download source file for fpm to install it"
    echo "Please install curl and then rerun ./setup.sh"
    exit 1
  fi
  mkdir temp-dir-to-build-fpm-for-inference-engine-installation
  curl -L -o temp-dir-to-build-fpm-for-inference-engine-installation/fpm.F90 https://github.com/fortran-lang/fpm/releases/download/current/fpm.F90
  gfortran -o $PREFIX/bin/fpm -Jtemp-dir-to-build-fpm-for-inference-engine-installation temp-dir-to-build-fpm-for-inference-engine-installation/fpm.F90
  rm -rf temp-dir-to-build-fpm-for-inference-engine-installation
  if command -v fpm > /dev/null ; then
    echo "fpm installed"
  else
    echo "Some error has occured while trying to install fpm. Please install fpm, ensure it is in your path, and rerun script"
  fi
}

# if no fpm, install either through homebrew, or gfortran compiling fpm.F90
if ! command -v fpm > /dev/null ; then
  if ! command -v brew > /dev/null ; then
    if ! command -v gfortran > /dev/null ; then
      echo "Please install fpm, ensure it is in your path, and rerun script"
      exit 1
    else # has gfortran, but not homebrew
      install_fpm_from_source
    fi
  else # has homebrew
    brew tap fortran-lang/fortran
    brew install fortran-lang/fortran/fpm
  fi
fi

FPM_FC=${FC:-"gfortran-13"}
FPM_CC=${CC:-"gcc-13"}

mkdir -p build

fpm test

echo ""
echo "____________________ Inference-Engine has been set up! _______________________"
echo ""
echo "To run one of the programs in the example subdirectory, enter a command of the"
echo "following form at a shell command prompt after replacing <example-base-name>"
echo "with the base name of a file in the example/ subdirectory:"
echo ""
echo "fpm run --example <example-base-name>"
