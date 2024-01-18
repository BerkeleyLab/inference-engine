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

install_fpm_from_source()
{
  echo "Installing fpm in following location: $PREFIX/bin"
  echo "Ensure $PREFIX/bin is in your path or rerun script with the --prefix=PREFIX flag"
  if ! command -v curl > /dev/null ; then
    echo "This script uses curl to download source file for fpm to install it"
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
echo "The example/ subdirectory contains sample use cases.  Execute the following"
echo "command to see a list of examples that you can run:"
echo ""
echo "fpm run --example"
echo ""
echo "Then execute an example with a command of the form"
echo ""
echo "fpm run --profile release --example <name>"
echo ""
echo "where '--profile release' ensures an optimized build. Replace <name> with any"
echo "name from the previously generated list. The example program will print usage"
echo "information if additional arguments are required."
