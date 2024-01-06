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

if ! command -v brew > /dev/null ; then
  if ! command -v curl > /dev/null ; then
    echo "Please install curl and then rerun ./setup.sh"
    exit 1
  fi
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  if [ $(uname) = "Linux" ]; then
    if [ -z "$PATH" ]; then
      PATH=/home/linuxbrew/.linuxbrew/bin/
    else
      PATH=/home/linuxbrew/.linuxbrew/bin/:"$PATH"
    fi
  fi
fi


brew tap fortran-lang/fortran # required for building fpm
brew install fortran-lang/fortran/fpm

FPM_FLAG="-fcoarray=single -O3"
FPM_FC=${FC:-"gfortran-13"}
FPM_CC=${CC:-"gcc-13"}

mkdir -p build

fpm build --flag ${FPM_FLAG}

echo ""
echo "____________________ Inference-Engine has been set up! _______________________" 
echo ""
echo "To run one of the programs in the example subdirectory, enter a command of the"
echo "following form at a shell command prompt after replacing <example-base-name>"
echo "with the base name of a file in the example/ subdirectory:"
echo ""
echo "fpm run --example <example-base-name>"
