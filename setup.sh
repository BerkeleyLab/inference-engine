#!/bin/sh

set -e # exit on error

usage()
{
  echo "Inference Engine Setup Script"
  echo ""
  echo "USAGE:"
  echo "./setup.sh [--help|-h] | [-p|--prefix=PREFIX]"
  echo ""
  echo " --help             Display this help text"
  echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
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
brew install fpm cmake netcdf netcdf-fortran pkg-config coreutils # coreutils supports `realpath` below

PREFIX=`realpath $PREFIX`

NETCDF_LIB_PATH="`brew --prefix netcdf`/lib"
HDF5_LIB_PATH="`brew --prefix hdf5`/lib"
NETCDFF_LIB_PATH="`brew --prefix netcdf-fortran`/lib"

FPM_LD_FLAG=" -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH -L$NETCDFF_LIB_PATH"
FPM_FLAG="-fallow-argument-mismatch -ffree-line-length-none -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
FPM_FC=${FC:-"gfortran"}
FPM_CC=${CC:-"gcc"}

mkdir -p build

CI=${CI:-"false"} # GitHub Actions workflows set CI=true

if [ $CI = true ]; then
  PKG_CONFIG_PATH=`realpath ./build/pkgconfig`
  echo "---------------"
  echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"
  echo "---------------"
else
  PKG_CONFIG_PATH=`realpath "$PREFIX"/lib/pkgconfig`
fi

if [ ! -d $PKG_CONFIG_PATH ]; then
  mkdir -p $PKG_CONFIG_PATH 
fi

INFERENCE_ENGINE_PC="$PKG_CONFIG_PATH/inference-engine.pc"
echo "INFERENCE_ENGINE_FPM_CC=\"$FPM_CC\""                  >  $INFERENCE_ENGINE_PC
echo "INFERENCE_ENGINE_FPM_FC=\"$FPM_FC\""                  >> $INFERENCE_ENGINE_PC
echo "INFERENCE_ENGINE_FPM_LD_FLAG=\"$FPM_LD_FLAG\""        >> $INFERENCE_ENGINE_PC
echo "INFERENCE_ENGINE_FPM_FLAG=\"$FPM_FLAG\""              >> $INFERENCE_ENGINE_PC
echo "Name: inference-engine"                               >> $INFERENCE_ENGINE_PC
echo "Description: Inference Engine"                        >> $INFERENCE_ENGINE_PC
echo "URL: https://github.com/berkeleylab/inference-engine" >> $INFERENCE_ENGINE_PC
echo "Version: 0.1.2"                                       >> $INFERENCE_ENGINE_PC
if [ $CI = true ]; then
  echo "---------------"
  echo "cat $INFERENCE_ENGINE_PC"
  cat $INFERENCE_ENGINE_PC
  echo "---------------"
fi

export PKG_CONFIG_PATH
cp scripts/run-fpm.sh-header build/run-fpm.sh
RUN_FPM_SH="`realpath ./build/run-fpm.sh`"
echo "`which fpm` \$fpm_arguments \\" >>  $RUN_FPM_SH
echo "--profile debug \\" >> $RUN_FPM_SH
echo "--c-compiler \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_CC`\" \\" >> $RUN_FPM_SH
echo "--compiler \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_FC`\" \\" >> $RUN_FPM_SH
echo "--flag \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_FLAG`\" \\"  >> $RUN_FPM_SH
echo "--link-flag \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_LD_FLAG`\" \\" >> $RUN_FPM_SH
echo "\$program_arguments" >> $RUN_FPM_SH
chmod u+x $RUN_FPM_SH
if [ $CI = true ]; then
  echo "---------------"
  echo "cat $RUN_FPM_SH"
  cat $RUN_FPM_SH
  echo "---------------"
fi

if command -v fpm > /dev/null 2>&1; then
  brew tap fortran-lang/fortran
  brew install fpm
fi

echo "$RUN_FPM_SH test"
$RUN_FPM_SH test

echo ""
echo "___________________ Inference-Engine has been set up! ______________________" 
echo ""
echo "To run one the programs in the example subdirectory, enter a command of the"
echo "following form at a shell command prompt after replacing <example-base-name>"
echo "with the base name of a file in the example/ subdirectory:"
echo ""
echo "./build/run-fpm.sh run --example <example-base-name>"
