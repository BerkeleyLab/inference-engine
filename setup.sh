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
    echo "Please install curl and then rerun ./install.sh"
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
brew install opencoarrays fpm cmake netcdf pkg-config coreutils # coreutils supports `realpath` below

PREFIX=`realpath $PREFIX`

mkdir -p build/dependencies
if [ ! -d ./build/dependencies/netcdf-fortran ]; then
  git clone https://github.com/Unidata/netcdf-fortran.git build/dependencies/netcdf-fortran
fi
mkdir -p build/dependencies/netcdf-fortran/build

cd build/dependencies/netcdf-fortran/build
  GCC_VER="12" # This should be replaced with code extracting the version number from Homebrew
  export FC=gfortran-${GCC_VER} CC=gcc-${GCC_VER} CXX=g++-${GCC_VER}
  NETCDF_PREFIX="`brew --prefix netcdf`"
  cmake .. \
    -DNETCDF_C_LIBRARY="$NETCDF_PREFIX/lib" \
    -DNETCDF_C_INCLUDE_DIR="$NETCDF_PREFIX/include"
  make -j4
  sudo make install
cd -

GIT_VERSION=`git describe --long --dirty --all --always | sed -e's/heads\///'`
NETCDF_LIB_PATH="`brew --prefix netcdf`/lib"
HDF5_LIB_PATH="`brew --prefix hdf5`/lib"

FPM_FLAG="-cpp -DUSE_ASSERTIONS=.true."
FPM_FLAG=" $FPM_FLAG -fallow-argument-mismatch -ffree-line-length-none"
FPM_FLAG=" $FPM_FLAG -DVERSION=\\\'$GIT_VERSION\\\'"
FPM_FLAG=" $FPM_FLAG -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
FPM_FC="$FC"
FPM_CC="$CC"

PKG_CONFIG_PATH="$PREFIX"/lib/pkgconfig

if [ ! -d $PKG_CONFIG_PATH ]; then
  mkdir -p $PKG_CONFIG_PATH
fi
cd "$PKG_CONFIG_PATH"
  echo "INFERENCE_ENGINE_FPM_CXX=\"$CXX\""       >  inference-engine.pc
  echo "INFERENCE_ENGINE_FPM_CC=\"$FPM_CC\""     >> inference-engine.pc
  echo "INFERENCE_ENGINE_FPM_FC=\"$FPM_FC\""     >> inference-engine.pc
  echo "INFERENCE_ENGINE_FPM_FLAG=\"$FPM_FLAG\"" >> inference-engine.pc
  echo "Name: inference-engine"                  >> inference-engine.pc
  echo "Description: Inference Engine"           >> inference-engine.pc
  echo "URL: https://github.com/berkeleylab/inference-engine" >> inference-engine.pc
  echo "Version: 0.1.0"                                       >> inference-engine.pc
cd -

export PKG_CONFIG_PATH
cd build
  echo "#!/bin/sh"                                                    >  run-fpm.sh
  echo "#-- DO NOT EDIT -- created by inference-engine/install.sh"    >> run-fpm.sh
  echo "export PKG_CONFIG_PATH"                                       >> run-fpm.sh
  echo "`brew --prefix fpm`/bin/fpm \$@ --verbose \\"                 >> run-fpm.sh
  echo "--profile debug \\"                                           >> run-fpm.sh
  echo "--c-compiler \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_CC`\" \\" >> run-fpm.sh
  echo "--compiler \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_FC`\" \\"   >> run-fpm.sh
  echo "--flag \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_FLAG`\""        >> run-fpm.sh
  chmod u+x run-fpm.sh
cd -

if command -v fpm > /dev/null 2>&1; then
  brew tap fortran-lang/fortran
  brew install fpm
fi

export PKG_CONFIG_PATH
./build/run-fpm.sh test

echo ""
echo "____________ Inference-Engine has been installed! ____________" 
echo ""
echo "To rebuild or to run tests or examples via the Fortran Package"
echo "Manager (fpm) with the required compiler/linker flags, pass a"
echo "fpm command to the build/run-fpm.sh script. For example, run"
echo ""
echo "./build/run-fpm.sh run"
