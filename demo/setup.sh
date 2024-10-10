#!/bin/bash

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

FPM_FC=${FPM_FC:-"flang-new"}
FPM_CC=${FPM_CC:-"clang"}

if [ $(uname) = "Darwin" ]; then
  if command -v brew ; then
    brew install netcdf netcdf-fortran pkg-config coreutils # coreutils supports `realpath` below
    NETCDF_LIB_PATH="`brew --prefix netcdf`/lib"
    HDF5_LIB_PATH="`brew --prefix hdf5`/lib"
    NETCDFF_LIB_PATH="`brew --prefix netcdf-fortran`/lib"
    fpm_cc_version=$($FPM_CC --version)
    if [[ $fpm_cc_version = Apple* ]]; then
      echo "$FPM_CC appears to be an Apple compiler.  Please set FPM_CC to the location of LLVM clang."
      exit 1
    fi
  else
    cat <<'EOF'

      Command 'brew' not found. On macOS, this script uses Homebrew (https://brew.sh) to 
      install the prerequisite packages netcdf, netcdf-fortran, pkg-config, and coreutils.
      Please install Homebrew and restart this script."
EOF
  fi
elif [ $(uname) = "Linux" ]; then
  if [[ -z ${HDF5_LIB_PATH:-}    ]]; then 
    printf "Please set HDF5_LIB_PATH to the HDF5 library path and restart this script.\n\n"
    exit 1
  fi
  if [[ -z ${NETCDF_LIB_PATH:-}  ]]; then
    printf "Please set NETCDF_LIB_PATH to the NetCDF library path and restart this script.\n\n"
     exit 1
  fi
  if [[ -z ${NETCDFF_LIB_PATH:-} ]]; then
    printf "Please set NETCDFF_LIB_PATH to the NetCDF-Fortran library path and restart this script.\n\n"
     exit 1
  fi
fi

FPM_LD_FLAG=" -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH -L$NETCDFF_LIB_PATH"

PREFIX=`realpath $PREFIX`

fpm_fc_version=$($FPM_FC --version)
if [[ $fpm_fc_version = flang* ]]; then
  FPM_FLAG="-mmlir -allow-assumed-rank -O3 -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
elif [[ $fpm_fc_version = GNU* ]]; then
  echo
  echo "$FPM_FC appears to be gfortran, which is currently unsupported due to compiler bugs for parameterized derived types."
  echo
  exit 1
  FPM_FLAG="-fcoarray=single -O3 -fallow-argument-mismatch -ffree-line-length-none -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
  FPM_RUNNER="cafrun -n 1"
  FPM_CC="mpicc"
else
  FPM_FLAG=""
fi

mkdir -p build

CI=${CI:-"false"} # GitHub Actions workflows set CI=true

if [ $CI = true ]; then
  PKG_CONFIG_PATH=`realpath ./build/pkgconfig`
  echo "---------------"
  echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"
  echo "---------------"
else
  PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig"
  if [ ! -d "$PKG_CONFIG_PATH" ]; then
    mkdir -p "$PKG_CONFIG_PATH"
  fi
  PKG_CONFIG_PATH=`realpath "$PKG_CONFIG_PATH"`
fi

INFERENCE_ENGINE_VERSION=$(grep version ../fpm.toml | grep -o '".*"' - | sed 's/"//g')

INFERENCE_ENGINE_PC="$PKG_CONFIG_PATH/inference-engine.pc"
echo "INFERENCE_ENGINE_FPM_CC=\"$FPM_CC\""                  >  $INFERENCE_ENGINE_PC
echo "INFERENCE_ENGINE_FPM_FC=\"$FPM_FC\""                  >> $INFERENCE_ENGINE_PC
if [[ ! -z ${FPM_RUNNER:-} ]];  then
  echo "INFERENCE_ENGINE_FPM_RUNNER=\"$FPM_RUNNER\""          >> $INFERENCE_ENGINE_PC
fi
echo "INFERENCE_ENGINE_FPM_LD_FLAG=\"$FPM_LD_FLAG\""        >> $INFERENCE_ENGINE_PC
echo "INFERENCE_ENGINE_FPM_FLAG=\"$FPM_FLAG\""              >> $INFERENCE_ENGINE_PC
echo "Name: inference-engine"                               >> $INFERENCE_ENGINE_PC
echo "Description: Inference Engine"                        >> $INFERENCE_ENGINE_PC
echo "URL: https://github.com/berkeleylab/inference-engine" >> $INFERENCE_ENGINE_PC
echo "Version: $INFERENCE_ENGINE_VERSION"                   >> $INFERENCE_ENGINE_PC

if [ $CI = true ]; then
  echo "---------------"
  echo "cat \$INFERENCE_ENGINE_PC"
  cat $INFERENCE_ENGINE_PC
  echo "---------------"
fi

export PKG_CONFIG_PATH
cp src/run-fpm.sh-header build/run-fpm.sh
RUN_FPM_SH="`realpath ./build/run-fpm.sh`"
echo "`which fpm` \$fpm_arguments \\" >>  $RUN_FPM_SH
echo "--profile release \\" >> $RUN_FPM_SH
echo "--c-compiler \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_CC`\" \\"     >> $RUN_FPM_SH
echo "--compiler \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_FC`\" \\"       >> $RUN_FPM_SH
if [[ ! -z ${FPM_RUNNER:-} ]];  then
  echo "--runner \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_RUNNER`\" \\"     >> $RUN_FPM_SH
fi
echo "--flag \"-cpp `pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_FLAG`\" \\"    >> $RUN_FPM_SH
echo "--link-flag \"`pkg-config inference-engine --variable=INFERENCE_ENGINE_FPM_LD_FLAG`\" \\" >> $RUN_FPM_SH
echo "\$program_arguments" >> $RUN_FPM_SH
chmod u+x $RUN_FPM_SH
if [ $CI = true ]; then
  echo "---------------"
  echo "cat $RUN_FPM_SH"
  cat $RUN_FPM_SH
  echo "---------------"
fi

$RUN_FPM_SH build

echo ""
echo "____________________ The inference-engine demo apps build succeeded! _______________________"
echo ""
echo "Run the following command to see a list of available apps:"
echo ""
echo "./build/run-fpm.sh run"
echo ""
echo "Append a space followe by an app's name to see basic app usage information."
