#!/usr/bin/env bash
# Copyright (c) 2018-2019, NVIDIA CORPORATION.  All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Test Fortran character assignment/expression lowering by driving compilation
# and executions of Fortran and C++ source codes.
#
# Usage: cmd bbc llc
# To keep generated files, run `export KEEP=TRUE` before the test call.

BBC=$1
LLC=$2
CPP=g++
CPP_SRC=$PWD/test-character-assignment.cc
F_SRC=$PWD/character-assignment.f90

function die {
  echo "$(basename $0): $*" >&2
  exit 1
}

temp=`mktemp -d ./tmp.XXXXXX`
cd $temp
[[ $KEEP ]] || trap "cd .. && rm -rf $temp" EXIT

bbcLog=bbc.log
assembly=a.s
testObject=test.o
testExec=./test_exec
testLog=test.log
llFile=a.mlir.ll
$BBC -emit-llvm -disable-fir2std $F_SRC 2>$bbcLog
[[ $? -ne 0 ]] && die "bbc test.f90 compilation failure"
sed -i 's/_QP//g' -i $llFile
$LLC $llFile -o $assembly
[[ $? -ne 0 ]] && die "llc failed compiling bbc output"
as $assembly -o $testObject
[[ $? -ne 0 ]] && die "as failed compiling llc output"
$CPP -std=c++17 $testObject $CPP_SRC -o $testExec
[[ $? -ne 0 ]] && die "driver.f90 compilation/linking failure"
$testExec > $testLog
result=$?
cat $testLog
if [ $result -ne 0 ]; then
  echo "FAIL"
else
  echo "PASS"
fi
