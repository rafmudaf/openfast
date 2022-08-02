REM SPDX-FileCopyrightText: 2022 Intel Corporation
REM
REM SPDX-License-Identifier: MIT

set VS_VER=%1

@call "C:\Program Files (x86)\Intel\oneAPI\setvars-vcvarsall.bat" %VS_VER%

for /f "tokens=* usebackq" %%f in (`dir /b "C:\Program Files (x86)\Intel\oneAPI\compiler\" ^| findstr /V latest ^| sort`) do @set "LATEST_VERSION=%%f"
@call "C:\Program Files (x86)\Intel\oneAPI\compiler\%LATEST_VERSION%\env\vars.bat"

cd ${{runner.workspace}}\openfast\build
cmake .. ^
  -G"NMake Makefiles" ^
  -DCMAKE_INSTALL_PREFIX:PATH=${{runner.workspace}}\openfast\install ^
  -DCMAKE_BUILD_TYPE:STRING=Release
@REM -DCMAKE_Fortran_COMPILER:STRING=${{env.FORTRAN_COMPILER}} \
@REM  -DCMAKE_CXX_COMPILER:STRING=${{env.CXX_COMPILER}} \
@REM  -DCMAKE_C_COMPILER:STRING=${{env.C_COMPILER}} \

cmake --build . --target regression_test_module_drivers -- -j ${{env.NUM_PROCS}}
