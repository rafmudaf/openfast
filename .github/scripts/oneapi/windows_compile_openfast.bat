REM SPDX-FileCopyrightText: 2022 Intel Corporation
REM
REM SPDX-License-Identifier: MIT

set VS_VER=%1

@call "C:\Program Files (x86)\Intel\oneAPI\setvars-vcvarsall.bat" %VS_VER%

for /f "tokens=* usebackq" %%f in (`dir /b "C:\Program Files (x86)\Intel\oneAPI\compiler\" ^| findstr /V latest ^| sort`) do @set "LATEST_VERSION=%%f"
@call "C:\Program Files (x86)\Intel\oneAPI\compiler\%LATEST_VERSION%\env\vars.bat"

@REM dir "C:\Program Files (x86)\Intel\oneAPI\"
@REM dir "C:\Program Files (x86)\Intel\oneAPI\mkl\"
@REM dir "C:\Program Files (x86)\Intel\oneAPI\mkl\%LATEST_VERSION%\"
@REM dir "C:\Program Files (x86)\Intel\oneAPI\mkl\%LATEST_VERSION%\env\"
@REM dir "C:\Program Files (x86)\Intel\oneAPI\mkl\%LATEST_VERSION%\env\vars.bat"

@call "C:\Program Files (x86)\Intel\oneAPI\mkl\%LATEST_VERSION%\env\vars.bat"

@REM cd ${{runner.workspace}}\openfast\build
cmake ^
  -S "D:\a\openfast\openfast" ^
  -B "D:\a\openfast\openfast\build" ^
  -G "NMake Makefiles" ^
  -DCMAKE_BUILD_TYPE:STRING=Release
@REM   -DCMAKE_INSTALL_PREFIX:PATH=${{runner.workspace}}\openfast\install ^
@REM -DCMAKE_Fortran_COMPILER:STRING=${{env.FORTRAN_COMPILER}} \
@REM  -DCMAKE_CXX_COMPILER:STRING=${{env.CXX_COMPILER}} \
@REM  -DCMAKE_C_COMPILER:STRING=${{env.C_COMPILER}} \

cmake --build "D:\a\openfast\openfast\build" --target openfast
