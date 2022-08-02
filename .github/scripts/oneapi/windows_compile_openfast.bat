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

mkdir "D:\a\openfast\openfast\build"
cd "D:\a\openfast\openfast\build"
cmake ^
  -S "D:\a\openfast\openfast" ^
  -B "D:\a\openfast\openfast\build" ^
  -G "NMake Makefiles" ^
  -DCMAKE_BUILD_TYPE:STRING=Release

cmake --build "D:\a\openfast\openfast\build" --target beamdyn_driver
@REM cmake --build "D:\a\openfast\openfast\build" --target openfast
@REM cmake --build "D:\a\openfast\openfast\build" --target openfast
@REM cmake --build "D:\a\openfast\openfast\build" --target turbsim
