@echo off
if "%~1" == "" (
    echo Usage: %~n0 ^<full.test.ClassName^>
    exit /b 1
)

set "test=%~1"

javac ^
    -d "__out" ^
    "--class-path=%~dp0/lib/*;%~dp0../java;%~dp0../javascript;%~dp0../prolog" ^
    "%~dp0%test:.=/%.java" ^
 && java ^
    -ea ^
    "--class-path=%~dp0/lib/*;__out" ^
    "%test%" "%~2"
