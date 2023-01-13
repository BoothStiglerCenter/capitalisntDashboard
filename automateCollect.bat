@echo off
@REM Setting today's date etc for logging
for /f "tokens=1-4 delims=/" %%i in ("%date%") do (
    set dow=%%i
    set month=%%j
    set day=%%k
    set year=%%l
)
set logfilename=%year%-%month%-%day%-apiCollect-log.txt

> logs/%logfilename% (
    echo Starting Automated API Collection

    echo ---------------------
    echo ------ CONDA --------
    echo ---------------------
    call activate ioCapture



    echo ---------------------
    echo ------ PYTHON -------
    echo ---------------------
    @REM python HelloWorld.py
    python apiCollect.py


    echo ---------------------
    echo ------ ENDING -------
    echo ---------------------
)
