call "..\..\envs\windows.cmd"

:: parameters
set OMP_NUM_THREADS=2
set INPUTFILE=sim1.geo

:: logfile
set LOGFILE=%INPUTFILE:.geo=.stdout.txt%
IF EXIST %LOGFILE% DEL %LOGFILE%

:: create banner
echo Running model %CD%\%INPUTFILE% > %LOGFILE%
echo Start time: %DATE% %TIME% >> %LOGFILE%
echo User: %USERNAME%@%COMPUTERNAME% >> %LOGFILE%
echo -------------------------------------------------------------------------------- >> %LOGFILE%

:: run model
..\..\src\build\solver.exe sim1.geo 2>&1 | tee -a %LOGFILE%

echo -------------------------------------------------------------------------------- >> %LOGFILE%
echo End time: %DATE% %TIME% >> %LOGFILE%

PAUSE