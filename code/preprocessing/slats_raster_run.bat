@echo off
setlocal enabledelayedexpansion

set arcgispro_python="C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3\python.exe"

REM Run 1998-2010 data

set "home_dir=H:\nsw-habitat-loss"

set "input_dir=!home_dir!"
set "out_dir=!home_dir!\processed_data\slats"
set "data_description_csv=!home_dir!\data\slats\slats_paths.txt"

REM del /S /Q "!out_dir!"

cd !input_dir!
for /F "usebackq tokens=* delims=" %%A in ("%data_description_csv%") do (
    for /F "tokens=3 delims=," %%a in ("%%A") do (
        set "filename=%%a"
    )
    for /F "tokens=6 delims=," %%a in ("%%A") do (
        set "outname=%%a"
    )
    for /F "tokens=4 delims=," %%a in ("%%A") do (
        set "field=%%a"
    )
    for /F "tokens=5 delims=," %%a in ("%%A") do (
        set "change_code=%%a"
    )

    set "in_file=!input_dir!\!filename!"
    set "out_file=!out_dir!\!outname!"
    start "" /b %arcgispro_python% "!home_dir!\code\preprocessing\slats_raster.py" "!in_file!" "!out_file!" "!field!" "!change_code!"
    if %errorlevel% neq 0 (
        echo Error in slats_raster.py. Exiting...
        exit /b %errorlevel%
    )
)



