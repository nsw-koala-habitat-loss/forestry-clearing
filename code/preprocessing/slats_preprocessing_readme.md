## Pre-processing steps for SLATS data

1. Use `load_slats.R` to generate the csv file for running the SLATS pre-processing scripts in BAT file
2. Execute `slats_raster_run.bat` to convert the SLATS files to a common grid and resample at a 10-meter resolution in ArcGIS Python
3. Run `slats_reclassify.R` to reclassify the code of the causes of deforestation to a common numeric format