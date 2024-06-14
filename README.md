# Forestry clearing spillover effects from state forest revocations

Author: Frankie Cho
Date: 14 June 2024

Code to replicate the analysis for the spillover effects of the revocation of state forests to national parks in New South Wales, Australia.

As a high-level overview, the code does the following:
1. Prepare historical Statewide Landcover and Trees Study (SLATS) data into a  rasterised format common across years
2. Extract SLATS clearing data for (1) each lot, and (2) across timber supply zones
3. Extract covariate values for each individual lot in the study area
4. Match properties in "treated" lots in each Timber Supply Zones to other "control" lots in other control Timber Supply Zones
5. Plot the figures in the report provided to the state government

## 1. SLATS data preparation
To use the SLATS data preparation script, one must have the source data in SLATS stored in `data/slats`. The naming conventions of the folders are:

* `slats_2015_17`: 2015-2017 SLATS data folder
* `slats_2017_20`: 2017-2020 SLATS data folder
* `slats1998_2010`: 1998-2010 SLATS data folder
* `slats2008_2014`: 2008-2015 SLATS data folder

The user should also have folders `intermediate_data` to collect intermediate raster layers and `output/slats` to collect the outputs of the data processing script. `job_logs` folder is also useful for collecting console printouts from the HPC and not have it clog up the working directory.

The parallelised processing of SLATS data is achieved with the use of high-performance compute clusters at QUT, with the main script of interest being `code/hpc/slats_batch_process.pbs`. If all is set up correctly, one can call Lyra at QUT with the following command:

`qsub code/hpc/slats_batch_process.pbs`

This HPC script calls the R script `code/preprocessing/slats_batch.R` iteratively for a range of index values (from 1-24). Each index value is a year of SLATS data that needs to be processed. For each iteration, the script `code/preprocessing/slats_batch.R` does the following:

1. Call `load_slats.R` to generate an object documenting the specific features of each year of SLATS data and its directories
2. Call a function `slats_batch` that takes the object as the input and reads the SLATS data in its folder, reprojecting/ rasterising/ re-coding the raster according to its specific data formatting of that year of SLATS data, and writing the raster to `output/slats`

The outputs are in TIF format and are in the naming convention `slats_{start_year}_{end_year}.tif`, with the start and end year of the period of analysis in SLATS. Note that for the years before 2006, the SLATS data are only available in a biannual basis.

The raster data are all re-categorised to match the code conventions of the latest years of SLATS data as follows:

1 - Natural

2 - Agriculture

3 - Infrastructure

4 - Forestry

## SLATS data extraction to timber supply zones and lots

This part requires that the user have completed the processing steps for SLATS above and have the annual/ biannual SLATS data in `output/slats/slats_{start_year}_{end_year}.tif`. The R function `extract_slats()` in `code/preprocessing/extract_functions.R` is a generally-applicable function for flexibly extracting SLATS land clearing data across all years and all classes of land-use change for any simple feature object in R (sf_obj). Because this is a computationally intensive process, the step of extracting SLATS data across all years in all lots is again made more computationally efficient through splitting this into several chunks and parallelising its extraction. Because parallelisation involves creating several copies of the source data (the SF object and the raster files), it unfortunately is only possible through the use of HPC to overcome memory limitations on the desktop.

### Timber Supply Zones

The code for extracting the SLATS clearing in Timber Supply Zones is in `code/analysis/slats_tsz_parallel.R`, which takes as an argument an index (of the timber supply zone). This script is parallelised across the 13 timber supply zones with `code/hpc/slats_tsz_parallel.pbs`. 

To obtain estimates of forestry clearing in private native forestry within timber supply zones, I masked out the timber supply zones state forest (including revoked and current) from the timber supply zone polygons. This is created with `code/analysis/slats_tsz.R`, with the `TimberSZ_LL.shp` file provided by Allen McIlwee in NSW DPE (of the Timber Supply Zones).

It extracts the area of SLATS clearing by total, and by the polygons after excluding state forests (i.e. private native forestry) separately. The difference between the two numbers are therefore the clearing in state forests and national parks.

### Lots

The extraction of clearing statistics across time at the lot level is done in `slats_prop.R`, that is parallelised (across KMRs, 1-9) with `code/hpc/slats_prop.pbs`. It simply reads the lots from the GDB in the risk analysis folder, assigns a unique "lot_id", and extracts the SLATS data into a data.frame that can be related back to the KMR lots with the lot_id.

## Covariate extraction at the lot level

For the code that was used to generate the covariate layers, check the repository "risk-model-covariates".

After the covariate layers are generated, the covariate values for each lot is extracted with the PBS script `code/hpc/cov_extract_prop.pbs` in parallel, which calls the R script `code/preprocessing/cov_prop.R` at 1-9000 to parallelise across KMR and across lots in the KMR. The strategy is to break up the lots in each KMR into a number of chunks (e.g. 1000) and subset lots into each chunk, feeding each chunk (a subset of the lots in the KMR) into the algorithm. This reduces the memory usage per instance. Outputs are stored in `output/cov_prop_parallel` and indexed by `cov_prop_{kmr_abbr}_{chunk_id}_{cov_type}.csv`, with kmr_abbr being the abbreviation of the KMR (Koala Management Region), chunk id being the index of the chunk (1-1000, if chunk size is 1000) and cov_type, being either "discrete", "continuous" or "both" (both discrete and continuous covariates).

## State forest combined dataset

The state forest dataset is generated by combining the dataset of current state forests downloaded from FCNSW and a revoked state forests dataset provided by Todd Walmsley at the FCNSW. 

## Locational attributes of the lots

The script `code/preprocessing/lot_sf_association.R` does a spatial join between the lots and the state forest dataset.

The file `revoked_sf_np.shp` is generated with the script in `code/preprocessing/buffer_revoked_sf.R`, which simply takes the file of the full list of revoked state forests provided by the FCNSW and subsets only those which are currently national park, state conservation areas, flora reserve, regional park or a nature reserve. This thus excludes those others that are revoked and now used for roads or infrastructure for instance.

Afterwards, the `revoked_sf_np.shp` dataset is combined with the dataset of current state forests estate obtained from the FCNSW to get a dataset of all state forests, with columns of when it was being revoked.

## Matching

After the SLATS and covariates of the lots are prepared, the matching analysis is done in `code/analysis/matching.R`. The analysis is split into two sections. First, it matches lots within revoked state forests and current state forests to estimate the effect of the revocation had on clearing rates and trends. Second, it estimates the effect of the spillovers across each timber supply zone.

## Report figures

The report figures are generated using the code in `code/anaysis/report_figures.R`.