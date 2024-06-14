library(rstudioapi)

rm(list=ls())
gc()

# Process SLATS
for (job_id in 1:9) {
  jobRunScript("code/preprocessing/slats_prop.R",
              name = job_id,
              workingDir = "H:/nsw-habitat-loss",
              importEnv = TRUE)
}

# Process Covariates
for (job_id in 1:9) {
  jobRunScript("code/preprocessing/cov_prop.R",
               name = paste0('Cov_kmr_', job_id),
               workingDir = "H:/nsw-habitat-loss",
               importEnv = TRUE)
}

# Process woody
rm(list=ls())
for (job_id in 1:9) {
  jobRunScript("code/preprocessing/woody_prop.R",
               name = paste0('Woody_kmr_', job_id),
               workingDir = "H:/nsw-habitat-loss",
               importEnv = TRUE)
  Sys.sleep(1)
}


# Process buffer of revoked SF
source("code/preprocessing/buffer_revoked_sf.R")
for (kmr_id in 1:9) {
  jobRunScript("code/preprocessing/lots_buffer_revoked_sf.R",
               name = paste0('Lots_sf_buffer_kmr_', kmr_id),
               workingDir = "H:/nsw-habitat-loss",
               importEnv = TRUE)
}
