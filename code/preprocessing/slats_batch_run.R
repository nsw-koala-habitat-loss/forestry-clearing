rm(list=ls())

library(rstudioapi)
library(foreach)
library(doParallel)
library(pbapply)
library(future)
library(future.apply)

source("code/preprocessing/load_slats.R")
# source("code/preprocessing/slats_batch.R")

#plan(multicore)
# slats_batch(slats_path[[length(slats_path)]])
for (i in 1:length(slats_path)) {
  obj <- slats_path[[i]]
  rstudioapi::jobRunScript(
    "code/preprocessing/slats_batch.R",
    name = tools::file_path_sans_ext(obj$out_name),
    workingDir = "H://nsw-habitat-loss",
    importEnv = TRUE
  )
  if (i %% 6 == 0) {
    Sys.sleep(1000)
  }
}
# 
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# 
# foreach (i=1:length(slats_path), .packages = c('terra', 'dplyr', 'tools')) %dopar% {
#   path_obj <- slats_path[[i]]
#   slats_batch(path_obj)
# }
# 
# stopCluster(cl)