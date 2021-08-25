#-to run on cluster---------------------------------------------------------------------------------------
# drat:::add("mrc-ide")
# install.packages("didehpc")
library(didehpc)
root <- "context" 

#-describe computer--------------------------------
options(didehpc.cluster = "fi--dideclusthn",
        didehpc.username = "ht1212")

#-check config--------------------------------------
didehpc::didehpc_config()
didehpc::web_login()

#-load model running function-----------------------
sources <- c("2_model_function.R")
src <- conan::conan_sources("local::mlgts_1.0.20.tar.gz")

#-create a context----------------------------------
ctx <- context::context_save(root,
                             sources = sources,
                             packages = c("tibble", "dplyr","here", "stringr", "mlgts"),
                             package_sources =src)

#-config the cluster---------------------------------
config <- didehpc::didehpc_config(cluster="fi--dideclusthn", cores = 1, use_rrq = FALSE)

#-create a queue - interface to the cluster queue----
obj <- didehpc::queue_didehpc(ctx, config=config)

#-submit actual workers-----------------------------
#-the argument is the number of workers to submit---
#workers <- obj$submit_workers(10)
#workers

# #-to send tasks to these workers we need a *second* type of queue--------------
#rrq <- obj$rrq_controller()
#rrq

#-submitting jobs works as before, but should hopefully be a little faster----
ploop <- obj$enqueue_bulk(median_df %>% filter(run %in% c("srtss_5_dose_original","srtss_5_dose_update")), run_sites, progress = TRUE)


