# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","readr","colorEvoHelpers","inatDailyActivity") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(observations_file, "D:/GitProjects/inat-daily-activity-analysis/data/raw/inat/usa_butterflies_inat_gbif.csv", format="file"),
  tar_target(baseline_obs_file, "D:/GitProjects/inat-daily-activity-analysis/data/raw/inat/usa_insects_inat_gbif.csv", format="file"),
  
  tar_target(observations_df, {read_delim(observations_file, delim = "\t", escape_double = FALSE, trim_ws = TRUE)}),
  tar_target(baseline_obs_df, {read_delim(baseline_obs_file, delim = "\t", escape_double = FALSE, trim_ws = TRUE)}),
  
  #tar_target(daymet_downloaded,downloadDaymet(cellsize_km=250, download_path="D:/GitProjects/inat-daily-activity-analysis/data",return=T)),
  tar_skip(daymet_downloaded,downloadDaymet(cellsize_km=250, download_path="D:/GitProjects/inat-daily-activity-analysis/data",return=T),skip=T),
  
  tar_target(daymet_data, prepDaymetData(daymet_downloaded, season_intervals_per_year = 4)),
  
  tar_target(sscs, createSSCsFromRawData(taxon_obs=observations_df, 
                                         baseline_obs=baseline_obs_df,
                                         daymet_downloaded = daymet_downloaded,
                                         season_intervals_per_year=4,rm_bfs_from_bl=TRUE,
                                         ssc_cellsize_km=250, ssc_n_obs_threshold=30,
                                         diff_metric_nbins=8,start_hr_trim=1,end_hr_trim = 1,
                                         merge_leptraits=TRUE, add_weib_metrics = FALSE))
)
