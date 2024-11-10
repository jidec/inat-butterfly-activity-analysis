# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","readr","colorEvoHelpers","inatDailyActivity","dplyr") # packages that your targets need to run
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
  # load observations
  tar_target(obs_file, "data/usa_butterflies_inat_gbif.txt", format="file"),
  tar_target(obs_data, readr::read_csv(obs_file)), # may have to change this
  #tar_target(obs_data, bindiNatGBIFAnnotations(read_delim(obs_file, delim = "\t", escape_double = FALSE, trim_ws = TRUE),
  #                                             "lifeStage", 
  #                                             "D:/GitProjects/inat-daily-activity-analysis/data/raw/inat/usa_butterflies_inat_gbif_dwc.zip")),
  
  # add longitude and datetime cols, filter for 2016 onward and in contiguous USA only
  tar_target(obs_fixed, filterContiguousUSA(obs_data %>% mutate(latitude=decimalLatitude,longitude=decimalLongitude,datetime=eventDate)
                                            %>% filter(year >= 2016) 
                                            %>% filter(lifeStage != "Larva")
                                            %>% filter(species != "Danaus plexippus"))),
  
  # bind solar hour and season interval
  tar_target(obs_timed, bindSolarTime(bindSeasonIntervals(obs_fixed,season_intervals_per_year = 4))),
  
  # grid it up
  tar_target(obs_gridded, bindGridCells(obs_timed,cellsize_km=250)),
  
  # species-season-cells
  #tar_target(sscs, createSSCs(obs_data_gridded, approach="species_season_cell", min_per_ssc=30)),
  # bind climate data
  #tar_target(sscs_climate, bindPrismData(sscs)),
  # bind leptraits
  #tar_target(sscs_final, mergeLeptraitsToSSCs(sscs_climate)),
  
  # species-season-YEAR-cell8
  tar_target(sscs2_initial, createSSCs(obs_gridded, approach="species_season_year_cell", min_per_ssc=30,earliest_hr=6,latest_hr=21)), #earliest=8
  
  # bind climate data
  tar_target(sscs2_climate, bindPrismData(sscs2_initial,prism_dir="D:/GitProjects/inat-daily-activity-analysis/data/prism")),
  
  # bind leptraits
  tar_target(sscs2_traits, mergeLeptraitsToSSCs(sscs2_climate)),
  
  # add lat and lon columns 
  tar_target(sscs2_latlon, sscs2_traits %>% mutate(lat=cell_lat,lon=cell_lon)),
  
  # bind daylength
  tar_target(sscs2_daylength, bindDaylength(sscs2_traits %>% mutate(latitude=cell_lat,longitude=cell_lon))),
  
  # scale and finish
  tar_target(sscs2, sscs2_daylength %>% mutate(wingspan_sc=scale(wingspan),temp_sc=scale(value),daylength_sc=scale(daylength)))
  
  # DAYMET APPROACH
  # download Daymet data at the locations of the grid cells 
  #tar_target(daymet_downloaded,downloadDaymet(obs_data_gridded, download_path="D:/GitProjects/inat-daily-activity-analysis/data",return=T)),#,skip=F),
  # reshape and add season intervals to Daymet data
  #tar_target(daymet_data, prepDaymet(daymet_downloaded, season_intervals_per_year = 4)),
  # summarize Daymet to season cells 
  #tar_target(daymet_summarized, daymet_data %>% group_by(season,cell) %>% summarise(tmax_mean = mean(tmax),dl_mean=mean(daylength))),
  # merge on Daymet
  # tar_target(obs_data_daymet, merge(obs_data_gridded,daymet_data, by=c("cell","season"), all.x=TRUE)),
  # tar_target(sscs_climate, merge(sscs,daymet_summarized, by=c("cell","season"),all.x=TRUE)),
)
