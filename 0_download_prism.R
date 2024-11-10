library(prism)
prism_set_dl_dir("D:/GitProjects/inat-daily-activity-analysis/data/prism")
get_prism_monthlys(type = "tmean", year = 2014:2023, mon = 1:12, keepZip = FALSE)