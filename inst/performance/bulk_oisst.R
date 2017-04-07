library(raadtools)
library(aceecostats)
sstf <- sstfiles()
build_bulk_file(sstf, 
   file.path(getOption("default.datadir"), "data/webdav.data.aad.gov.au/data/environmental/derived/global/OI-daily-v2.grd"), 
             aceecostats:::read_i_sst, layer_prefix = "sst")
   
   