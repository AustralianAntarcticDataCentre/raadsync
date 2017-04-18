nodc_ghrsstfiles <- 
  function () 
  {
    ftx <- raadtools:::.allfilelist()
    cfiles <- grep("ftp.nodc.noaa.gov", ftx, value = TRUE)
    cfiles1 <- grep("ghrsst", cfiles, value = TRUE)
    cfiles2 <- grep("L4", cfiles1, value = TRUE)
    cfiles3 <- grep("GLOB/JPL/MUR", cfiles2, value = TRUE)
    cfiles4 <- grep("GLOB-v0", cfiles3, value = TRUE)
    cfiles5 <- grep("nc$", cfiles4, value = TRUE)
    files <- data.frame(fullname = cfiles5, stringsAsFactors = FALSE)
    files$date <- as.POSIXct(strptime(basename(cfiles5), "%Y%m%d"), 
                             tz = "GMT")
    files <- files[order(files$date), ]
    files <- files[!rev(duplicated(files[rev(seq(nrow(files))), 
                                         ]$date)), ]
    files
  }


nodc <- nodc_ghrsstfiles()

library(raster)
files <- c("/rdsi/PRIVATE/raad/data/ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/070/20150311-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc", 
           "/rdsi/PRIVATE/raad/data/ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/071/20150312090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc"
)

raster(files[1], stopIfNotEqualSpaced = FALSE)
# class       : RasterLayer 
# dimensions  : 16384, 32768, 536870912  (nrow, ncol, ncell)
# resolution  : 0.01098636, 0.01098633  (x, y)
# extent      : -180.0005, 180.0005, -89.99999, 89.99999  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
# data source : /rdsi/PRIVATE/raad/data/ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/070/20150311-JPL-L4UHfnd-GLOB-v01-fv04-MUR.nc 
# names       : analysed.sea.surface.temperature 
# z-value     : 1078909200 
# zvar        : analysed_sst 
# 
# Warning message:
#   In .varName(nc, varname, warn = warn) : varname used is: analysed_sst
# If that is not correct, you can set it to one of: analysed_sst, analysis_error, mask, sea_ice_fraction
raster(files[2], stopIfNotEqualSpaced = FALSE)
# class       : RasterLayer 
# dimensions  : 17999, 36000, 647964000  (nrow, ncol, ncell)
# resolution  : 0.01, 0.01  (x, y)
# extent      : -179.995, 180.005, -89.995, 89.995  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
# data source : /rdsi/PRIVATE/raad/data/ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/071/20150312090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc 
# names       : analysed.sea.surface.temperature 
# z-value     : 1078995600 
# zvar        : analysed_sst 


u <- c("ftp://podaac-ftp.jpl.nasa.gov/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/2015/070/20150311090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc", 
       "ftp://podaac-ftp.jpl.nasa.gov/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/2015/071/20150312090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc")
td <-  "/mnt/temp/RtmpRTsQwc"
for (i in u) download.file(i, file.path(td, basename(i)), mode = "wb")

f <- file.path(td, basename(u))
raster(f[1], stopIfNotEqualSpaced = FALSE)

# class       : RasterLayer
# dimensions  : 17999, 36000, 647964000  (nrow, ncol, ncell)
# resolution  : 0.01, 0.01  (x, y)
# extent      : -179.995, 180.005, -89.995, 89.995  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
# data source : /tmp/RtmpLMXRxX/20150311090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc
# names       : analysed.sea.surface.temperature
# z-value     : 1078909200
# zvar        : analysed_sst
# 
# Warning message:
#   In .varName(nc, varname, warn = warn) : varname used is: analysed_sst
# If that is not correct, you can set it to one of: analysed_sst, analysis_error, mask, sea_ice_fraction, dt_1km_data
raster(f[2], stopIfNotEqualSpaced = FALSE)
# class       : RasterLayer
# dimensions  : 17999, 36000, 647964000  (nrow, ncol, ncell)
# resolution  : 0.01, 0.01  (x, y)
# extent      : -179.995, 180.005, -89.995, 89.995  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
# data source : /tmp/RtmpLMXRxX/20150312090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc
# names       : analysed.sea.surface.temperature
# z-value     : 1078995600
# zvar        : analysed_sst
