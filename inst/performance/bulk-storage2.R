library(raadtools)
library(aceecostats)

sf <- tail(sstfiles(), 365)

## three versions of the raster
## 1. is raadtools, installed tools against the OISST native files
## build two versions of a raster binary .grd, 
## 2. brick is BIL
#brick(stack(sf$fullname), filename = "/mnt/raadput/sstbrick.grd", datatype = "FLT4S")
## 3. bulk is BSQ
#build_bulk_file(sf, "/mnt/raadput/sstbulk.grd", aceecostats:::read_i_sst, layer_prefix = "sst")
## 4. try GeoTIFF
# writeRaster(brick("/mnt/raadput/sstbulk.grd"), "/mnt/raadput/sst_flat.tif", options = c("COMPRESS=NONE", "TILED=NO"))
## writeRaster(brick("/mnt/raadput/sstbulk.grd"), "/mnt/raadput/sst_ctile.tif", options = c("COMPRESS=LZW", "TILED=YES"))

## The clear winner is 3, by losing some flexibility (bake in the ice mask, rotate to -180, 180, unscale to 32-bit floats)
## we get huge performance benefit for the front end extraction. 

ex <- extent(100, 120, -50, -40)

## raadtools-style read idioms, just to benchmark
## read the BIL version
readsstbrick <- function(date, returnfiles = FALSE, ..., inputfiles = NULL) {
  if (returnfiles) return(data.frame(fullname = "/mnt/raadput/sstbrick.grd", band = seq(nrow(sf)), date = sf$date, stringsAsFactors = FALSE))
  b <- brick("/mnt/raadput/sstbrick.grd")
  b[[findInterval(as.POSIXct(date), sf$date)]]
}
## read the BSQ version
readsstbulk <- function(date, returnfiles = FALSE, ..., inputfiles = NULL) {
  if (returnfiles) return(data.frame(fullname = "/mnt/raadput/sstbulk.grd", band = seq(nrow(sf)), date = sf$date, stringsAsFactors = FALSE))
  b <- brick("/mnt/raadput/sstbulk.grd")
  b[[findInterval(as.POSIXct(date), sf$date)]]
}

## read the GeoTIFF version
readsstgeoflat <- function(date, returnfiles = FALSE, ..., inputfiles = NULL) {
  if (returnfiles) return(data.frame(fullname = "/mnt/raadput/sst_flat.tif", band = seq(nrow(sf)), date = sf$date, stringsAsFactors = FALSE))
  b <- brick("/mnt/raadput/sst_flat.tif")
  b[[findInterval(as.POSIXct(date), sf$date)]]
}

## read the GeoTIFF version
readsstgeoctile <- function(date, returnfiles = FALSE, ..., inputfiles = NULL) {
  if (returnfiles) return(data.frame(fullname = "/mnt/raadput/sst_ctile.tif", band = seq(nrow(sf)), date = sf$date, stringsAsFactors = FALSE))
  b <- brick("/mnt/raadput/sst_ctile.tif")
  b[[findInterval(as.POSIXct(date), sf$date)]]
}
## -------------------------------------------------------------------
## sample extract pts
set.seed(7)
pts <- cbind(runif(1e5, 100, 120), runif(1e5, -50, -40))

df <- data.frame(x = sample(pts[,1], 1000), y = sample(pts[, 2], 1000), time = as.POSIXct(seq(Sys.Date() - 360, Sys.Date() - 5, length = 1000)))


## -------------------------------------------------------------------
## A. read a slab
# 1. raadtools
#system.time(readsst(sf$date, xylim = ex, lon180 = FALSE, inputfiles = sf))
#user  system elapsed 
#10.456   3.164  15.350 0 

# 2. raster BIL
system.time(crop(brick("/mnt/raadput/sstbrick.grd"), ex))
# 3. raster BSQ
system.time(crop(brick("/mnt/raadput/sstbulk.grd"), ex))
# 4. GeoTIFF
system.time(crop(brick("/mnt/raadput/sst_flat.tif"), ex))

## -------------------------------------------------------------------
## B. extract values at points for all times in the brick
system.time(extract(brick("/mnt/raadput/sstbrick.grd"), pts))
system.time(extract(brick("/mnt/raadput/sstbulk.grd"), pts))
system.time(extract(brick("/mnt/raadput/sst_flat.tif"), pts))

## -------------------------------------------------------------------
## C. Extract values at locations-times
##system.time(extract(readsst, df)) 
##    user  system elapsed 
## 145.764   6.584 154.710
system.time(extract(readsstbrick, df))
system.time(extract(readsstbulk, df))
#system.time(extract(readsstgeoflat, df))


