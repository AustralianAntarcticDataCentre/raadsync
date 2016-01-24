ghrsst=function(dataset) {
    ## ghrsst synchronisation handler

    ## The data source is ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/
    ## with yearly subdirectories ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/
    ## within yearly directories, days are subdirectories ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/051/ with actual data files therein
    ## BUT the daily subdirectories are symlinks and recursive wget won't recurse symlinked directories (known limitation of wget)

    ## hence use a custom handler, at least for now

    ## we can use wget within a daily directory, e.g.:
    ## wget --recursive --level=inf --no-parent --timestamping ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/051/

    ## we expect that the provided source URL is either pointing to the root of the collection:
    ## ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/
    ## or to a particular year (multiple years will need multiple source_urls)
    ## ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/2015/
    ##
    ## for the former, we will loop from 2002 to present here
    ## for the latter, just hit that specific yearly directory

    if (grepl("JPL/MUR/?$",dataset$source_url)) {
        ## pointing to root
        yearlist=seq(from=2002,to=as.numeric(format(Sys.Date(),"%Y")),by=1)
    } else {
        yearlist=as.numeric(basename(dataset$source_url))
    }
    ## make sure method_flags include --recursive --no-parent
    if (!grepl("--recursive",dataset$method_flags,ignore.case=TRUE)) {
        dataset$method_flags=paste(dataset$method_flags,"--recursive",sep=" ")
    }
    if (!grepl("--no-parent",dataset$method_flags,ignore.case=TRUE)) {
        dataset$method_flags=paste(dataset$method_flags,"--no-parent",sep=" ")
    }
    for (thisyear in yearlist) {
        for (thisday in 1:366) {
            dummy=dataset
            dummy$source_url=paste0("ftp://ftp.nodc.noaa.gov/pub/data.nodc/ghrsst/L4/GLOB/JPL/MUR/",thisyear,"/",sprintf("%03d",thisday),"/")
            wget_call=build_wget_call(dummy)
            do_wget(wget_call,dataset)
        }
    }
}



ghrsst2 <- 
function (dataset)
{
    if (grepl("JPL/MUR/?$", dataset$source_url)) {
        yearlist = seq(from = 2002, to = as.numeric(format(Sys.Date(),
            "%Y")), by = 1)
    }
    else {
        yearlist = as.numeric(basename(dataset$source_url))
    }
    if (!grepl("--recursive", dataset$method_flags, ignore.case = TRUE)) {
        dataset$method_flags = paste(dataset$method_flags, "--recursive",
            sep = " ")
    }
    if (!grepl("--no-parent", dataset$method_flags, ignore.case = TRUE)) {
        dataset$method_flags = paste(dataset$method_flags, "--no-parent",
            sep = " ")
    }
    for (thisyear in yearlist) {
        for (thisday in 1:366) {
            dummy = dataset
            dummy$source_url = paste0("http://podaac-w10n.jpl.nasa.gov/w10n/allData/ghrsst/data/L4/GLOB/UKMO/OSTIA/2016",
              sprintf("%03d", thisday), "/")
            wget_call = build_wget_call(dummy)
            do_wget(wget_call, dataset)
        }
    }
}
