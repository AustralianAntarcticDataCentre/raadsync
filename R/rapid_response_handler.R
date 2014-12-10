rapid_response=function(dataset) {
    ## NASA MODIS rapid response synchronisation handler

    ## URLs of the form:  http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.2014342.terra.4km.tif
    ## where
    ##  resolution can be "4km" "2km" "1km". (Individual image tiles can be at resolution down to 250m, e.g. http://lance-modis.eosdis.nasa.gov/imagery/subsets/?subset=Antarctica_r06c01.2014342.terra.250m.tif)
    ##  platform can be "terra" or "aqua"
    ## can also ask for other bands: http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.2014342.terra.721.4km http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.2014342.terra.367.4km
    ## note that band 6 on aqua is not functional (http://lance-modis.eosdis.nasa.gov/imagery/rapid/About_Antarctica_mosaic.html)

    ## we only provide sync for the 4km truecolor mosaic images so far

    ## simple loop from current day back to 2013 day 345 (Dec 11)

    this_resolution="4km"
    d=Sys.Date()
    while (d>=format.Date("2013-12-11")) {
        rapid_response_do_download(d,"terra",this_resolution,dataset)
        rapid_response_do_download(d,"aqua",this_resolution,dataset)
        d=d-1
    }
}

rapid_response_do_download=function(d,platform,resolution,dataset) {
            this_url=sprintf("http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.%s%s.%s.%s.tif",format(d,"%Y"),format(d,"%j"),platform,resolution)
        if (dataset$skip_downloads) {
            cat(sprintf("not downloading %s, skip_downloads is TRUE\n",this_url))
        } else {
            dummy=dataset
            dummy$method_flags=paste("--progress=dot:giga","--recursive",sep=" ")
            ## this gives output filenames like "index.html@mosaic=Antarctica.2014003.terra.4km.tif" - would prefer these to be just "Antarctica.*"
            ##dummy$method_flags=paste("--progress=dot:giga","--directory-prefix=lance-modis.eosdis.nasa.gov/imagery/subsets/",paste0("--output-document=",sub("^http.*mosaic=","",this_url)),sep=" ")
            ## can't use --output-document option with timestamping, so it looks like we have to put up with odd filenames
            dummy$source_url=this_url
            wget_call=build_wget_call(dummy)
            do_wget(wget_call,dataset)
        }
}

