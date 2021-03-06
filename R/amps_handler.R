amps <- function(dataset) {
    ## shouldn't need any specific method_flags for this
    ## could potentially set e.g. --progress=dot:giga
    ## --timestamping not needed (handled through clobber config setting)
    ## --recursive, etc not needed
    if (sub("/$","",dataset$source_url)!="http://www2.mmm.ucar.edu/rt/amps/wrf_grib") {
        cat(sprintf("Dataset source_url (%s) not as expected, not processing.\n",dataset$source_url))
        return()
    }
    x <- html_session(dataset$source_url)
    n <- html_attr(html_nodes(x,"a"),"href")
    idx <- which(sapply(n,function(z)grepl("[[:digit:]]+",z,ignore.case=TRUE))) ## links that are all digits
    accept <- function(z) grepl("\\.txt$",html_attr(z,"href"),ignore.case=TRUE) || grepl("d[12]_f(000|003|006|009|012|015|018|021|024|027)\\.grb$",html_attr(z,"href"),ignore.case=TRUE) ## which files to accept
    this_path_no_trailing_sep <- sub("[\\/]$","",directory_from_url(dataset$source_url))
    for (i in idx) { ## loop through directories
        target_dir <- sub("/$","",n[i])
        target_dir <- file.path(this_path_no_trailing_sep,sub("(00|12)$","",target_dir))
        ## make target_dir if it doesn't exist
        if (!dir.exists(target_dir)) {
            ok <- dir.create(target_dir)
            if (!ok) {
                cat(sprintf("Could not create target directory %s: aborting.\n",target_dir))
                return()
            }
        }
        x2 <- jump_to(x,n[i])
        files <- html_attr(Filter(accept,html_nodes(x2,"a")),"href")
        ## change into target directory, with no recursive fetch, to allow --timestamping on retrievals
        cwd <- getwd()
        setwd(target_dir)
        for (f in files) {
            ## loop through files to download
            file_url <- xml2::url_absolute(f,x2$url)
            dummy=dataset
            dummy$source_url=file_url
            wget_call=build_wget_call(dummy)
            do_wget(wget_call,dataset)
        }
        setwd(cwd)
    }
}
