#' Run the data repository synchronisation
#'
#' @param config data.frame: configuration as returned by \code{\link{read_repo_config}}
#' @param create_root logical: should the data root directory be created if it does not exist?
#' @param verbose logical: if TRUE, provide additional progress output
#'
#' @return vector of logical values indicating success of each data source in config. NA indicates that the data source was not run (e.g. do_sync set to FALSE)
#'
#' @export
sync_repo <- function(config,create_root=FALSE,verbose=TRUE) {
    ## general synchronization handler
    assert_that(is.data.frame(config))
    assert_that(is.flag(create_root))
    assert_that(is.flag(verbose))
    ## check that wget can be found
    tryCatch(
        system("wget --help",intern=TRUE),
        error=function(e) stop("could not find wget executable (error message was: ",e,")")
    )
    ## save some current settings: path and proxy env values
    settings=save_current_settings()
    ## iterate through each dataset in turn
    sync_ok <- rep(FALSE,nrow(config))
    for (di in 1:nrow(config)) {
        tryCatch(
            sync_ok[di] <- do_sync_repo(config[di,],create_root,verbose,settings),
            error=function(e) {
                cat("\nThere was a problem synchronizing the dataset:",config$name[di],". The error message was:",e$message,"\n")
            }
        )
    }
    restore_settings(settings)
    sync_ok
}


do_sync_repo <- function(this_dataset,create_root,verbose,settings) {
    on.exit({ restore_settings(settings) })
    ## check that the root directory exists
    if (!dir_exists(this_dataset$local_file_root)) {
        ## no, it does not exist
        ## unless create_root is TRUE, we won't create it, in case the user simply hasn't specified the right location
        if (create_root) {
            dir.create(this_dataset$local_file_root,recursive=TRUE)
        } else {
            stop("local_file_root: ",this_dataset$local_file_root," does not exist. Either create it or run sync_repo with create_root=TRUE")
        }
    }
    if (!this_dataset$do_sync) {
        cat(sprintf("\n%s\nSkipping dataset (do_sync is FALSE): %s\n----------------------------------------------------------------------------------------------------------\n\n",base::date(),this_dataset$name))
        return(NA)
    }
    cat(sprintf("\n%s\nSynchronizing dataset: %s\n----------------------------------------------------------------------------------------------------------\n\n",base::date(),this_dataset$name))
    setwd(this_dataset$local_file_root)

    ## set proxy env vars
    if (verbose) cat(sprintf(" setting proxy variables ... "))
    Sys.setenv(http_proxy=this_dataset$http_proxy)
    Sys.setenv(https_proxy=this_dataset$http_proxy)
    Sys.setenv(ftp_proxy=this_dataset$ftp_proxy)
    if (verbose) cat(sprintf("done.\n"))

    if (is.character(this_dataset$source_urls)) {
        ## was expecting a list
        this_dataset$source_urls=list(this_dataset$source_urls)
    }
    ## do the main synchonization, usually directly with wget, otherwise with custom methods
    for (si in 1:length(this_dataset$source_urls[[1]])) {
        ## iterate through source_urls
        this_dataset$source_url=this_dataset$source_urls[[1]][[si]]
        cat(sprintf("\n---\nProcessing source_url: %s\n",this_dataset$source_url))

        ## postprocessing
        pp=this_dataset$postprocess
        if (is.list(pp) && length(pp)==1) {
            pp=pp[[1]] ## may get char vector embedded in single-element list
        }
        if (!is.null(pp) && pp %in% c(NA,"NA")) pp=NULL
        pp=tolower(pp)
        pp=Filter(nchar,pp) ## drop empty entries
        this_path_no_trailing_sep=sub("[\\/]$","",directory_from_url(this_dataset$source_url))
        if (verbose) {
            cat(sprintf(" this dataset path is: %s\n",this_path_no_trailing_sep))
        }
        file_pattern=sub(".*/","",this_dataset$source_url)
        if (nchar(file_pattern)<1) file_pattern=NULL
        if (any(this_dataset$method==c("aadc_portal","aadc_eds"))) { file_pattern=NULL } ## set to null so that file_list_* (below) searches the data directory
        ## build file list if postprocessing required
        if (length(pp)>0) {
            ## take snapshot of this directory before we start syncing
            if (verbose) {
                cat(sprintf(" building file list ... "))
            }
            file_list_before=file.info(list.files(path=this_path_no_trailing_sep,pattern=file_pattern,recursive=TRUE,full.names=TRUE)) ## full.names TRUE so that names are relative to current working directory
            if (file.exists(this_path_no_trailing_sep)) {
                ## in some cases this points directly to a file
                temp=file.info(this_path_no_trailing_sep)
                temp=temp[!temp$isdir,]
                if (nrow(temp)>0) { file_list_before=rbind(file_list_before,temp) }
            }
            ##cat("file list before:\n")
            ##cat(str(file_list_before),"\n")
            if (verbose) cat(sprintf("done.\n"))
        }
        if (this_dataset$method=="wget") {
            do_wget(build_wget_call(this_dataset),this_dataset)
        } else if (this_dataset$method=="aadc_portal") {
            ## clumsy way to get around AADC portal file-renaming issue
            ## e.g. if we ask for http://data.aad.gov.au/aadc/portal/download_file.cfm?file_id=1234
            ## then we get local file named data.aad.gov.au/aadc/portal/download_file.cfm@file_id=1234 or similar
            ## if we set --content-disposition then the file name is read from the http header, but we lose the directory structure
            ##  (the file is placed in the local_file_root directory)
            ## Change 8-Dec-2015 - change into subdirectory named by file_id of file, so that we don't get files mixed together in data.aad.gov.au/aadc/portal/
            ## note that this requires the "--recursive" flag NOT TO BE USED
            this_file_id=str_match(this_dataset$source_url,"file_id=(\\d+)")[2]
            if (!file.exists(file.path(this_dataset$local_file_root,"data.aad.gov.au","aadc","portal",this_file_id))) {
                dir.create(file.path(this_dataset$local_file_root,"data.aad.gov.au","aadc","portal",this_file_id),recursive=TRUE)
            }
            setwd(file.path(this_dataset$local_file_root,"data.aad.gov.au","aadc","portal",this_file_id))
            if (!grepl("--content-disposition",this_dataset$method_flags,ignore.case=TRUE)) {
                this_dataset$method_flags=paste(this_dataset$method_flags,"--content-disposition",sep=" ")
            }
            ## these two should be doable in a single regex, but done separately until I can figure it out
            if (grepl("--recursive ",this_dataset$method_flags,ignore.case=TRUE)) {
                this_dataset$method_flags=str_trim(sub("--recursive ","",this_dataset$method_flags))
            }
            if (grepl("--recursive$",this_dataset$method_flags,ignore.case=TRUE)) {
                this_dataset$method_flags=str_trim(sub("--recursive$","",this_dataset$method_flags))
            }
            do_wget(build_wget_call(this_dataset),this_dataset)
            setwd(this_dataset$local_file_root)
            ## note that unzipping of files with method aadc_portal is odd, if there are multiple source_urls defined. The second and after will get unzipped multiple times
        } else if (this_dataset$method=="aadc_eds") {
            ## clumsy way to get around AADC EDS file naming issues
            ## e.g. if we ask for http://data.aad.gov.au/eds/file/4494
            ## then we get local file named data.aad.gov.au/eds/file/4494 (which is most likely a zipped file)
            ## if we unzip this here, we get this zip's files mixed with others
            ## change into subdirectory named by file_id of file, so that we don't get files mixed together in data.aad.gov.au/eds/file/
            ## note that this requires the "--recursive" flag NOT TO BE USED
            this_file_id=str_match(this_dataset$source_url,"/file/(\\d+)$")[2]
            if (!file.exists(file.path(this_dataset$local_file_root,"data.aad.gov.au","eds","file",this_file_id))) {
                dir.create(file.path(this_dataset$local_file_root,"data.aad.gov.au","eds","file",this_file_id),recursive=TRUE)
            }
            setwd(file.path(this_dataset$local_file_root,"data.aad.gov.au","eds","file",this_file_id))
            if (!grepl("--content-disposition",this_dataset$method_flags,ignore.case=TRUE)) {
                this_dataset$method_flags=paste(this_dataset$method_flags,"--content-disposition",sep=" ")
            }
            ## these two should be doable in a single regex, but done separately until I can figure it out
            if (grepl("--recursive ",this_dataset$method_flags,ignore.case=TRUE)) {
                this_dataset$method_flags=str_trim(sub("--recursive ","",this_dataset$method_flags))
            }
            if (grepl("--recursive$",this_dataset$method_flags,ignore.case=TRUE)) {
                this_dataset$method_flags=str_trim(sub("--recursive$","",this_dataset$method_flags))
            }
            do_wget(build_wget_call(this_dataset),this_dataset)
            setwd(this_dataset$local_file_root)
            ## note that unzipping of files is odd, if there are multiple source_urls defined. The second and after will get unzipped multiple times
        } else if (exists(this_dataset$method,mode="function")) {
            ## dispatch to custom handler
            if (verbose) cat(sprintf(" using custom handler \"%s\"\n",this_dataset$method))
            eval(parse(text=paste0(this_dataset$method,"(this_dataset)")))
        } else {
            stop("unsupported method ",this_dataset$method," specified")
        }

        ## build file list if postprocessing required
        if (length(pp)>0) {
            if (verbose) { cat(sprintf(" building post-download file list of %s ... ",this_path_no_trailing_sep)) }
            file_list_after=file.info(list.files(path=this_path_no_trailing_sep,pattern=file_pattern,recursive=TRUE,full.names=TRUE))
            if (file.exists(this_path_no_trailing_sep)) {
                ## in some cases this points directly to a file
                temp=file.info(this_path_no_trailing_sep)
                temp=temp[!temp$isdir,]
                if (nrow(temp)>0) { file_list_after=rbind(file_list_after,temp) }
            }
            ##cat("file list after:\n")
            ##cat(str(file_list_after),"\n")
            if (verbose) cat(sprintf("done.\n"))
        }

        ## AMPS
##         if (this_dataset$name=="Antarctic Mesoscale Prediction System grib files") {
##             dirlist <- list.dirs(this_path_no_trailing_sep,recursive=FALSE)
##             dirlist <- dirlist[nchar(basename(dirlist))==10] ## just those of the format yyyymmddPP where PP is the prediction time 00 or 12
##             for (i in dirlist) {
##                 newdir <- sub("(00|12)$","",i)
##                 if (!dir.exists(newdir)) dir.create(newdir)
##                 file_list <- dir(i,pattern="\\.(grb|txt)$",full.names=TRUE)
##                 if (length(file_list)>0) {
##                     if (verbose) cat(sprintf(" moving %d files from %s to %s ... ",length(file_list),i,newdir))
##                     copy_ok <- sapply(file_list,function(z)file.rename(from=z,to=file.path(newdir,basename(z))))
##                     if (verbose) cat(sprintf("done.\n"))
##                 }
##                 ## should be no files left in i
##                 if (length(dir(i))<1) unlink(i,recursive=TRUE)
##             }
##         }
        ## decompression behaviour: for *_delete, unconditionally decompress all compressed files and then delete them
        ## for gunzip/bunzip2 (which can only contain a single file), decompress only if .gz/.bz2 file has changed
        ## for unzip (which can contain multiple files), decompress all if the zip file has changed, or if there are any files present in the zip file that don't exist in decompressed form
        if (length(pp)>0) {
            for (i in 1:length(pp)) {
                if (pp[i]=="unzip_delete") {
                    ## unconditionally decompress any zipped files and then delete them
                    files_to_decompress=list.files(directory_from_url(this_dataset$source_url),pattern="\\.zip$",recursive=TRUE)
                    do_decompress_files(pp[i],files=files_to_decompress)
                } else if (pp[i] %in% c("gunzip_delete","bunzip2_delete","uncompress_delete")) {
                    ## unconditionally unzip then delete
                    file_pattern <- switch(pp[i],
                                           "gunzip_delete"="\\.gz$",
                                           "bunzip2_delete"="\\.bz2$",
                                           "uncompress_delete"="\\.Z$",
                                           stop("unrecognized decompression")
                                           )
                    files_to_decompress=list.files(directory_from_url(this_dataset$source_url),pattern=file_pattern,recursive=TRUE)
                    do_decompress_files(pp[i],files=files_to_decompress)
                } else if (pp[i] %in% c("gunzip","bunzip2","uncompress")) {
                    ## decompress but retain compressed file. decompress only if .gz/.bz2 file has changed
                    file_pattern <- switch(pp[i],
                                           "gunzip"="\\.gz$",
                                           "bunzip2"="\\.bz2$",
                                           "uncompress"="\\.Z$",
                                           stop("unrecognized decompression")
                                           )
                    files_to_decompress=find_changed_files(file_list_before,file_list_after,file_pattern)
                    do_decompress_files(pp[i],files=files_to_decompress)
                    ## also decompress if uncompressed file does not exist
                    files_to_decompress=setdiff(rownames(file_list_after),files_to_decompress) ## those that we haven't just dealt with
                    files_to_decompress=files_to_decompress[str_detect(files_to_decompress,file_pattern)] ## only .gz/.bz2 files
                    do_decompress_files(pp[i],files=files_to_decompress,overwrite=FALSE)
                    ## nb this may be slow, so might be worth explicitly checking for the existence of uncompressed files
                } else if (pp[i]=="unzip") {
                    ## decompress but retain compressed file
                    ## decompress unconditionally if the zip file has changed
                    files_to_decompress=find_changed_files(file_list_before,file_list_after,"\\.zip$")
                    do_decompress_files(pp[i],files=files_to_decompress)
                    ## also decompress any files present in the zip file that don't exist in decompressed form
                    files_to_decompress=setdiff(rownames(file_list_after),files_to_decompress) ## those that we haven't just dealt with
                    files_to_decompress=files_to_decompress[str_detect(files_to_decompress,"\\.zip$")] ## only zip files
                    do_decompress_files(pp[i],files=files_to_decompress,overwrite=FALSE)
                } else if (grepl("^cleanup",pp[i])) {
                    file_pattern=sub("(cleanup|cleanup_recursive) ","",pp[i])
                    recursive=grepl("^cleanup_recursive",tolower(pp[i]))
                    to_delete=list.files(pattern=file_pattern,recursive=recursive)
                    cat(sprintf("cleaning up files: %s\n",paste(to_delete,collapse=",")))
                    unlink(to_delete)
                } else if (nchar(pp[i])<1) {
                    ## empty string, do nothing
                } else {
                    stop("unrecognized postprocess option ",pp[i])
                }
            }
        }
    } ## end looping through multiple source urls
    cat(sprintf("\n%s dataset synchronization complete: %s\n",base::date(),this_dataset$name))
    TRUE
}
