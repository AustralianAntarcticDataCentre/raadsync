#' Run the data repository synchronisation
#'
#' @param config data.frame: configuration as returned by \code{\link{read_repo_config}}
#' @param create_root logical: should the data root directory be created if it does not exist?
#' @param verbose logical: if TRUE, provide additional progress output
#'
#' @export
sync_repo=function(config,create_root=FALSE,verbose=TRUE) {
    ## general synchronization handler
    ## check that wget can be found
    tryCatch(
        system("wget --help",intern=TRUE),
        error=function(e) stop("could not find wget executable (error message was: ",e,")")
    )
    ## save some current settings
    settings=save_current_settings()
    ## iterate through each dataset in turn
    for (di in 1:nrow(config)) {
        this_dataset=config[di,]
        ## check that the root directory exists
        if (!dir_exists(this_dataset$local_file_root)) {
            ## no, it does not exist
            ## unless create_root is TRUE, we won't create it, in case the user simply hasn't specified the right location
            if (create_root) {
                dir.create(this_dataset$local_file_root,recursive=TRUE)
            } else {
                setwd(working_dir)
                restore_settings(settings)
                stop("local_file_root: ",this_dataset$local_file_root," does not exist. Either create it or run sync_repo with create_root=TRUE")
            }
        }
        if (!this_dataset$do_sync) {
            cat(sprintf("\nSkipping dataset (do_sync is FALSE): %s\n----------------------------------------------------------------------------------------------------------\n\n",this_dataset$name))
            next
        }
        cat(sprintf("\nSynchronizing dataset: %s\n----------------------------------------------------------------------------------------------------------\n\n",this_dataset$name))
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
            ## take snapshot of this directory before we start syncing
            this_path_no_trailing_sep=sub("[\\/]$","",directory_from_url(this_dataset$source_url))
            if (verbose) cat(sprintf(" building file list ... "))
            file_pattern=sub(".*/","",this_dataset$source_url)
            if (nchar(file_pattern)<1) file_pattern=NULL
            file_list_before=file.info(list.files(path=this_path_no_trailing_sep,pattern=file_pattern,recursive=TRUE,full.names=TRUE)) ## full.names TRUE so that names are relative to current working directory
            if (verbose) cat(sprintf("done.\n"))
            if (this_dataset$method=="wget") {
                do_wget(build_wget_call(this_dataset),this_dataset)
            } else if (exists(this_dataset$method,mode="function")) {
                ## dispatch to custom handler
                if (verbose) cat(sprintf(" using custom handler \"%s\"\n",this_dataset$method))
                eval(parse(text=paste0(this_dataset$method,"(this_dataset)")))
            } else {
                restore_settings(settings)
                stop("unsupported method ",this_dataset$method," specified")
            }

            ## snapshot after syncing
            if (verbose) cat(sprintf(" building post-download file list ... "))
            file_list_after=file.info(list.files(path=this_path_no_trailing_sep,pattern=file_pattern,recursive=TRUE,full.names=TRUE))
            if (verbose) cat(sprintf("done.\n"))

            ## postprocessing
            pp=this_dataset$postprocess
            if (is.list(pp) && length(pp)==1) {
                pp=pp[[1]] ## may get char vector embedded in single-element list
            }
            if (!is.null(pp) && pp %in% c(NA,"NA")) pp=NULL
            pp=tolower(pp)

            ## decompression behaviour: for *_delete, unconditionally decompress all compressed files and then delete them
            ## for gunzip/bunzip2 (which can only contain a single file), decompress only if .gz/.bz2 file has changed
            ## for unzip (which can contain multiple files), decompress all if the zip file has changed, or if there are any files present in the zip file that don't exist in decompressed form

            if (length(pp)>0) {
                for (i in 1:length(pp)) {
                    if (pp[i]=="unzip_delete") {
                        ## unconditionally decompress any zipped files and then delete them
                        files_to_decompress=list.files(directory_from_url(this_dataset$source_url),pattern="\\.zip$",recursive=TRUE)
                        do_decompress_files(pp[i],files=files_to_decompress)
                    } else if (pp[i] %in% c("gunzip_delete","bunzip2_delete")) {
                        ## unconditionally unzip then delete
                        file_pattern=ifelse(pp[i]=="gunzip_delete","\\.gz$","\\.bz2$")
                        files_to_decompress=list.files(directory_from_url(this_dataset$source_url),pattern=file_pattern,recursive=TRUE)
                        do_decompress_files(pp[i],files=files_to_decompress)
                    } else if (pp[i] %in% c("gunzip","bunzip2")) {
                        ## decompress but retain compressed file. decompress only if .gz/.bz2 file has changed
                        file_pattern=ifelse(pp[i]=="gunzip","\\.gz$","\\.bz2$")
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
                        restore_settings(settings)
                        stop("unrecognized postprocess option ",pp[i])
                    }
                }
            }
        } ## end looping through multiple source urls
    }
    restore_settings(settings)
}
