# Get the local raadsync config file
#
# The defaults (filename "local_raadsync_config.json" under the subpath "admin" reflect the habits of the raadsync package authors
#
# @param name string: filename of the config file
# @param subpath string: the subpath under default.datadir to look in
#
# @return path to the config file. A warning will be issued if it does not exist. If option default.datadir does not exist, a warning will be issued and NULL returned
#
# @export
get_local_config <- function(name = "local_raadsync_config.json", subpath = "admin") {
    ddir <- getOption("raadtools.default.datadir")
    if (is.null(ddir)) ddir <- getOption("default.datadir") ## the old option, for backwards-compatibility
    if (is.null(ddir)) {
        warning("option raadtools.default.datadir is not set")
        path <- NULL
    } else {
        path <- file.path(ddir, subpath, name)
        if (!file.exists(path)) warning(sprintf("directory does not exist %s", path))
    }
    path
}

##-------------------------------------------
## various helper functions
## not exported for user

save_current_settings=function() {
    return(list(working_dir=getwd(), ## current working directory
                env_http_proxy=Sys.getenv("http_proxy"), ## proxy env vars
                env_https_proxy=Sys.getenv("https_proxy"),
                env_ftp_proxy=Sys.getenv("ftp_proxy")
                ))
}

restore_settings=function(settings) {
    setwd(settings$working_dir)
    Sys.setenv(http_proxy=settings$env_http_proxy)
    Sys.setenv(https_proxy=settings$env_https_proxy)
    Sys.setenv(ftp_proxy=settings$env_ftp_proxy)
    invisible(NULL)
}

directory_from_url=function(this_url) {
    this_url=sub("^(http|https|ftp)://","",this_url)
    this_url=sub(":","+",this_url) ## port
    ## discard anything after the last trailing slash if it includes asterisks (is a file mask)
    ##sub("/[^/]*\\*[^/]*$","/",this_url)
    ## discard anything at all after the last trailing slash
    sub("/[^/]*$","/",this_url)
}

find_changed_files=function(file_list_before,file_list_after,filename_pattern=".*") {
    ## expect both file_list_before and file_list_after to be a data.frame from file.info()
    ## detect changes on basis of ctime and size attributes
    ## returns names only
    changed_files=setdiff(rownames(file_list_after),rownames(file_list_before)) ## anything that has appeared afterwards
    for (thisf in intersect(rownames(file_list_after),rownames(file_list_before))) {
        ## files in both
        thisfile_after=file_list_after[rownames(file_list_after)==thisf,]
        thisfile_before=file_list_before[rownames(file_list_before)==thisf,]
        if ((thisfile_after$ctime>thisfile_before$ctime) | (thisfile_after$size!=thisfile_before$size)) {
            changed_files=c(changed_files,thisf)
        }
    }
    changed_files=changed_files[str_detect(changed_files,filename_pattern)]
    if (is.null(changed_files)) {
        c()
    } else {
        changed_files
    }
}

##uncompress=function(filename,outname=sub("\\.z$","",filename,ignore.case=TRUE)) {
##}

do_decompress_files=function(method,files,overwrite=TRUE) {
    ## decompress (unzip/gunzip) compressed files
    ## this function overwrites existing decompressed files if overwrite is TRUE
    assert_that(is.string(method))
    method=match.arg(method,c("unzip","unzip_delete","gunzip","gunzip_delete","bunzip2","bunzip2_delete","uncompress","uncompress_delete"))
    if (grepl("uncompress",method)) {
        ## uncompress uses archive::file_read, which is a suggested package
        ## check that we have it
        if (!requireNamespace("archive",quietly=TRUE))
            stop("the archive package is needed for uncompress functionality")

    }
    ## unzip() issues warnings in some cases when operations have errors, and sometimes issues actual errors
    warn=getOption("warn") ## save current setting
    options(warn=0) ## so that we can be sure that last.warning will be set
    ## MDS: this should use tail(warnings(), 1) instead
    last.warning <- NULL ## to avoid check note
    for (thisf in files) {
        ## decompress, check for errors in doing so
        cat(sprintf("  decompressing: %s ... ",thisf))
        switch(method,
               "unzip_delete"=,
               "unzip"={
                   was_ok<-FALSE
                   suppressWarnings(warning("")) ## clear last.warning message
                   ## unzip will put files in the current directory by default, so we need to extract the target directory for this file
                   target_dir<-dirname(thisf)
##cat(sprintf("\nWe are in %s\n",getwd()))
##cat(sprintf("thisf is: %s\n",thisf))
                   tryCatch({ unzipped_files<-unzip(thisf,list=TRUE) ## get list of files in archive
##cat("unzipped files: ")
##cat(str(unzipped_files),"\n")
                              files_to_extract<-unzipped_files$Name
                              if (!overwrite) {
                                  ## extract only files that don't exist
                                  files_to_extract<-files_to_extract[!file.exists(file.path(target_dir,files_to_extract))]
                              }
##cat("files to extract: ")
##cat(str(files_to_extract),"\n")
                              if (length(files_to_extract)>0) {
                                  cat(sprintf('extracting %d files into %s ... ',length(files_to_extract),target_dir))
                                  unzip(thisf,files=files_to_extract,exdir=target_dir) ## now actually unzip them
                                  was_ok<-is.null(last.warning[[1]]) && all(file.info(files_to_extract)$size>0)
                              } else {
                                  cat(sprintf('no new files to extract (not overwriting existing files) ... '))
                                  was_ok<-TRUE
                              }
                              cat("done.\n")
                          },
                            error=function(e) {
                                ## an error here might be because of an incompletely-downloaded file. Is there something more sensible to do in this case?
                                cat(sprintf("  %s failed to unzip (it may be incompletely-downloaded?)\n Error message was: %s",thisf,e))
                            })
                   if (identical(method,"unzip_delete")) {
                       ## if all looks OK, delete zipped file
                       if (was_ok) {
                           cat(sprintf("  unzip of %s appears OK, deleting\n",thisf))
                           unlink(thisf)
                       } else {
                           cat(sprintf("  problem unzipping %s, not deleting\n",thisf))
                       }
                   }
               },
               "gunzip_delete"=,
               "gunzip"={
                   ## gunzip takes care of deleting the compressed file if remove is TRUE
                   unzip_this<-TRUE
                   if (!overwrite) {
                       ## check if file exists, so that we can issue a more informative trace message to the user
                       destname<-gsub("[.]gz$","",thisf,ignore.case=TRUE)
                       if (file.exists(destname)) {
                           cat(sprintf(" uncompressed file exists, skipping ... "))
                           unzip_this<-FALSE
                       }
                   }
                   if (unzip_this) {
                       ## wrap this in tryCatch block so that errors do not halt our whole process
                       tryCatch(gunzip(thisf,destname=sub("\\.gz$","",thisf),remove=method=="gunzip_delete",overwrite=overwrite),
                                error=function(e){
                                    cat(sprintf("  problem gunzipping %s: %s",thisf,e))
                                }
                                )
                   }
                   cat(sprintf("done\n"))
               },
               "bunzip2_delete"=,
               "bunzip2"={
                   ## same as for gunzip
                   unzip_this<-TRUE
                   if (!overwrite) {
                       ## check if file exists, so that we can issue a more informative trace message to the user
                       destname<-gsub("[.]bz2$","",thisf,ignore.case=TRUE)
                       if (file.exists(destname)) {
                           cat(sprintf(" uncompressed file exists, skipping ... "))
                           unzip_this<-FALSE
                       }
                   }
                   if (unzip_this) {
                       ## wrap this in tryCatch block so that errors do not halt our whole process
                       tryCatch(bunzip2(thisf,destname=sub("\\.bz2$","",thisf),remove=method=="bunzip2_delete",overwrite=overwrite),
                                error=function(e){
                                    cat(sprintf("  problem bunzipping %s: %s",thisf,e))
                                }
                                )
                   }
                   cat(sprintf("done\n"))
               },
               "uncompress_delete"=,
               "uncompress"={
                   unzip_this<-TRUE
                   destname <- gsub("\\.Z$","",thisf,ignore.case=TRUE)
                   if (!overwrite) {
                       ## check if file exists, so that we can issue a more informative trace message to the user
                       if (file.exists(destname)) {
                           cat(sprintf(" uncompressed file exists, skipping ... "))
                           unzip_this <- FALSE
                       }
                   }
                   if (unzip_this) {
                       ## wrap this in tryCatch block so that errors do not halt our whole process
                       tryCatch({
                           fsize <- ceiling(file.info(thisf)$size/1e4)*1e4
                           ff <- archive::file_read(thisf)
                           writeBin(readBin(ff,"raw",fsize),destname)
                           close(ff)
                           if (grepl("delete",method)) file.remove(thisf)
                       },
                       error=function(e){
                           cat(sprintf("  problem uncompressing %s: %s",thisf,e))
                       })
                   }
                   cat(sprintf("done\n"))
               },
               stop("unsupported decompress method ",method)
               )
    }
    options(warn=warn) ## reset
}

dir_exists=function(z) file.exists(dirname(z)) && !(!file.info(z)$isdir || is.na(file.info(z)$isdir))


##-------------------------------------------
## helper functions for wget

build_wget_call=function(dataset) {
    ## build wget system call given our dataset config
    wget_call="wget"
    ## resolve fileset-specific flags (method_flags) and global flags (wget_flags, which were inherited from config$global)
    this_flags=resolve_wget_flags(dataset$method_flags,dataset$wget_flags)
    ## proxy-user and proxy-password flags
    ## this needs to decide whether it should use http_proxy_user or ftp_proxy_user info - exclude for now
    #if (!grepl("proxy-user=",dataset$wget_flags)) {
    #    dataset$wget_flags=paste(dataset$wget_flags,paste0("--proxy-user=",dataset$http_proxy_user),sep=" ")
    #}
    #if (!grepl("proxy-password=",dataset$wget_flags)) {
    #    dataset$wget_flags=paste(dataset$wget_flags,paste0("--proxy-password=",dataset$http_proxy_password),sep=" ")
    #}
    if (nchar(dataset$user)>0) {
        wget_call=paste0(wget_call," --user=",dataset$user)
    }
    if (nchar(dataset$password)>0) {
        wget_call=paste0(wget_call," --password=",dataset$password)
    }
    wget_call=paste(wget_call,this_flags,sep=" ")
    if (dataset$wait>0) {
        wget_call=paste0(wget_call," --wait=",dataset$wait)
    }
    wget_call=paste(wget_call,dataset$source_url,sep=" ")
    wget_call
}

do_wget <- function(wget_call,dataset) {
    assert_that(is.string(wget_call))
    if (dataset$skip_downloads) {
        cat(sprintf(" skip_downloads is TRUE, not executing: %s\n",wget_call))
    } else {
        cat(sprintf(" executing: %s\n",wget_call))
        if (sink.number()>0) {
            ## we have a sink() redirection in place
            ## sink() won't catch the output of system commands, which means we miss stuff in our log
            ## workaround: send output to temporary file so that we can capture it
            output_file <- gsub("\\\\","\\\\\\\\",tempfile()) ## escape backslashes
            wget_call <- sub("^wget ",paste0("wget -o \"",output_file,"\" "),wget_call)
            system(wget_call)
            ## now echo the contents of output_file to console, so that sink() captures it
            cat(readLines(output_file),sep="\n")
        } else {
            system(wget_call)
        }
    }
}

resolve_wget_flags=function(primary_flags,secondary_flags) {
    wgf=str_split(primary_flags,"[ ]+")[[1]]
    secondary_flags=str_split(secondary_flags,"[ ]+")[[1]]
    for (thisflag in setdiff(secondary_flags,wgf)) {
        switch(thisflag,
               "-N"=,
               "--timestamping"={ if (! any(c("--no-clobber","-nc") %in% wgf)) wgf<-c(wgf,thisflag) },
               "-nc"=,
               "--no-clobber"={ if (! any(c("--timestamping","-N") %in% wgf)) wgf<-c(wgf,thisflag) },
               wgf<-c(wgf,thisflag))
    }
    paste(wgf,sep="",collapse=" ")
}
