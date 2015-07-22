#' Load data repository configuration file
#'
#' This configuration file specifies global settings that control the synchronization behaviour in general, and provides details of each of the datasets in the repository. Any global configuration setting can also be set at the dataset level. A dataset inherits any global configuration settings not overridden in its own configuration. It is suggested that a user only makes changes to the local_config_file, because package updates may change the default configuration file provided with the package. To use only a local_config_file, specify NULL for the default_config_file (and vice-versa).
#'
#' Each dataset is specified by the following parameters in the configuration file:
#' \itemize{
#' \item name string: dataset name
#' \item description string: description
#' \item reference string: URL to metadata record or home page for the dataset
#' \item source_urls list: list of URLs to the data
#' \item do_sync logical: if FALSE this dataset will not be synchronized
#' \item method string: the synchronization method (currently only "wget")
#' \item method_flags string: flags to pass to wget (if method=="wget") or the custom handler (if not)
#' \item postprocess string array: operations to apply after all file downloads. Currently "unzip", "gunzip" (unzip or gunzip but do not delete the compressed copy), "gunzip_delete", "unzip_delete" (unzip and delete the zipped file), "cleanup <pattern>" (remove any files in local_directory with names matching <pattern>), "cleanup_recursive <pattern>" (as for cleanup but is applied recursively to child directories)
#' \item user string: username for access to restricted ftp/http sites
#' \item password string: password for access to restricted ftp/http sites
#' }
#'
#' @param local_config_file string: file or URL to JSON configuration file.
#' @param default_config_file string: file or URL to JSON configuration file. By default, the default_config_file is the one that ships with this package.
#'
#' @export
read_repo_config=function(local_config_file,default_config_file=system.file("extdata","raad_repo_config.json",package="raadsync")) {
    ## check that config file is valid JSON
    dcf=NULL
    if (!is.null(default_config_file)) {
        if (nchar(default_config_file)<1 && missing(default_config_file)) {
            ##empty file name, couldn't find raad_repo_config.json in package files
            stop("could not find default config file in package (is the package installed?) Specify default_config_file=NULL for no default file")
        }
        dcf=readLines(default_config_file)
        if (!validate(dcf)) {
            stop("configuration file ",default_config_file," is not valid JSON")
        }
        dcf=fromJSON(dcf)
    }
    lcf=NULL
    if (!is.null(local_config_file) && !missing(local_config_file)) {
        lcf=readLines(local_config_file)
        if (!validate(lcf)) {
            stop("configuration file ",local_config_file," is not valid JSON")
        }
        lcf=fromJSON(lcf)
    }
    if (is.null(lcf) && is.null(dcf)) {
        stop("either default or local config file must be specified")
    }
    if (!is.null(lcf) && !is.null(dcf)) {
        ## overwrite default config values dcf with local ones lcf
        for (parm in names(lcf$global)) {
            dcf$global[[parm]]=lcf$global[[parm]]
        }
    } else if (is.null(dcf) && !is.null(lcf)) {
        dcf=lcf
        lcf=NULL
    }
    ## set wget flag for clobber
    if (dcf$global$clobber==1 && !(any(c("--timestamping","-N") %in% str_split(dcf$global$wget_flags,"[ ]+")))) {
        dcf$global$wget_flags=paste(dcf$global$wget_flags,"--timestamping",sep=" ")
    }
    ## also may need to remove timestamping from global wget flags if clobber==2, other settings
    # old code, needs re-using
    #switch(as.character(config$global$clobber),
    #       "0"={ wget_call=paste(wget_call,"--no-clobber",sep=" ") },
    #       "1"={ wget_call=paste(wget_call,"--timestamping",sep=" ") },
    #       "2"={ if (!missing(fileurl)) { output_file_name=basename(fileurl); wget_call=paste(wget_call,"-O",output_file_name,sep=" ") } }
    #             ## for "2" and url supplied, do nothing. this may not work as expected if we are downloading a file rather than a directory. wget -r (with no -nc or -N flags) should overwrite an existing file, but wget a_url may not
    #       )
    ## copy datasets from lcf to dcf
    if (!is.null(lcf)) {
        for (k in 1:nrow(lcf$datasets)) {
            idx=dcf$datasets$name==lcf$datasets[k,]$name
            if (sum(idx)>0) {
                ## this datasets exists in dcf
                ## update any values in dcf with lcf ones
                for (parm in names(lcf$datasets[k,])) {
                    if (!is.na(lcf$datasets[k,][[parm]])) {
                        ## if this parm in the local config dataframe is NA, then it wasn't set in the local config file
                        dcf$datasets[idx,][[parm]]=lcf$datasets[k,][[parm]]
                    }
                }
            } else {
                ## append this datasets to dcf
                dcf$datasets=rbind.fill(dcf$datasets,lcf$datasets[k,])
            }
        }
    }
    ## now check that all entries match what we expect
    assert_that(is.string(dcf$global$local_file_root)) ## just check that it's a string, not that it's a directory
    ## make sure that local_file_root path is in correct form for this system (but don't test its existence)
    dcf$global$local_file_root=normalizePath(dcf$global$local_file_root,mustWork=FALSE)

    ## check string parms and initialise to empty string if they aren't defined
    check_fields=c("wget_flags","http_proxy","ftp_proxy","user","password") ##"http_proxy_user","ftp_proxy_user"
    for (fi in check_fields) {
        if (is.null(dcf$global[[fi]]) || is.na(dcf$global[[fi]])) {
            dcf$global[[fi]]=""
        }
        assert_that(is.string(dcf$global[[fi]]))
    }
    if (is.null(dcf$global$skip_downloads) || is.na(dcf$global$skip_downloads)) {
        dcf$global$skip_downloads=FALSE
    }
    assert_that(is.flag(dcf$global$skip_downloads))
    if (is.null(dcf$global$clobber) || is.na(dcf$global$clobber)) {
        dcf$global$clobber=1
    }
    assert_that(dcf$global$clobber %in% c(0,1,2))
    if (is.null(dcf$global$wait) || is.na(dcf$global$wait)) {
        dcf$global$wait=0
    }
    assert_that(dcf$global$wait>=0)

    ## copy all global settings into each dataset unless already specified at the dataset level
    for (parm in names(dcf$global)) {
        if (!parm %in% names(dcf$datasets)) {
            dcf$datasets[[parm]]=dcf$global[[parm]]
        } else {
            temp=dcf$datasets[[parm]]
            dcf$datasets[[parm]][is.na(temp) | is.null(temp)]=dcf$global[[parm]]
        }
    }

    ## check dataset configs
    ## not complete!
    for (di in 1:nrow(dcf$datasets)) {
        dataset=dcf$datasets[di,]
        cat(sprintf("Checking config for dataset: %s ... ",dataset$name))
        assert_that(is.string(dataset$source_urls) || is.list(dataset$source_urls))
        assert_that(is.string(dataset$method_flags))
        assert_that(is.string(dataset$user))
        assert_that(is.string(dataset$password))
        cat("done.\n")
    }
    ##class(dcf)=c('repo_config',class(dcf)) ## class info not used yet - comment out for time being
    dcf$datasets
}

#' Produce summary of repository configuration
#'
#' @param repo_config data.frame: configuration as returned by read_repo_config
#' @param file string: path to file to write summary to. A temporary file is used by default
#' @param format string: produce HTML ("html") or Rmarkdown ("Rmd") file?
#'
#' @return Path to the summary file in HTML or Rmarkdown format
#'
#' @export
repo_summary=function(repo_config,file=tempfile(),format="html") {
    assert_that(is.string(file))
    assert_that(is.string(format))
    format=match.arg(tolower(format),c("html","rmd"))

    ## for backwards compatibility with older configs that might not have these columns
    if (is.null(repo_config$access_function)) {
        repo_config$access_function=""
    }
    if (is.null(repo_config$data_group)) {
        repo_config$data_group=""
    }

    ## write summary as temporary Rmd file
    rmd_file=tempfile(fileext=".Rmd")
    cat("---\ntitle: \"Summary of raadsync repository\"\ndate: \"",date(),"\"\noutput:\n  html_document:\n    toc: true\n    theme: cerulean\n    highlight: default\n---\n\n",file=rmd_file,append=FALSE)

    cat("Summary of raadsync repository\n========\n",file=rmd_file,append=TRUE)

    repo_config$data_group[repo_config$data_group==""]=NA ## so that arrange puts them last
    repo_config=arrange(repo_config,data_group)
    repo_config$data_group[is.na(repo_config$data_group)]=""
    last_group="blah"
    for (k in 1:nrow(repo_config)) {
        if (last_group!=repo_config$data_group[k]) {
            cat("\n## Data group: ",repo_config$data_group[k],"\n",file=rmd_file,append=TRUE)
        }
        last_group=repo_config$data_group[k]
        cat("\n### ",repo_config$name[k],"\n",file=rmd_file,append=TRUE)
        cat("\n",repo_config$description[k],"\n",file=rmd_file,append=TRUE)
        cat("\nReference: ",repo_config$reference[k],"\n",file=rmd_file,append=TRUE)
        this_citation=repo_config$citation[k]
        if (is.null(this_citation) || is.na(this_citation) || this_citation=="") {
            this_citation="No citation details provided; see reference"
        }
        cat("\nCitation: ",this_citation,"\n",file=rmd_file,append=TRUE)
        this_license=repo_config$license[k]
        if (is.null(this_license) || is.na(this_license) || this_license=="") {
            this_license="No formal license details provided; see reference"
        }
        cat("\nLicense: ",this_license,"\n",file=rmd_file,append=TRUE)
        thisfun=repo_config$access_function[k]
        if (is.null(thisfun) || is.na(thisfun) || thisfun=="") { thisfun="none registered" }
        cat("\nAssociated access functions: ",thisfun,"\n",file=rmd_file,append=TRUE)
        cat("\nIs currently synchronized: ",repo_config$do_sync[k],"\n",file=rmd_file,append=TRUE)
    }

    if (format=="html") {
        ## knit to html
        knit2html(rmd_file,output=sub("Rmd$","md",rmd_file))
        sub("Rmd$","html",rmd_file)
    } else {
        rmd_file
    }
}
