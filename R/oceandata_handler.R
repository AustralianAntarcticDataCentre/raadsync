oceandata=function(dataset) {
    ## oceandata synchronisation handler

    ## oceandata provides a file search interface, e.g.:
    ## wget -q --post-data="cksum=1&search=A2002*DAY_CHL_chlor*9km*" -O - http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi
    ## or
    ## wget -q --post-data="dtype=L3b&cksum=1&search=A2014*DAY_CHL.*" -O - http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi
    ## returns list of files and SHA1 checksum for each file
    ## each file can be retrieved from http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/filename

    ## expect that dataset$method_flags will contain the search and dtype components of the post string
    ##  i.e. "search=...&dtype=..." in "dtype=L3m&addurl=1&results_as_file=1&search=A2002*DAY_CHL_chlor*9km*"
    ##  or just include the data type in the search pattern e.g. "search=A2002*L3m_DAY_CHL_chlor*9km*

    assert_that(is.string(dataset$method_flags))
    myfiles=system(paste0("wget -q --post-data=\"cksum=1&",dataset$method_flags,"\" -O - http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi"),intern=TRUE)
    myfiles=myfiles[-c(1,2)] ## get rid of header line and blank line that follows it
    myfiles=ldply(str_split(myfiles,"[[:space:]]+")) ## split checksum and file name from each line
    colnames(myfiles)=c("checksum","filename")
    ## for each file, download if needed and store in appropriate directory
    for(idx in 1:nrow(myfiles)) {
        this=myfiles[idx,]
        this_url=paste0("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",this$filename) ## full URL
        this_fullfile=oceandata_url_mapper(this_url) ## where local copy will go
        if (is.null(this_fullfile)) {
            next
        }
        this_exists=file.exists(this_fullfile)
        download_this=!this_exists
        if (dataset$clobber==0) {
            ## don't clobber existing
        } else if (dataset$clobber==1) {
            ## replace existing if server copy newer than local copy
            ## use checksum rather than dates for this
            if (this_exists) {
                existing_checksum=calculate_checksum(this_fullfile)
                download_this=existing_checksum!=this$checksum
            }
        } else {
            download_this=TRUE
        }
        if (download_this) {
            dummy=dataset
            dummy$method_flags=paste("--progress=dot:giga","--recursive","--directory-prefix",oceandata_url_mapper(this_url,path_only=TRUE),"--cut-dirs=2","--no-host-directories",sep=" ")
            dummy$source_url=this_url
            wget_call=build_wget_call(dummy)
            do_wget(wget_call,dataset)
            ## recalculate checksum so that cache gets updated
            blah=calculate_checksum(this_fullfile)
        } else {
            if (this_exists) {
                cat(sprintf("not downloading %s, local copy exists with identical checksum\n",this$filename))
            }
        }
    }

}

## actual function to calculate SHA1 checksums
do_calculate_checksum=function(file) {
    checksum=NULL
    if (.Platform$OS.type=="unix") {
        ## try using openssl, since it's faster than digest for these files
        try({ temptxt<-system(paste0("openssl sha1 ",file),intern=TRUE)
            checksum=gsub("^.*=\\s+","",temptxt)
        },silent=TRUE)
    }
    if (is.null(checksum)) {
        checksum=digest(file=file,algo="sha1")
    }
    checksum
}

## and a memoized version, that will use a cached result if available (which will be much faster for large files)
calculate_checksum=addMemoization(do_calculate_checksum)


#' Satellite platform names and abbreviations used in Oceancolor URLs and file names
#' Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
#' For example, \url{http://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the OceanColor visual browser or file search facility)
#' map to \url{http://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). RAADSync will store the local copy of this file as oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
#' The \code{oceandata_platform_map} function maps the URL platform component ("V" in this example) to the corresponding directory name ("VIIRS")
#' @param abbrev string: the platform abbreviation from the URL (e.g. "Q" for Aquarius, "M" for MODIS-Aqua)
#' @references \url{http://oceandata.sci.gsfc.nasa.gov/}
#' @return Either the platform name string corresponding to the abbreviation, if \code{abbrev} supplied, or a named list of all abbreviations and platform name strings if \code{abbrev} is missing
#' @seealso \code{\link{oceandata_timeperiod_map}} \code{\link{oceandata_parameter_map}}
#' @export
oceandata_platform_map=function(abbrev) {
    allp=list(Q="Aquarius",C="CZCS",H="HICO",M="MERIS",A="MODISA",T="MODIST",O="OCTS",S="SeaWiFS",V="VIIRS")
    if (missing(abbrev)) {
        allp
    } else {
        allp[abbrev]
    }
}

#' Time periods and abbreviations used in Oceancolor URLs and file names
#' Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
#' For example, \url{http://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the OceanColor visual browser or file search facility)
#' map to \url{http://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). RAADSync will store the local copy of this file as oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
#' The \code{oceandata_timeperiod_map} function maps the URL time period component ("DAY" in this example) to the corresponding directory name ("Daily")
#' @references \url{http://oceandata.sci.gsfc.nasa.gov/}
#' @param abbrev string: the time period abbreviation from the URL (e.g. "DAY" for daily, "SCSP" for seasonal spring climatology)
#' @return Either the time period string corresponding to the abbreviation, if \code{abbrev} supplied, or a named list of all abbreviations and time period strings if \code{abbrev} is missing
#' @seealso \code{\link{oceandata_platform_map}} \code{\link{oceandata_parameter_map}}
#' @export
oceandata_timeperiod_map=function(abbrev) {
    alltp=list(WC="8D_Climatology","8D"="8Day",YR="Annual",CU="Cumulative",DAY="Daily",MO="Monthly",MC="Monthly_Climatology",R32="Rolling_32_Day",SNSP="Seasonal",SNSU="Seasonal",SNAU="Seasonal",SNWI="Seasonal",SCSP="Seasonal_Climatology",SCSU="Seasonal_Climatology",SCAU="Seasonal_Climatology",SCWI="Seasonal_Climatology")
    if (missing(abbrev)) {
        alltp
    } else {
        alltp[abbrev]
    }
}


#' Parameter names used in Oceancolor URLs and file names
#' Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
#' For example, \url{http://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the OceanColor visual browser or file search facility)
#' map to \url{http://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). RAADSync will store the local copy of this file as oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
#' The \code{oceandata_parameter_map} function maps the URL parameter component ("NPP_PAR_par" in this example) to the corresponding directory name ("par").
#' Note: the list of parameters is not yet complete.
#' @references \url{http://oceandata.sci.gsfc.nasa.gov/}
#' @param urlparm string: the parameter component of the URL (e.g. "KD490_Kd_490" for MODIS diffuse attenuation coefficient at 490 nm)
#' @param platform string: the platform abbreviation (currently one of "Q" (Aquarius), "C" (CZCS), "H" (HICO), "M" (MERIS), "A" (MODISA), "T" (MODIST), "O" (OCTS), "S" (SeaWiFS), "V" (VIIRS)
#' @return Either the directory string corresponding to the URL code, if \code{abbrev} supplied, or a named list of all URL regexps and corresponding directory name strings if \code{urlparm} is missing
#' @export
oceandata_parameter_map=function(platform,urlparm) {
    if (missing(platform)) stop("platform must be specified")
    ## platforms yet to do: "Q","H" (are different folder structure to the others)
    parm_map=switch(platform,
                    C=,
                    O=,
                    S=,
                    T=,
                    A=list("KD490_Kd_490"="Kd", ## MODIS, SeaWiFS
                           "NSST"="NSST",
                           "RRS_Rrs_[[:digit:]]+"="Rrs",
                           "SST"="SST",
                           "SST4"="SST4",
                           "IOP_a_.*"="a",
                           "IOP_adg_.*"="adg",
                           "RRS_angstrom"="angstrom",
                           "RRS_aot_[[:digit:]]+"="aot",
                           "IOP_aph_.*"="aph",
                           "IOP_bb_.*"="bb",
                           "IOP_bbp_.*"="bbp",
                           "CDOM_cdom_index"="cdom",
                           "CHL_chl_ocx"="chl",
                           "CHL_chlor_a"="chlor",
                           "FLH_ipar"="ipar",
                           "FLH_nflh"="nflh",
                           "PAR_par"="par",
                           "PIC_pic"="pic",
                           "POC_poc"="poc",
                           "LAND_NDVI","NDVI"),
                    V=list("NPP_KD490_Kd_490"="KD490", ## VIIRS
                           "NPP_CHL_chl_ocx","CHL",
                           ##??CHLOCI
                           ##?? GSM
                           "NPP_IOP_.*"="IOP",
                           "NPP_PAR_par"="par",
                           "NPP_PIC_pic"="pic",
                           "NPP_POC_poc"="poc",
                           ##?? QAA
                           "NPP_RRS_.*"="RRS"
                           ##?? ZLEE
                           )
                   ,stop("platform ",platform," not recognised"))
    this_parm_folder<-sapply(names(parm_map),function(z){ grepl(paste0("^",z,"$"),urlparm) })
    unlist(parm_map[this_parm_folder])
}

oceandata_url_mapper=function(this_url,path_only=FALSE,sep=.Platform$file.sep) {
    ## take getfile URL and return (relative) path to put the file into
    ## this_url should look like: http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km.bz2
    ## Mapped files (L3m) should become oceandata.sci.gsfc.nasa.gov/platform/Mapped/timeperiod/spatial/parm/[yyyy/]basename
    ## [yyyy] only for 8Day,Daily,Rolling_32_Day
    ## Binned files (L3b) should become oceandata.sci.gsfc.nasa.gov/platform/L3BIN/yyyy/ddd/basename
    assert_that(is.string(this_url))
    assert_that(is.flag(path_only))
    assert_that(is.string(sep))
    ## map various things from URL to path
    ## this needs to be a table, read from a config file
    #parm=c("Kd","NSST","Rrs","SST","SST4","angstrom","aot","cdom","chlor","ipar","nflh","par","pic","poc", "par")
    #parm_str=c("KD490_Kd_490","NSST","RRS_Rrs_[[:digit:]]+","SST","SST4","RRS_angstrom","RRS_aot_869","CDOM_cdom_index","CHL_chlor_a","FLH_ipar","FLH_nflh","PAR_par","PIC_pic","POC_poc",  ## these from MODISA: probably need to add other parms from other platforms?
    #           "NPP_PAR_par") ## VIIRS
    if (grepl("\\.L3m_",this_url)) {
        ## mapped file
        url_parts=str_match(this_url,"/([ASTCV])([[:digit:]]+)\\.(L3m)_([[:upper:][:digit:]]+)_(.*?)_(9|4)(km)?\\.(bz2|nc)")
        ## e.g. [1,] "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km"
        ## [,2] [,3]      [,4]  [,5]  [,6]          [,7]
        ## "A"  "2002359" "L3m" "DAY" "CHL_chlor_a" "9"
        url_parts=as.data.frame(url_parts,stringsAsFactors=FALSE)
        colnames(url_parts)=c("full_url","platform","date","type","timeperiod","parm","spatial","spatial_unit")
    } else if (grepl("\\.L3b_",this_url)) {
        url_parts=str_match(this_url,"/([ASTC])([[:digit:]]+)\\.(L3b)_([[:upper:][:digit:]]+)_(.*?)\\.(bz2|nc)")
        ## http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2
        ## e.g. [1,] "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2" "A"  "20090322009059" "L3b" "MO" "KD490"
        ## http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2015016.L3b_DAY_RRS.nc
        url_parts=as.data.frame(url_parts,stringsAsFactors=FALSE)
        colnames(url_parts)=c("full_url","platform","date","type","timeperiod","parm")
    } else {
        stop("not a L3 binned or mapped file")
    }
    this_year=substr(url_parts$date,1,4)
    if (is.na(url_parts$type)) {
        warning("unrecognized URL pattern",this_url,", ignoring")
        out=NULL
    } else {
        switch(url_parts$type,
               L3m={
                   this_parm_folder=oceandata_parameter_map(url_parts$platform,url_parts$parm)
                   out<-paste("oceandata.sci.gsfc.nasa.gov",oceandata_platform_map(url_parts$platform),"Mapped",oceandata_timeperiod_map(url_parts$timeperiod),paste0(url_parts$spatial,"km"),this_parm_folder,sep=sep)
                   if (url_parts$timeperiod %in% c("8D","DAY","R32")) {
                       out<-paste(out,this_year,sep=sep)
                   }
                   if (!path_only) {
                       out<-paste(out,basename(this_url),sep=sep)
                   } else {
                       out<-paste0(out,sep) ## trailing path separator
                   }
                 },
               L3b={ this_doy<-substr(url_parts$date,5,7)
                     out<-paste("oceandata.sci.gsfc.nasa.gov",oceandata_platform_map(url_parts$platform),"L3BIN",this_year,this_doy,sep=sep)
                     if (!path_only) {
                         out<-paste(out,basename(this_url),sep=sep)
                     } else {
                         out<-paste0(out,sep) ## trailing path separator
                     }
                 },
               stop("unrecognized file type: ",url_parts$type,"\n",str(url_parts))
               )
    }
    out
}

oceandata_url_mapper_test=function() {
    do_test=function(x,out) {
        if (! oceandata_url_mapper(x,path_only=TRUE)==out) {
            stop("oceandata_url_mapper(\"",x,"\") gives:\n",oceandata_url_mapper(x,path_only=TRUE),"\nwhereas I expected:\n",out)
        }
    }
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021852007192.L3m_WC_SST_9.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8D_Climatology/9km/SST/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030012003008.L3m_8D_KD490_Kd_490_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8Day/9km/Kd/2003/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090012009365.L3m_YR_FLH_ipar_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Annual/9km/ipar/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021852010120.L3m_CU_CDOM_cdom_index_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Cumulative/9km/cdom/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2007005.L3m_DAY_FLH_nflh_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Daily/9km/nflh/2007/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20023052002334.L3m_MO_FLH_nflh_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Monthly/9km/nflh/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021822006212.L3m_MC_SST4_9.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Monthly_Climatology/9km/SST4/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20110012011032.L3m_R32_RRS_aot_869_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Rolling_32_Day/9km/aot/2011/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20040812004172.L3m_SNSP_PIC_pic_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Seasonal/9km/pic/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030802011171.L3m_SCSP_RRS_angstrom_9km.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Seasonal_Climatology/9km/angstrom/")

    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_RRS.main.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_RRS.x06.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20140012014008.L3b_8D_PAR.main.bz2","oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2014/001/")
    do_test("http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc","oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/")
    do_test("http://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc","oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/")
}


