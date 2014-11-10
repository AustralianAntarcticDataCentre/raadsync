oceandata=function(dataset) {
    ## oceandata synchronisation handler

    ## oceandata provides a file search interface, e.g.:
    ## wget -q --post-data="dtype=L3m&addurl=1&results_as_file=1&search=A2002*DAY_CHL_chlor*9km*" -O - http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi
    ## or
    ## wget -q --post-data="dtype=L3b&addurl=1&results_as_file=1&search=A2014*DAY_CHL.*" -O - http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi
    ## this returns list of file URLs like http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km.bz2
    ## then do individual wget with renaming of output path

    ## expect that dataset$method_flags will contain the search and dtype components of the post string
    ##  i.e. "search=...&dtype=..." in "dtype=L3m&addurl=1&results_as_file=1&search=A2002*DAY_CHL_chlor*9km*"
    ##  or include the data type in the search e.g. "search=A2002*L3m_DAY_CHL_chlor*9km*
    assert_that(is.string(dataset$method_flags))
    myfiles=system(paste0("wget -q --post-data=\"addurl=1&results_as_file=1&",dataset$method_flags,"\" -O - http://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi"),intern=TRUE)
    myfiles=myfiles[grepl("^http://",myfiles)] ## get rid of header and blank lines
    ## for each file, download and store in appropriate directory
    for (this_url in myfiles) {
        dummy=dataset
        switch(as.character(dataset$clobber),
               "0"={dummy$method_flags<-"--no-clobber"},
               "1"={dummy$method_flags<-"--timestamping"},
               dummy$method_flags<-"" ## otherwise no flag needed, specifying -O below will cause overwrite of existing file
               )
        ##dummy$method_flags=paste(dummy$method_flags,"-O",oceandata_url_mapper(this_url),sep=" ")
        dummy$method_flags=paste(dummy$method_flags,"--recursive","--directory-prefix",oceandata_url_mapper(this_url,path_only=TRUE),"--cut-dirs=2","--no-host-directories",sep=" ")
        dummy$source_url=this_url
        wget_call=build_wget_call(dummy)
        do_wget(wget_call,dataset)
    }

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
    platform_map=list(Q="Aquarius",C="CZCS",H="HICO",M="MERIS",A="MODISA",T="MODIST",O="OCTS",S="SeaWiFS",V="VIIRS")
    timeperiod_map=list(WC="8D_Climatology","8D"="8Day",YR="Annual",CU="Cumulative",DAY="Daily",MO="Monthly",MC="Monthly_Climatology",R32="Rolling_32_Day",SNSP="Seasonal",SNSU="Seasonal",SNAU="Seasonal",SNWI="Seasonal",SCSP="Seasonal_Climatology",SCSU="Seasonal_Climatology",SCAU="Seasonal_Climatology",SCWI="Seasonal_Climatology")

    parm=c("Kd","NSST","Rrs","SST","SST4","angstrom","aot","cdom","chlor","ipar","nflh","par","pic","poc")
    parm_str=c("KD490_Kd_490","NSST","RRS_Rrs_[[:digit:]]+","SST","SST4","RRS_angstrom","RRS_aot_869","CDOM_cdom_index","CHL_chlor_a","FLH_ipar","FLH_nflh","PAR_par","PIC_pic","POC_poc") ## these from MODISA: probably need to add other parms from other platforms?

    if (grepl("\\.L3m_",this_url)) {
        ## mapped file
        url_parts=str_match(this_url,".*getfile/([ASTC])([[:digit:]]+)\\.(L3m)_([[:upper:][:digit:]]+)_(.*?)_(9|4)(km)?\\.bz2")
        ## e.g. [1,] "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km"
        ## [,2] [,3]      [,4]  [,5]  [,6]          [,7]
        ## "A"  "2002359" "L3m" "DAY" "CHL_chlor_a" "9"
        url_parts=as.data.frame(url_parts,stringsAsFactors=FALSE)
        colnames(url_parts)=c("full_url","platform","date","type","timeperiod","parm","spatial","spatial_unit")
    } else if (grepl("\\.L3b_",this_url)) {
        url_parts=str_match(this_url,".*getfile/([ASTC])([[:digit:]]+)\\.(L3b)_([[:upper:][:digit:]]+)_(.*?)\\.bz2")
        ## http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2
        ## e.g. [1,] "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2" "A"  "20090322009059" "L3b" "MO" "KD490"
        url_parts=as.data.frame(url_parts,stringsAsFactors=FALSE)
        colnames(url_parts)=c("full_url","platform","date","type","timeperiod","parm")
    } else {
        stop("not a L3 binned or mapped file")
    }
    this_year=substr(url_parts$date,1,4)
    switch(url_parts$type,
           L3m={ this_parm_folder<-sapply(parm_str,function(z){ grepl(paste0("^",z,"$"),url_parts$parm) })
                 this_parm_folder<-parm[which(this_parm_folder)]
                 out<-paste("oceandata.sci.gsfc.nasa.gov",platform_map[[url_parts$platform]],"Mapped",timeperiod_map[[url_parts$timeperiod]],paste0(url_parts$spatial,"km"),this_parm_folder,sep=sep)
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
                 out<-paste("oceandata.sci.gsfc.nasa.gov",platform_map[[url_parts$platform]],"L3BIN",this_year,this_doy,sep=sep)
                 if (!path_only) {
                     out<-paste(out,basename(this_url),sep=sep)
                 } else {
                     out<-paste0(out,sep) ## trailing path separator
                 }
             },
           stop("unrecognized file type: ",url_parts$type,"\n",str(url_parts))
           )
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
}


