#' Synchronise a raadsync repo using BatchJobs
#'
#' Note that the \code{reg} object should be created with raadsync in the list of packages (see example).
#'
#' @param config data.frame: raadsync configuration as returned by \code{\link{read_repo_config}}
#' @param reg Registry: a BatchJobs registry object
#' @param create_root logical: should the data root directory be created if it does not exist?
#' @param verbose logical: if TRUE, provide additional progress output
#' @return list, with status and captured console output from each synchronisation task
#'
#' @examples
#' \dontrun{
#'  library(BatchJobs)
#'  setConfig(cluster.functions= makeClusterFunctionsMulticore(ncpus=7),debug=FALSE)
#'  reg <- makeRegistry(id="syncysync",file.dir="sync_files",packages="raadsync")
#'  cf <- read_repo_config("/my/local/repo/config")
#'  sync_log <- sync_batch(cf,reg)
#'  removeRegistry(reg,ask="no") ## clean up
#' }
#'
#' @export
sync_batch <- function(config,reg,create_root=FALSE,verbose=TRUE) {
    if (!requireNamespace("BatchJobs",quietly=TRUE))
        stop("Package BatchJobs must be installed to use sync_batch()")
    assert_that(is.data.frame(config))
    assert_that(inherits(reg,"Registry"))
    assert_that(is.flag(create_root))
    assert_that(is.flag(verbose))
    ## wrapper function for sync_repo
    sync_source <- function(rownum,cf) {out <- capture.output(status <- sync_repo(cf[rownum,])); list(status=status,output=out) }
    ## map the jobs to the register
    batchMap(reg,sync_source,1:nrow(config),more.args=list(cf=config))
    ## start jobs
    submitJobs(reg)
    waitForJobs(reg)
    results <- reduceResultsList(reg,fun=function(job,res)res)
    out <- list(status=sapply(results,function(z)z$status),output=lapply(results,function(z)paste(z,collapse="\n")))
    out
}
