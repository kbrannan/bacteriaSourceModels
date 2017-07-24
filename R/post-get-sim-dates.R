#' gets the simulation period from the HSPF uci file
#' returns data frame with start and end POSIXct dates of simulation period
#'
#' @param chr.dir.hspf path to loaction of hspf input files and where hspf is run
#' @param chr.file.uci name of hspf-uci input file
#' @export
post.get.sim.dates <- function(chr.dir.hspf, chr.file.uci) {
  chr.hspf.uci <- scan(file = paste0(chr.dir.hspf, "/", chr.file.uci), sep = "\n",
                     what = "character", quiet = TRUE)
  dte.sim.period <- as.POSIXct(do.call(rbind,
                   strsplit(gsub("/", "-", gsub(" ","", gsub("24:00", "", gsub("00:00", ",", gsub("([aA-zZ])", "", grep("start.*end.*$", chr.hspf.uci, ignore.case = TRUE, value = TRUE))))))
                            , split = ",")))
  df.sim.period <- data.frame(start = dte.sim.period[1], end = dte.sim.period[2])
  return(df.sim.period)
}
