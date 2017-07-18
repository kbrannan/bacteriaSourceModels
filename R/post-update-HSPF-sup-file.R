#' get reads in HSPF-sup-file and returns a long-format data.frame with accum and sqolim info
#'
#' @param chr.file.sup is the file name for the HSPF-sup-file (includes path if necessary)
#' @export

post.update.HSPF.sup.file <- function(df.pls.line.info, chr.hspf.sup, df.sup.table, chr.sup.col.num.name) {

  chr.hspf.sup.update <- chr.hspf.sup
  num.rows <- 1:length(df.pls.line.info[, 1])
  for(ii in num.rows) {
    row.cur <- grep(paste0(df.sup.table[ii, chr.sup.col.num.name], "( ){1,}12$"), chr.hspf.sup.update) + 1
    chr.hspf.sup.update[row.cur] <- paste0("  ", paste0(sprintf(fmt = "%.8E", df.sup.table[ii, 4:15]), collapse = " "))
  }
  return(chr.hspf.sup.update)
}
