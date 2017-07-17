#' get reads in HSPF-sup-file and returns a long-format data.frame with accum and sqolim info
#'
#' @param chr.file.sup is the file name for the HSPF-sup-file (includes path if necessary)
#' @export

post.update.HSPF.sup.file <- function(row.pls.line.info, chr.hspf.sup, df.sup.table, chr.sup.col.num.name) {

  chr.hspf.sup.update <- chr.hspf.sup
  row.cur <- grep(paste0(df.sup.table[row.pls.line.info, chr.sup.col.num.name], "( ){1,}12$"), chr.hspf.sup.update) + 1
  chr.hspf.sup[row.cur] <- paste0("  ",
                                  paste0(sprintf(fmt = "%.8E",
                                                 df.accum.sup.out[row.pls.line.info, 4:15]), collapse = " "))
  return(chr.hspf.sup)
}
