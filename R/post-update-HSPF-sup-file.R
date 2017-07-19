#' updates the sup file (in a character vector) with the loads for each pls given the line info in the sup file
#'
#' @param df.pls.line.info pls line info that correposnds to the HSPF-sup-file info from HSPF-uci-file
#' @param chr.hspf.sup character vector containing the HSPF-sup-file with each each corresponding to a line of the file
#' @param df.sup.table the accum or sqolim loads from the sub-models for the update of sup file
#' @param chr.sup.col.num.name the column name in df.sup.table that has the sup-file line numbers for the loads
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
