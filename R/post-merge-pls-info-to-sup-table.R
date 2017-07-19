#' merges HSPF-sup-file and pls line info then returns a long-format data.frame with accum and sqolim info
#'
#' @param df.pls.line.info pls line info that correposnds to the HSPF-sup-file info from HSPF-uci-file
#' @param df.sup.table the accum or sqolim loads from the sub-models for the update of sup file
#' @param chr.sup.col.num.name the column name in df.sup.table that has the sup-file line numbers for the loads
#' @export

post.merge.pls.info.to.sup.table <- function(df.pls.line.info, df.df.sup.table, chr.sup.col.num.name) {

  df.pls.line.info.merge <- cbind(df.pls.line.info, merge.me = paste0(as.character(df.pls.line.info$sub),
                                                                      tolower(as.character(df.pls.line.info$pls.name))))

  df.table.merge <- cbind(df.df.sup.table,
                          merge.me = paste0(as.character(df.df.sup.table$sub),
                                            tolower(as.character(df.df.sup.table$PLS))))

  df.table.cmb <- merge(df.pls.line.info.merge, df.table.merge)
  rows.dup <- duplicated(df.table.cmb[, c("Month", "merge.me")])
  df.table.sup <- df.table.cmb[rows.dup == FALSE, c("sub", "pls.name", chr.sup.col.num.name, "Month", "load")]
  df.table.sup.out <- reshape(df.table.sup[, ], timevar = "Month", direct = "wide",
                              idvar = c("sub", "pls.name", chr.sup.col.num.name))
  return(df.table.sup.out)
}
