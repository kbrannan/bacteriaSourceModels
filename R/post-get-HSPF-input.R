#' get output from source model runs for input to HSPF sup-file or MUTSIN files
#'
#' @param chr.srch is search term either "accum", "lim", or "mutsin"
#' @param df.out is data.frame from "run.model.source.subs" function
#' @param chr.col.name is the name of the column that has the HSPF  input either "accum", "lim", or "mutsin"
#' @importFrom doBy summaryBy
#' @export

post.get.HSPF.input <- function(chr.srch, df.out, chr.col.name) {
  rows.var <- grep(chr.srch, df.out$variable, ignore.case = TRUE)

  df.var <- df.out[rows.var, c("sub", "Month", "variable", "value")]
  df.var$sub <- factor(df.var$sub)
  df.var$Month <- factor(df.var$Month, levels = format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"))
  df.var$variable <- gsub(chr.srch, "", df.var$variable, ignore.case = TRUE)
  df.var$variable <- factor(df.var$variable)
  df.var.sum <- doBy::summaryBy(value ~ sub + Month + variable, data = df.var, FUN = sum)
  names(df.var.sum) <- c("sub", "Month", chr.col.name, "load")
  return(df.var.sum)
}
