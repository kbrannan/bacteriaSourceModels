#' write updated mutsin files
#'
#' @param chr.dir.hspf path to loaction of hspf input files and where hspf is run
#' @param chr.file.uci name of hspf-uci input file
#' @param chr.name.wtsd name of the watershed
#' @param df.mut data.frame containing mutsin data in the list-class output from function "post.run.models.update.sup.mut".
#' @export

post.write.mutsin.files <- function(chr.dir.hspf, chr.file.uci, chr.name.wtsd, df.mut) {
  df.sim.dates <- post.get.sim.dates(chr.dir.hspf, chr.file.uci)
  df.files.mut <- post.get.mutsin.file.names(chr.dir.hspf, chr.file.uci)
  df.seq.date.month <-data.frame(
    date = vec.sim.period <- seq(df.sim.dates$start, df.sim.dates$end,
                                 by = "month"),
    Month = strftime(vec.sim.period, format = "%b"))

  for(ii in 1:length(df.files.mut$file)) {
    chr.cur.sub.wtsd <- df.files.mut[ii, ]
    df.cur.mut <- merge(df.seq.date.month,
                        df.mut[grep(chr.cur.sub.wtsd$sub, df.mut$sub), c("Month", "load")],
                        by = c("Month"))
    df.cur.mut <- df.cur.mut[order(df.cur.mut$date), c("date", "load")]
    chr.out.mut <- c("**** Direct Deposit Fecal Coliform Load",
                     paste0("**** Watershed: ", chr.name.wtsd),
                     paste0("**** Sub-watershed: ", chr.cur.sub.wtsd$sub),
                     "      Year Mo Da Hr Mi   FC",
                     paste0(strftime(df.cur.mut$date,
                                     format = "      %Y %m %d 24 00   "),
                            sprintf(fmt = "%.6E", df.cur.mut$load), sep = "")
    )
    con.mut.cur <-  file(paste0(chr.dir.hspf, "/", df.files.mut$file[ii]), "w")
    writeLines(chr.out.mut, con.mut.cur)
    close(con.mut.cur)
    rm(df.cur.mut, chr.out.mut, con.mut.cur)
  }
}

