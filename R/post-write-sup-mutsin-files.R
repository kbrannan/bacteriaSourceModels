## chr.dir.hspf, chr.file.sup, chr.file.uci, chr.name.wtsd

chr.name.wtsd <- "Big Elk Creek"

df.sim.dates <- post.get.sim.dates(chr.dir.hspf, chr.file.uci)
df.files.mut <- post.get.mutsin.file.names(chr.dir.hspf, chr.file.uci)
df.seq.date.month <-data.frame(
  date = vec.sim.period <- seq(df.sim.dates$start, df.sim.dates$end,
                               by = "month"),
  Month = strftime(vec.sim.period, format = "%b"))

df.mut <- lst.updates[[2]]

ii <- 1

chr.cur.sub.wtsd <- df.files.mut[ii, ]

df.cur.mut <- merge(df.seq.date.month,
                    df.mut[grep(chr.cur.sub.wtsd$sub, df.mut$sub), c("Month", "load")],
                    by = c("Month"))
df.cur.mut <- df.cur.mut[order(df.cur.mut$date), c("date", "load")]

chr.out.mut <- c("**** Direct Deposit Fecal Coliform Load",
                 paste0("**** Watershed: ", chr.name.wtsd),
                 paste0("**** Sub-watershed: ", chr.cur.sub.wtsd$sub),
                 "      Year Mo Da Hr Mi   FC",
                 paste0(strftime(head(df.cur.mut$date, 10),
                                 format = "      %Y %m %d 24 00   "),
                        sprintf(fmt = "%.6E", head(df.cur.mut$load, 10)), sep = "")
)
