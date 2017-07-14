#' get pls line info that correposnds to the HSPF-sup-file info from HSPF-uci-file
#'
#' @param chr.file.uci is the file name for the HSPF-uci-file (includes path if necessary)
#' @export
#'
post.get.pls.line.info <- function(chr.file.uci) {

  ## read uci file
  chr.input.hspf.uci <- scan(file = chr.file.uci,
                           what = "character", sep = "\n",
                           quiet = TRUE)

  ## get beginning and end of PERLND block
  int.str <- min(grep("^PERLND", chr.input.hspf.uci))
  int.end <- grep("^END PERLND", chr.input.hspf.uci)

  ## get pls info from GEN-INFO block
  chr.gen.info <-
    chr.input.hspf.uci[
      (int.str +
         min(grep("GEN-INFO",
                  chr.input.hspf.uci[int.str:int.end])) -1):
        (int.str +
           grep("END GEN-INFO", chr.input.hspf.uci[int.str:int.end]) - 1)]
  int.drop <- -1 * sort(c(grep("GEN-INFO", chr.gen.info),
                     grep("\\*\\*", chr.gen.info)))
  chr.gen.info <- chr.gen.info[int.drop]
  df.gen.info <- data.frame(do.call(rbind,
                                    strsplit(chr.gen.info,
                                             "( ){1,}"))[, c(2,4,5)])
  names(df.gen.info) <- c("pls.num", "sub", "pls.name.lng")
  df.gen.info <- cbind(df.gen.info,
                       pls.name =
                         do.call(rbind,
                                 strsplit(as.character(df.gen.info[, 3]),
                                          "/"))[,1])
  rm(int.drop, chr.gen.info)

## function gets sup number given pls numer and mon-accum/sqolim data frame
  get.sup.from.pls <- function(num.pls, df.mon) {
    num.pls <- as.numeric(num.pls)
    df.mon$sup.line <- as.character(df.mon$sup.line)
    no.na <- as.numeric(row.names(df.mon[is.na(df.mon$end) == FALSE, ]))
    df.mon.no.nas <- df.mon[no.na, ]

    chr.sup.line <- character(0)

    if(sum(df.mon$str == num.pls) > 0) {
      chr.sup.line <- df.mon$sup.line[df.mon$str == num.pls]
    }
    if(sum(df.mon.no.nas$end == num.pls) > 0) {
      chr.sup.line <- df.mon.no.nas$sup.line[df.mon.no.nas$end == num.pls]
    }
    if(length(chr.sup.line) == 0) {
      chr.sup.line <- df.mon.no.nas$sup.line[((df.mon.no.nas$str < num.pls) &
                               (df.mon.no.nas$end > num.pls))]
    }
    return(chr.sup.line)
  }
## get line numbers for MON-ACCUM
## get MON-ACCUM block
  chr.mon.accum <-
    chr.input.hspf.uci[
      (int.str +
         min(grep("MON-ACCUM",
                  chr.input.hspf.uci[int.str:int.end])) -1):
        (int.str +
           grep("END MON-ACCUM", chr.input.hspf.uci[int.str:int.end]) - 1)]

  int.drop <- -1 * c(grep("MON-ACCUM", chr.mon.accum),
                     grep("\\*\\*", chr.mon.accum))
  chr.mon.accum <- chr.mon.accum[int.drop]
## create data.frame for MON-ACCUM block with pls nums and sup nums
  df.mon.accum <- data.frame(
    str = as.numeric(gsub(" ", "", substr(chr.mon.accum, 3, 5))),
    end = as.numeric(gsub(" ", "", substr(chr.mon.accum, 8, 10))),
    sup.line = gsub(" ", "", (gsub("~","",
                                   substr(chr.mon.accum, 71, 80)))))
  rm(chr.mon.accum, int.drop)
## get MON-ACCUM sup nums for the pls nums
  df.pls.sup.nums.mon.accum <-
    data.frame(pls.num = df.gen.info$pls.num,
               sup.num = do.call(rbind,
                                 lapply(as.character(df.gen.info$pls.num),
                                        get.sup.from.pls, df.mon.accum)),
               stringsAsFactors = FALSE)
  names(df.pls.sup.nums.mon.accum) <- c("pls.num", "sup.num.accum")

## get line numbers for MON-SQOLIM
  chr.mon.sqolim <-
    chr.input.hspf.uci[
      (int.str +
         min(grep("MON-SQOLIM",
                  chr.input.hspf.uci[int.str:int.end])) -1):
        (int.str +
           grep("END MON-SQOLIM", chr.input.hspf.uci[int.str:int.end]) - 1)]

  int.drop <- -1 * c(grep("MON-SQOLIM", chr.mon.sqolim),
                     grep("\\*\\*", chr.mon.sqolim))
  chr.mon.sqolim <- chr.mon.sqolim[int.drop]
  df.mon.sqolim <- data.frame(
    str = as.numeric(gsub(" ", "", substr(chr.mon.sqolim, 3, 5))),
    end = as.numeric(gsub(" ", "", substr(chr.mon.sqolim, 8, 10))),
    sup.line = gsub(" ", "", (gsub("~","",
                                   substr(chr.mon.sqolim, 71, 80)))))
  df.pls.sup.nums.mon.sqolim <-
    data.frame(pls.num = df.gen.info$pls.num,
               sup.num = do.call(rbind,
                                 lapply(as.character(df.gen.info$pls.num),
                                        get.sup.from.pls, df.mon.sqolim)),
               stringsAsFactors = FALSE)


  rm(chr.mon.sqolim, int.drop)
  names(df.pls.sup.nums.mon.sqolim) <- c("pls.num", "sup.num.sqolim")

  ## merge accum and sqlim data frames and return the result
  ##options(stringsAsFactors = TRUE)
  df.pls.sup.num <- merge.data.frame(df.pls.sup.nums.mon.accum,
                   df.pls.sup.nums.mon.sqolim,
                   by = c("pls.num"), stringsAsFactors = FALSE)
  df.pls.sup.num <- merge(df.gen.info, df.pls.sup.num, by = "pls.num")
  return(df.pls.sup.num)
}
