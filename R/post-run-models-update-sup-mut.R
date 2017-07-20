#' runs models and updates HSPF-sup file (with accum and sqolim info) and MUSTIN loads
#' returns a list with the updated sup-file as a character vector with each row containing a line of the files and a data.frame for the MUTSIN loads
#'
#' @param chr.dir.sub.models path to location of sub-model input files
#' @param chr.dir.hspf path to loaction of hspf input files and where hspf is run
#' @param chr.file.sup name of hspf-sup file
#' @param chr.file.uci name of hspf-uci input file
#' @param df.models data frame constaining name of sub-mode function and the corresponding path to the model input files
#'## Not Run:
#'Example Input
#'chr.dir.sub.models <- "M:/Models/Bacteria/HSPF/bacteria-sub-model-testing/SourceControl/sub-models"
#'chr.dir.hspf <- "M:/Models/Bacteria/HSPF/bacteria-sub-model-testing/SourceControl/hspf"
#'chr.file.sup <- "bigelkwq.sup"
#'chr.file.uci <- "bigelkwq.uci"
#'
#'chr.sub.models <- as.character(lsf.str("package:bacteriaSourceModels"))
#'chr.sub.models <- grep("post", chr.sub.models, invert = TRUE, value = TRUE)
#'
#'chr.dirs.sub.models <- list.dirs(path = chr.dir.sub.models, full.names = FALSE)
#'chr.dirs.sub.models <- grep("General", chr.dirs.sub.models[
#'  nchar(chr.dirs.sub.models) > 0], invert = TRUE, value = TRUE)
#'
#'df.models <- data.frame(model = chr.sub.models, dir =
#'                          chr.dirs.sub.models, stringsAsFactors = FALSE)
#'## End (No run)
#' @export
post.run.models.update.sup.mut <- function(chr.dir.sub.models, df.models, chr.dir.hspf, chr.file.sup, chr.file.uci) {
  df.out <- data.frame()
  for(kk in 1:length(df.models$model)) {
    df.cur <- post.run.model.source.subs(df.models$model[kk]
                                         ,paste0(chr.dir.sub.models,"/",
                                                 df.models$dir[kk]))
    df.out <- rbind(df.out,df.cur)
    rm(df.cur)
  }
  df.accum <- post.get.HSPF.input("accum\\.", df.out, "PLS")
  df.sqolim <- post.get.HSPF.input("lim\\.", df.out, "PLS")
  df.mut <- post.get.HSPF.input("bacteria\\.direct\\.to\\.stream", df.out, "MUTSIN")
  df.pls.line.info <- post.get.pls.line.info(paste0(chr.dir.hspf, "/", chr.file.uci))
  df.accum.sup <- post.merge.pls.info.to.sup.table(df.pls.line.info, df.accum, "sup.num.accum")
  df.sqolim.sup <- post.merge.pls.info.to.sup.table(df.pls.line.info, df.sqolim, "sup.num.sqolim")
  ## read uci file
  chr.hspf.sup <- scan(file = paste0(chr.dir.hspf, "/", chr.file.sup),
                       what = "character", sep = "\n",
                       quiet = TRUE)
  chr.sup.update <- post.update.HSPF.sup.file(df.pls.line.info, chr.hspf.sup, df.accum.sup, "sup.num.accum")
  chr.sup.update <- post.update.HSPF.sup.file(df.pls.line.info, chr.sup.update, df.sqolim.sup, "sup.num.sqolim")
  lst.sup.mut <- list(chr.sup.update, df.mut)
  return(lst.sup.mut)
}
