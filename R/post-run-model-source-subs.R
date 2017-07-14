#' run bacteria source model for a specific input file
#'
#' This function is the bacteria source-model for all the sub watersheds
#' Note: this function does not check for the existance of the path to the input files nor
#' if the input files are the correct format for the source model called. This is
#' left to the user to ensure.
#'
#' @param chr.model is the name of the bacteria source model to use, which is a part of the bacteriaSourceModels
#' @param chr.dir.sub.model is the path to the sub model input files for the sub wathersheds
#' @importFrom reshape2 melt
#' @export

post.run.model.source.subs <- function(chr.model, chr.dir.sub.model) {
  chr.files.input <- list.files(path = chr.dir.sub.model, pattern = "*\\.txt",
                                full.names = TRUE)
  df.cur <- do.call(rbind,lapply(chr.files.input, post.run.model.source, chr.model))
  df.r.cur <- reshape2::melt(df.cur, id = c("sub", "Month"))
  df.r.cur$variable <- as.character(df.r.cur$variable)
  df.out <- data.frame(df.r.cur[, c(1,2)], source = chr.model, df.r.cur[, c(-1,-2)],
                       stringsAsFactors = FALSE)
  return(df.out)
}
