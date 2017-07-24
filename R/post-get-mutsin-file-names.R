#' gets the names of the mutsin files from the HSPF uci file
#' returns data.frame with the file names (without path if included) and sub watershed number
#'
#' @param chr.dir.hspf path to loaction of hspf input files and where hspf is run
#' @param chr.file.uci name of hspf-uci input file
#' @export

post.get.mutsin.file.names <- function(chr.dir.hspf, chr.file.uci) {
  options(stringsAsFactors = FALSE)
  chr.hspf.uci <- scan(file = paste0(chr.dir.hspf, "/", chr.file.uci), sep = "\n",
                     what = "character", quiet = TRUE)
  chr.files.block <- chr.hspf.uci[grep("^FILES", chr.hspf.uci):grep("^END FILES", chr.hspf.uci)]
  chr.file.names.mut <- stringr::str_extract(grep(".*\\.mut$", chr.files.block, value = TRUE), "[aA-zZ0-9]{1,}\\.mut")
  df.files.mut <- data.frame(sub = stringr::str_extract(chr.file.names.mut, "[0-9]{1,}"),
                             file = chr.file.names.mut)

  return(df.files.mut)
}
