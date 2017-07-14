#' run bacteria source model for a specific input file
#' Note: this function does not check for the existance of the input file nor
#' if the input file is the correct format for the source model called. This is
#' left to the user to ensure.
#'
#' @param chr.file.input is the input file for the model
#' @param chr.model is the name of the bacteria source model to use, which is a part of the bacteriaSourceModels package
#' @export

post.run.model.source <- function(chr.file.input, chr.model) {
  df.cur <- data.frame()
  eval(parse(text = paste0("df.cur <- bacteriaSourceModels::", chr.model, "('", chr.file.input, "')")))
  return(df.cur)
}
