#' write updated hspf-sup file
#'
#' @param chr.dir.hspf path to loaction of hspf input files and where hspf is run
#' @param chr.file.sup name of hspf-sup file to update (WARNING: file is overwrtten)
#' @param chr.sup character vector containing lines of the sup file in the list-class output from function "post.run.models.update.sup.mut".
#' @export

post.write.sup.file <- function(chr.dir.hspf, chr.file.sup, chr.sup) {
    con.sup <-  file(paste0(chr.dir.hspf, "/", chr.file.sup), "w")
    writeLines(chr.sup, con.sup)
    close(con.sup)
}

