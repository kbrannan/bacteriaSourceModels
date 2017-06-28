#'  model bacteria generation from beavers
#'
#' This function is the bacteria source-model Beaver. The model
#' generates input for HSPF. The specific outputs from this source model
#' are loads from the beaver to land and directly to the stream
#' The load to the land is in the form of load/acre for forest
#' PLS in a sub-watershed that the source-model contributes to and
#' the hourly load to the stream in the form of a MUTSIN file. The
#' input for the model is from an ASCII text file. Use the text below
#' as a template for the input file. The symbol used for comments in
#' the input file is "***". The definitions for the symbols used in
#' the template are: YYYY is four-digit year, MM two-digit month,
#' DD is the two-digit day, ## is an integer, #.# is a floating point
#' number, and #.#E+## is a number in scientific notation
#' @param chr.input.file is the input file for the model
#' @export

wildlifeBeaver <- function(chr.input.file) {

  df.input <- read.delim(chr.input.file, sep=":",
                          comment.char="*", stringsAsFactors=FALSE,
                          header=FALSE)
  names(df.input) <- c("parameter","value(s)")

##
## set values for variables

## land use information
  ### Habitat (only forest)
  lu.habitatarea   <- as.numeric(df.input$value[
    df.input$parameter == "Forest Area in Watershed (ac)"])
## animal information
  ## population densities
  amn.density  <- as.numeric(df.input$value[
    df.input$parameter == "Animal Densities in buffer around streams/rivers (animal/ac)"])
  ### percent of time defecating in or around streams
  amn.percentstream <- as.numeric(df.input$value[
    df.input$parameter == "Percent of time defecating in streams"])/100
  ### bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "bacteria Production of adult per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])

  ##
  ### Calculations
  ### Populations
  pop.total   <- lu.habitatarea * amn.density
  pop.on.land     <- (1-amn.percentstream) * pop.total
  pop.in.stream <- amn.percentstream * pop.total
  ### bacteria loads
  bac.total <- pop.total * amn.bac.prod
  bac.on.land  <- amn.bac.prod * pop.on.land
  bac.in.stream   <- amn.bac.prod * pop.in.stream
  ### accum values
  accum.forest  <- 0
  if(lu.habitatarea > 0) {
    accum.forest  <- bac.on.land / lu.habitatarea
  }

  ##
  ## Assemble output data frame
  df.output <- data.frame(
    Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
    pop.total=pop.total,
    pop.on.land=pop.on.land,
    pop.in.stream=pop.in.stream,
    Bacteria.total=bac.total,
    Bacteria.on.land=bac.on.land,
    Bacteria.direct.to.stream=bac.in.stream,
    Accum.forest=accum.forest,
    Lim.forest=amn.SQLIM.factor * accum.forest,
    stringsAsFactors=FALSE)

  return(df.output)
}
