#' model bacteria generation from geese
#'
#' This function is the bacteria source-model geese The model
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

wildlife.Geese <- function(chr.file.input) {

  ## read input files
  df.input <- read.delim(chr.file.input, sep=":",
                         comment.char="*", stringsAsFactors=FALSE,
                         header=FALSE)
  names(df.input) <- c("parameter","value")

  ##
  ## set values for variables

  ## get sub watershed number
  chr.sub <- gsub("([^0-9]){1, }", "" , df.input[df.input$parameter == "Watershed", "value"])

  ## land use information
  lu.pasture.area   <- as.numeric(df.input$value[
    df.input$parameter == "Pasture Area in Watershed (ac)"])
  lu.forest.area   <- as.numeric(df.input$value[
    df.input$parameter == "Forest Area in Watershed (ac)"])
  lu.RAOCUT.area   <- as.numeric(df.input$value[
    df.input$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"])  ### Animal Densities
  lu.habitatarea <- lu.pasture.area + lu.forest.area + lu.RAOCUT.area
  ## animal information
  ## months for seasons
  amn.months.season.1 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 1 Months (numbers)"], split = ",")[[1]])
  amn.months.season.2 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 2 Months (numbers)"], split = ",")[[1]])
  ## Animal Densities
  amn.animal.density.season.1  <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Animal Density (animal/ac)"])
  amn.animal.density.season.2  <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Animal Density (animal/ac)"])
  ## Percent animals around streams
  amn.percentstream <- as.numeric(df.input$value[
    df.input$parameter == "Percent of animals on Pasture in and around streams"])/100
  ## bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Bacteria Production of animal per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])

  ## Calculations
  ## populations
  ## overall locations
  pop.total.season.1 <-   lu.habitatarea * amn.animal.density.season.1
  pop.pasture.season.1 <- lu.pasture.area * amn.animal.density.season.1
  pop.forest.season.1 <-  lu.forest.area * amn.animal.density.season.1
  pop.RAOCUT.season.1 <-  lu.RAOCUT.area * amn.animal.density.season.1
  pop.total.season.2 <-   lu.habitatarea * amn.animal.density.season.2
  pop.pasture.season.2 <- lu.pasture.area * amn.animal.density.season.2
  pop.forest.season.2 <-  lu.forest.area * amn.animal.density.season.2
  pop.RAOCUT.season.2 <-  lu.RAOCUT.area * amn.animal.density.season.2
  ## on land
  pop.total.on.land.season.1 <-   (1 - amn.percentstream) * pop.total.season.1
  pop.pasture.on.land.season.1 <- (1 - amn.percentstream) * pop.pasture.season.1
  pop.forest.on.land.season.1 <-  (1 - amn.percentstream) * pop.forest.season.1
  pop.RAOCUT.on.land.season.1 <-  (1 - amn.percentstream) * pop.RAOCUT.season.1
  pop.total.on.land.season.2 <-   (1 - amn.percentstream) * pop.total.season.2
  pop.pasture.on.land.season.2 <- (1 - amn.percentstream) * pop.pasture.season.2
  pop.forest.on.land.season.2 <-  (1 - amn.percentstream) * pop.forest.season.2
  pop.RAOCUT.on.land.season.2 <-  (1 - amn.percentstream) * pop.RAOCUT.season.2
  ## in stream
  pop.total.in.stream.season.1 <-   amn.percentstream * pop.total.season.1
  pop.pasture.in.stream.season.1 <- amn.percentstream * pop.pasture.season.1
  pop.forest.in.stream.season.1 <-  amn.percentstream * pop.forest.season.1
  pop.RAOCUT.in.stream.season.1 <-  amn.percentstream * pop.RAOCUT.season.1
  pop.total.in.stream.season.2 <-   amn.percentstream * pop.total.season.2
  pop.pasture.in.stream.season.2 <- amn.percentstream * pop.pasture.season.2
  pop.forest.in.stream.season.2 <-  amn.percentstream * pop.forest.season.2
  pop.RAOCUT.in.stream.season.2 <-  amn.percentstream * pop.RAOCUT.season.2
  ## bacteria loads
  ## overall locations
  bac.total.season.1 <-   amn.bac.prod * pop.total.season.1
  bac.pasture.season.1 <- amn.bac.prod * pop.pasture.season.1
  bac.forest.season.1 <-  amn.bac.prod * pop.forest.season.1
  bac.RAOCUT.season.1 <-  amn.bac.prod * pop.RAOCUT.season.1
  bac.total.season.2 <-   amn.bac.prod * pop.total.season.2
  bac.pasture.season.2 <- amn.bac.prod * pop.pasture.season.2
  bac.forest.season.2 <-  amn.bac.prod * pop.forest.season.2
  bac.RAOCUT.season.2 <-  amn.bac.prod * pop.RAOCUT.season.2
  ## on land
  bac.total.on.land.season.1 <-   pop.total.on.land.season.1   * amn.bac.prod
  bac.pasture.on.land.season.1 <- pop.pasture.on.land.season.1 * amn.bac.prod
  bac.forest.on.land.season.1 <-  pop.forest.on.land.season.1  * amn.bac.prod
  bac.RAOCUT.on.land.season.1 <-  pop.RAOCUT.on.land.season.1  * amn.bac.prod
  bac.total.on.land.season.2 <-   pop.total.on.land.season.2   * amn.bac.prod
  bac.pasture.on.land.season.2 <- pop.pasture.on.land.season.2 * amn.bac.prod
  bac.forest.on.land.season.2 <-  pop.forest.on.land.season.2  * amn.bac.prod
  bac.RAOCUT.on.land.season.2 <-  pop.RAOCUT.on.land.season.2  * amn.bac.prod
  ## in stream
  bac.total.in.stream.season.1 <-   pop.total.in.stream.season.1   * amn.bac.prod
  bac.pasture.in.stream.season.1 <- pop.pasture.in.stream.season.1 * amn.bac.prod
  bac.forest.in.stream.season.1 <-  pop.forest.in.stream.season.1  * amn.bac.prod
  bac.RAOCUT.in.stream.season.1 <-  pop.RAOCUT.in.stream.season.1  * amn.bac.prod
  bac.total.in.stream.season.2 <-   pop.total.in.stream.season.2   * amn.bac.prod
  bac.pasture.in.stream.season.2 <- pop.pasture.in.stream.season.2 * amn.bac.prod
  bac.forest.in.stream.season.2 <-  pop.forest.in.stream.season.2  * amn.bac.prod
  bac.RAOCUT.in.stream.season.2 <-  pop.RAOCUT.in.stream.season.2  * amn.bac.prod
  ## accum values
  accum.pasture.season.1 <- 0
  accum.forest.season.1 <-  0
  accum.RAOCUT.season.1 <-  0
  accum.pasture.season.2 <- 0
  accum.forest.season.2 <-  0
  accum.RAOCUT.season.2 <-  0
  if(lu.pasture.area > 0) {
    accum.pasture.season.1 <- bac.pasture.on.land.season.1 / lu.pasture.area
  }
  if(lu.forest.area > 0) {
    accum.forest.season.1 <-  bac.forest.on.land.season.1 / lu.forest.area
  }
  if(lu.RAOCUT.area > 0) {
    accum.RAOCUT.season.1 <-  bac.RAOCUT.on.land.season.1 / lu.RAOCUT.area
  }
  if(lu.pasture.area > 0) {
    accum.pasture.season.2 <- bac.pasture.on.land.season.2 / lu.pasture.area
  }
  if(lu.forest.area > 0) {
    accum.forest.season.2 <-  bac.forest.on.land.season.2 / lu.forest.area
  }
  if(lu.RAOCUT.area > 0) {
    accum.RAOCUT.season.2 <-  bac.RAOCUT.on.land.season.2 / lu.RAOCUT.area
  }
  ##
## Assemble output data frame
  ## season 1
  df.output.season.1 <- data.frame(
    sub = chr.sub,
    Month=format(as.POSIXct(paste0("1967-",amn.months.season.1,"-01")), format = "%b"),
    pop.total=pop.total.season.1,
    pop.on.land=pop.total.on.land.season.1,
    pop.in.stream=pop.total.in.stream.season.1,
    Bacteria.total=bac.total.season.1,
    Bacteria.on.land=bac.total.on.land.season.1,
    Bacteria.direct.to.stream=bac.total.in.stream.season.1,
    Accum.pasture=accum.pasture.season.1,
    Accum.forest=accum.forest.season.1,
    Accum.RAOCUT=accum.RAOCUT.season.1,
    Lim.pasture=amn.SQLIM.factor * accum.pasture.season.1,
    Lim.forest=amn.SQLIM.factor * accum.forest.season.1,
    Lim.RAOCUT=amn.SQLIM.factor * accum.RAOCUT.season.1,
    month.order=amn.months.season.1,
    stringsAsFactors=FALSE)
  ## season 1
  df.output.season.2 <- data.frame(
    sub = chr.sub,
    Month=format(as.POSIXct(paste0("1967-",amn.months.season.2,"-01")), format = "%b"),
    pop.total=pop.total.season.2,
    pop.on.land=pop.total.on.land.season.2,
    pop.in.stream=pop.total.in.stream.season.2,
    Bacteria.total=bac.total.season.2,
    Bacteria.on.land=bac.total.on.land.season.2,
    Bacteria.direct.to.stream=bac.total.in.stream.season.2,
    Accum.pasture=accum.pasture.season.2,
    Accum.forest=accum.forest.season.2,
    Accum.RAOCUT=accum.RAOCUT.season.2,
    Lim.pasture=amn.SQLIM.factor * accum.pasture.season.2,
    Lim.forest=amn.SQLIM.factor * accum.forest.season.2,
    Lim.RAOCUT=amn.SQLIM.factor * accum.RAOCUT.season.2,
    month.order=amn.months.season.2,
    stringsAsFactors=FALSE)
  ## combine, re-order for months and drop month.order column
  df.output <- rbind(df.output.season.1, df.output.season.2)
  df.output <- df.output[order(df.output$month.order), ]
  df.output <- df.output[, -1 *grep("month.order", names(df.output))]

  ##
  ### return results
  return(df.output)
}
