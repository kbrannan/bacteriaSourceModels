#' model bacteria generation from elk
#'
#' This function is the bacteria source-model deer The model
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

wildlife.Elk <- function(chr.file.input) {

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
  lu.pasture.area.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Pasture Area in Watershed (ac)"])
  lu.forest.area.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Forest Area in Watershed (ac)"])
  lu.habitatarea.season.1 <- lu.pasture.area.season.1 + lu.forest.area.season.1
  lu.pasture.area.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Pasture Area in Watershed (ac)"])
  lu.forest.area.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Forest Area in Watershed (ac)"])
  lu.habitatarea.season.2 <- lu.pasture.area.season.2 + lu.forest.area.season.2
  ### Percent of Landuse with Stream access
  lu.pasture.percent.w.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of Pasture with stream access"]) / 100
  lu.forest.percent.w.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of Forest with stream access"]) / 100
  lu.pasture.percent.w.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of Pasture with stream access"]) / 100
  lu.forest.percent.w.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of Forest with stream access"]) / 100
  ## animal information
  ## months for seasons
  amn.months.season.1 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 1 Months (numbers)"], split = ",")[[1]])
  amn.months.season.2 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 2 Months (numbers)"], split = ",")[[1]])
  ## Animal Densities
  amn.animal.density.pasture.season.1  <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Animal Density for Pasture in watershed (animal/ac)"])
  amn.animal.density.forest.season.1  <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Animal Density for Forest in watershed (animal/ac)"])
  amn.animal.density.pasture.season.2  <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Animal Density for Pasture in watershed (animal/ac)"])
  amn.animal.density.forest.season.2  <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Animal Density for Forest in watershed (animal/ac)"])
    ## Percent animals around streams
  amn.percentstream.pasture.season.1 <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of animals on Pasture in and around streams"]) / 100
  amn.percentstream.forest.season.1 <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of animals on Forest in and around streams"]) / 100
  amn.percentstream.pasture.season.2 <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of animals on Pasture in and around streams"]) / 100
  amn.percentstream.forest.season.2 <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of animals on Forest in and around streams"]) / 100
  ## bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Bacteria Production of animal per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])

  ## Calculations
  ## land use
  ## without stream access
  lu.land.wo.stream.access.pasture.season.1 <- (1 - lu.pasture.percent.w.season.1) * lu.pasture.area.season.1
  lu.land.wo.stream.access.forest.season.1  <- (1 - lu.forest.percent.w.season.1) * lu.forest.area.season.1
  lu.land.wo.stream.access.total.season.1   <- lu.land.wo.stream.access.pasture.season.1 + lu.land.wo.stream.access.forest.season.1
  lu.land.wo.stream.access.pasture.season.2 <- (1 - lu.pasture.percent.w.season.2) * lu.pasture.area.season.2
  lu.land.wo.stream.access.forest.season.2  <- (1 - lu.forest.percent.w.season.2) * lu.forest.area.season.2
  lu.land.wo.stream.access.total.season.2   <- lu.land.wo.stream.access.pasture.season.2 + lu.land.wo.stream.access.forest.season.2
  ## with stream access
  lu.land.w.stream.access.pasture.season.1 <- lu.pasture.percent.w.season.1 * lu.pasture.area.season.1
  lu.land.w.stream.access.forest.season.1 <- lu.forest.percent.w.season.1 * lu.forest.area.season.1
  lu.land.w.stream.access.total.season.1   <- lu.land.w.stream.access.pasture.season.1 + lu.land.w.stream.access.forest.season.1
  lu.land.w.stream.access.pasture.season.2 <- lu.pasture.percent.w.season.2 * lu.pasture.area.season.2
  lu.land.w.stream.access.forest.season.2 <- lu.forest.percent.w.season.2 * lu.forest.area.season.2
  lu.land.w.stream.access.total.season.2   <- lu.land.w.stream.access.pasture.season.2 + lu.land.w.stream.access.forest.season.2
  ## populations
  ## overall locations
  pop.pasture.season.1 <- lu.pasture.area.season.1 * amn.animal.density.pasture.season.1
  pop.forest.season.1 <-  lu.forest.area.season.1 * amn.animal.density.forest.season.1
  pop.total.season.1 <- pop.pasture.season.1 + pop.forest.season.1
  pop.pasture.season.2 <- lu.pasture.area.season.2 * amn.animal.density.pasture.season.2
  pop.forest.season.2 <-  lu.forest.area.season.2 * amn.animal.density.forest.season.2
  pop.total.season.2 <- pop.pasture.season.2 + pop.forest.season.2
  ## on-land without stream access
  pop.pasture.on.land.wo.season.1 <- amn.animal.density.pasture.season.1 * lu.land.wo.stream.access.pasture.season.1
  pop.forest.on.land.wo.season.1 <- amn.animal.density.pasture.season.1 * lu.land.wo.stream.access.forest.season.1
  pop.total.on.land.wo.season.1   <- pop.pasture.on.land.wo.season.1 + pop.forest.on.land.wo.season.1
  pop.pasture.on.land.wo.season.2 <- amn.animal.density.pasture.season.2 * lu.land.wo.stream.access.pasture.season.2
  pop.forest.on.land.wo.season.2 <- amn.animal.density.pasture.season.2 * lu.land.wo.stream.access.forest.season.2
  pop.total.on.land.wo.season.2   <- pop.pasture.on.land.wo.season.2 + pop.forest.on.land.wo.season.2
  ## with stream access
  pop.pasture.w.season.1 <- amn.animal.density.pasture.season.1 * lu.land.w.stream.access.pasture.season.1
  pop.forest.w.season.1 <- amn.animal.density.pasture.season.1 * lu.land.w.stream.access.forest.season.1
  pop.pasture.w.season.2 <- amn.animal.density.pasture.season.2 * lu.land.w.stream.access.pasture.season.2
  pop.forest.w.season.2 <- amn.animal.density.pasture.season.2 * lu.land.w.stream.access.forest.season.2
  ## on-land with stream access
  pop.pasture.on.land.w.season.1 <- (1 - amn.percentstream.pasture.season.1) * pop.pasture.w.season.1
  pop.forest.on.land.w.season.1 <- (1 - amn.percentstream.forest.season.1) * pop.forest.w.season.1
  pop.total.on.land.w.season.1 <- pop.pasture.on.land.w.season.1 + pop.forest.on.land.w.season.1
  pop.pasture.on.land.w.season.2 <- (1 - amn.percentstream.pasture.season.2) * pop.pasture.w.season.2
  pop.forest.on.land.w.season.2 <- (1 - amn.percentstream.forest.season.2) * pop.forest.w.season.2
  pop.total.on.land.w.season.2 <- pop.pasture.on.land.w.season.2 + pop.forest.on.land.w.season.2
  ## in stream
  pop.pasture.in.stream.season.1 <- amn.percentstream.pasture.season.1 * pop.pasture.w.season.1
  pop.forest.in.stream.season.1 <- amn.percentstream.forest.season.1 * pop.forest.w.season.1
  pop.total.in.stream.season.1 <- pop.pasture.in.stream.season.1 + pop.forest.in.stream.season.1
  pop.pasture.in.stream.season.2 <- amn.percentstream.pasture.season.2 * pop.pasture.w.season.2
  pop.forest.in.stream.season.2 <- amn.percentstream.forest.season.2 * pop.forest.w.season.2
  pop.total.in.stream.season.2 <- pop.pasture.in.stream.season.2 + pop.forest.in.stream.season.2
  ## on-land
  pop.pasture.on.land.season.1 <- pop.pasture.on.land.wo.season.1 + pop.pasture.on.land.w.season.1
  pop.forest.on.land.season.1 <- pop.forest.on.land.wo.season.1 + pop.forest.on.land.w.season.1
  pop.total.on.land.season.1 <- pop.pasture.on.land.season.1 + pop.forest.on.land.season.1
  pop.pasture.on.land.season.2 <- pop.pasture.on.land.wo.season.2 + pop.pasture.on.land.w.season.2
  pop.forest.on.land.season.2 <- pop.forest.on.land.wo.season.2 + pop.forest.on.land.w.season.2
  pop.total.on.land.season.2 <- pop.pasture.on.land.season.2 + pop.forest.on.land.season.2
  ## bacteria loads
  ## overall locations
  bac.total.season.1 <-   amn.bac.prod * pop.total.season.1
  bac.pasture.season.1 <- amn.bac.prod * pop.pasture.season.1
  bac.forest.season.1 <-  amn.bac.prod * pop.forest.season.1
  bac.total.season.2 <-   amn.bac.prod * pop.total.season.2
  bac.pasture.season.2 <- amn.bac.prod * pop.pasture.season.2
  bac.forest.season.2 <-  amn.bac.prod * pop.forest.season.2
  ## on land
  bac.total.on.land.season.1 <-   pop.total.on.land.season.1   * amn.bac.prod
  bac.pasture.on.land.season.1 <- pop.pasture.on.land.season.1 * amn.bac.prod
  bac.forest.on.land.season.1 <-  pop.forest.on.land.season.1  * amn.bac.prod
  bac.total.on.land.season.2 <-   pop.total.on.land.season.2   * amn.bac.prod
  bac.pasture.on.land.season.2 <- pop.pasture.on.land.season.2 * amn.bac.prod
  bac.forest.on.land.season.2 <-  pop.forest.on.land.season.2  * amn.bac.prod
  ## in stream
  bac.total.in.stream.season.1 <-   pop.total.in.stream.season.1   * amn.bac.prod
  bac.pasture.in.stream.season.1 <- pop.pasture.in.stream.season.1 * amn.bac.prod
  bac.forest.in.stream.season.1 <-  pop.forest.in.stream.season.1  * amn.bac.prod
  bac.total.in.stream.season.2 <-   pop.total.in.stream.season.2   * amn.bac.prod
  bac.pasture.in.stream.season.2 <- pop.pasture.in.stream.season.2 * amn.bac.prod
  bac.forest.in.stream.season.2 <-  pop.forest.in.stream.season.2  * amn.bac.prod
  ## accum values
  accum.pasture.season.1 <- 0
  accum.forest.season.1 <-  0
  accum.pasture.season.2 <- 0
  accum.forest.season.2 <-  0
  if(lu.pasture.area.season.1 > 0) {
    accum.pasture.season.1 <- bac.pasture.on.land.season.1 /
      lu.pasture.area.season.1
  }
  if(lu.forest.area.season.1 > 0) {
    accum.forest.season.1 <-  bac.forest.on.land.season.1 /
      lu.forest.area.season.1
  }
  if(lu.pasture.area.season.2 > 0) {
    accum.pasture.season.2 <- bac.pasture.on.land.season.2 /
      lu.pasture.area.season.2
  }
  if(lu.forest.area.season.2 > 0) {
    accum.forest.season.2 <-  bac.forest.on.land.season.2 /
      lu.forest.area.season.2
  }
## Assemble output data frame
  ## season 1
  df.output.season.1 <- data.frame(
    Month=format(as.POSIXct(paste0("1967-",amn.months.season.1,"-01")), format = "%b"),
    pop.total=pop.total.season.1,
    pop.on.land=pop.total.on.land.season.1,
    pop.in.stream=pop.total.in.stream.season.1,
    Bacteria.total=bac.total.season.1,
    Bacteria.on.land=bac.total.on.land.season.1,
    Bacteria.direct.to.stream=bac.total.in.stream.season.1,
    Accum.pasture=accum.pasture.season.1,
    Accum.forest=accum.forest.season.1,
    Lim.pasture=amn.SQLIM.factor * accum.pasture.season.1,
    Lim.forest=amn.SQLIM.factor * accum.forest.season.1,
    month.order=amn.months.season.1,
    stringsAsFactors=FALSE)
  ## season 1
  df.output.season.2 <- data.frame(
    Month=format(as.POSIXct(paste0("1967-",amn.months.season.2,"-01")), format = "%b"),
    pop.total=pop.total.season.2,
    pop.on.land=pop.total.on.land.season.2,
    pop.in.stream=pop.total.in.stream.season.2,
    Bacteria.total=bac.total.season.2,
    Bacteria.on.land=bac.total.on.land.season.2,
    Bacteria.direct.to.stream=bac.total.in.stream.season.2,
    Accum.pasture=accum.pasture.season.2,
    Accum.forest=accum.forest.season.2,
    Lim.pasture=amn.SQLIM.factor * accum.pasture.season.2,
    Lim.forest=amn.SQLIM.factor * accum.forest.season.2,
    month.order=amn.months.season.2,
    stringsAsFactors=FALSE)
  ## combine, re-order for months and drop month.order column
  df.output <- rbind(df.output.season.1, df.output.season.2)
  df.output <- df.output[order(df.output$month.order), ]
  df.output <- df.output[, -1 * grep("month.order", names(df.output))]
  ## may get NaN in accum becuase of zero areas for habitat. replace these
  ## these NaN with 0
  df.nan <- df.output[, -1]
  df.nan[is.na(df.nan)] <- 0
  df.output <- cbind(sub = chr.sub, Month=df.output[, "Month"], df.nan)

  ##
  ## return results
  return(df.output)
}
