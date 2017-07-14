#' model bacteria generation from Hrrons or Egrets
#'
#' This function is the bacteria source-model herons and Ergtrets. The model
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
#' @param chr.file.input is the input file for the model
#' @export

  wildlife.HerEgr <- function(chr.file.input) {

  ## read input files
  df.input <- utils::read.delim(chr.file.input, sep=":",
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
    df.input$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"])
  ## gulls habitat set to all PLS area
  lu.habitatarea <- lu.pasture.area + lu.forest.area + lu.RAOCUT.area
  ### percent of habitat with stream access
  ## Note Gulls have stream access on all PLS
  lu.percentstream <- 100 / 100
  ## animal information
  ## population densities
  amn.density  <- as.numeric(df.input$value[
    df.input$parameter == "Animal Density (animal/ac)"])
  ## percent of time defecating in or around streams
  amn.percentstream <- as.numeric(df.input$value[
    df.input$parameter == "Percent of animals in and around streams"]) / 100
  ## all landuse has stream access for herons and ergrets
  ## bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Bacteria Production of animal per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])

  ##
## Calculations
  ## land use stream access
  ## with stream access
  lu.w.st.access <- lu.habitatarea * lu.percentstream
  lu.w.st.access.pasture <- lu.pasture.area * lu.percentstream
  lu.w.st.access.forest <- lu.forest.area * lu.percentstream
  lu.w.st.access.RAOCUT <- lu.RAOCUT.area * lu.percentstream
  ## without stream access
  lu.wo.st.access <- lu.habitatarea - lu.w.st.access
  lu.wo.st.access.pasture <- lu.pasture.area - lu.w.st.access.pasture
  lu.wo.st.access.forest <- lu.forest.area- lu.w.st.access.forest
  lu.wo.st.access.RAOCUT <- lu.RAOCUT.area - lu.w.st.access.RAOCUT
  ## populations
  ## overall locations
  pop.total   <- lu.habitatarea * amn.density
  pop.pasture   <- lu.pasture.area * amn.density
  pop.forest   <- lu.forest.area * amn.density
  pop.RAOCUT   <- lu.RAOCUT.area * amn.density
  ## on-land
  pop.on.land <- (lu.wo.st.access +
                    (1 - amn.percentstream) * lu.w.st.access) * amn.density
  pop.on.land.pasture <- (lu.wo.st.access.pasture +
                    (1 - amn.percentstream) * lu.w.st.access.pasture) *
    amn.density
  pop.on.land.forest <- (lu.wo.st.access.forest +
                            (1 - amn.percentstream) * lu.w.st.access.forest) *
    amn.density
  pop.on.land.RAOCUT <- (lu.wo.st.access.RAOCUT +
                            (1 - amn.percentstream) * lu.w.st.access.RAOCUT) *
    amn.density
  ## in-stream
  pop.in.stream <- amn.percentstream * lu.w.st.access * amn.density
  pop.instream.pasture <- amn.percentstream * lu.w.st.access.pasture *
    amn.density
  pop.in.stream.forest <- amn.percentstream * lu.w.st.access.forest *
    amn.density
  pop.in.stream.RAOCUT <- amn.percentstream * lu.w.st.access.RAOCUT *
    amn.density
  ## bacteria loads
  bac.total <- amn.bac.prod * pop.total
  bac.on.land  <- amn.bac.prod * pop.on.land
  bac.on.land.pasture  <- amn.bac.prod * pop.on.land.pasture
  bac.on.land.forest  <- amn.bac.prod * pop.on.land.forest
  bac.on.land.RAOCUT  <- amn.bac.prod * pop.on.land.RAOCUT
  bac.in.stream   <- amn.bac.prod * pop.in.stream
  ### accum values
  accum.pasture  <- 0
  accum.forest   <- 0
  accum.RAOCUT   <- 0
  if(lu.pasture.area > 0) {
    accum.pasture  <- bac.on.land.pasture / lu.pasture.area
  }
  if(lu.forest.area > 0) {
    accum.forest  <- bac.on.land.forest / lu.forest.area
  }
  if(lu.RAOCUT.area > 0) {
    accum.RAOCUT  <- bac.on.land.RAOCUT / lu.RAOCUT.area
  }
  ##
  ## Assemble output data frame
  df.output <- data.frame(
    sub = chr.sub,
    Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
    pop.total=pop.total,
    pop.on.land=pop.on.land,
    pop.in.stream=pop.in.stream,
    Bacteria.total=bac.total,
    Bacteria.on.land=bac.on.land,
    Bacteria.direct.to.stream=bac.in.stream,
    Accum.pasture=accum.pasture,
    Accum.forest=accum.forest,
    Accum.RAOCUT=accum.RAOCUT,
    Lim.pasture=amn.SQLIM.factor * accum.pasture,
    Lim.forest=amn.SQLIM.factor * accum.forest,
    Lim.RAOCUT=amn.SQLIM.factor * accum.RAOCUT,
    stringsAsFactors=FALSE)

  ##
  ### return results
  return(df.output)
}
