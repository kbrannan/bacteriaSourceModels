library(bacteriaSourceModels)
chr.sub.models <- as.character(lsf.str("package:bacteriaSourceModels"))
library(reshape2)

chr.dir.sub.models <- "M:/Models/Bacteria/HSPF/bacteria-sub-model-testing/SourceControl/sub-models"

chr.dirs.sub.models <- list.dirs(path = chr.dir.sub.models, full.names = FALSE)
chr.dirs.sub.models <- grep("General", chr.dirs.sub.models[nchar(chr.dirs.sub.models) > 0], invert = TRUE, value = TRUE)

df.models <- data.frame(model = chr.sub.models, dir = chr.dirs.sub.models, stringsAsFactors = FALSE)


run.model.source <- function(chr.file.input, chr.model) {
  eval(parse(text = paste0("df.cur <- ", chr.model, "('", chr.file.input, "')")))
  return(df.cur)
}

run.model.source.subs <- function(chr.model, chr.dir.sub.model) {
  library(reshape2)
  chr.files.input <- list.files(path = chr.dir.sub.model, pattern = "*\\.txt",
                                full.names = TRUE)
  df.cur <- do.call(rbind,lapply(chr.files.input, run.model.source, chr.model))
  df.r.cur <- melt(df.cur, id = c("sub", "Month"))
  df.r.cur$variable <- as.character(df.r.cur$variable)
  df.out <- data.frame(df.r.cur[, c(1,2)], source = chr.model, df.r.cur[, c(-1,-2)],
                  stringsAsFactors = FALSE)
  return(df.out)
}

df.out <- data.frame()

for(kk in 1:length(df.models$model)) {
  df.cur <- run.model.source.subs(df.models$model[kk],paste0(chr.dir.sub.models,"/", df.models$dir[kk]))
  df.out <- rbind(df.out,df.cur)
  rm(df.cur)
}

get.HSPF.input <- function(chr.srch, df.out, chr.col.name) {
  library(doBy)
  rows.var <- grep(chr.srch, df.out$variable, ignore.case = TRUE)

  df.var <- df.out[rows.var, c("sub", "Month", "variable", "value")]
  df.var$sub <- factor(df.var$sub)
  df.var$Month <- factor(df.var$Month, levels = format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"))
  df.var$variable <- gsub(chr.srch, "", df.var$variable, ignore.case = TRUE)
  df.var$variable <- factor(df.var$variable)
  df.var.sum <- summaryBy(value ~ sub + Month + variable, data = df.var, FUN = sum)
  names(df.var.sum) <- c("sub", "Month", chr.col.name, "load")
  return(df.var.sum)
}

df.accum.chk <- get.HSPF.input("accum\\.", df.out, "PLS")
df.lim.chk <- get.HSPF.input("lim\\.", df.out, "PLS")
df.mut.chk <- get.HSPF.input("bacteria\\.direct\\.to\\.stream", df.out, "MUTSIN")

# ii <- sample.int(length(df.models[, 1]), 1)
# chr.files.input <- list.files(path = paste0(chr.dir.sub.models, "/", df.models$dir[ii]),
#                               pattern = "*\\.txt")
# jj <- sample.int(length(chr.files.input), 1)
# chr.file.input <- paste0(chr.dir.sub.models,"/", df.models$dir[ii], "/", chr.files.input[jj])
# eval(parse(text = paste0("df.cur <- ", df.models$model[ii], "('", chr.file.input, "')")))
# df.models$model[ii]
# chr.file.input
# df.cur
# df.r.cur <- melt(df.cur, id = c("sub", "Month"))
#
#
# cbind(df.cur[, 1:2], data.frame(var = names(df.cur)[3], value = df.cur[,3]))
#
# chr.files.input.to.model <- paste0(chr.dir.sub.models,"/", df.models$dir[ii], "/", chr.files.input)
# df.cur <- run.model.source.subs(df.models$model[ii],paste0(chr.dir.sub.models,"/", df.models$dir[ii]))
#
# df.out <- data.frame()
#
# for(kk in 1:length(df.models$model)) {
#   df.cur <- run.model.source.subs(df.models$model[kk],paste0(chr.dir.sub.models,"/", df.models$dir[kk]))
#   df.out <- rbind(df.out,df.cur)
#   rm(df.cur)
# }
#
#
# library(doBy)
# rows.accum <- grep("accum\\.", df.out$variable, ignore.case = TRUE)
# rows.lim <- grep("lim\\.", df.out$variable, ignore.case = TRUE)
# rows.direct.to.stream <- grep("bacteria\\.direct\\.to\\.stream", df.out$variable, ignore.case = TRUE)
#
# df.accum <- df.out[rows.accum, c("sub", "Month", "variable", "value")]
# df.accum$sub <- factor(df.accum$sub)
# df.accum$Month <- factor(df.accum$Month, levels = format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"))
# df.accum$variable <- gsub("accum\\.", "", df.accum$variable, ignore.case = TRUE)
# df.accum$variable <- factor(df.accum$variable)
# df.accum.sum <- summaryBy(value ~ sub + Month + variable, data = df.accum, FUN = sum)
# names(df.accum.sum) <- c("sub", "Month", "PLS", "load")
