library(bacteriaSourceModels)

chr.sub.models <- as.character(lsf.str("package:bacteriaSourceModels"))
chr.sub.models <- grep("post", chr.sub.models, invert = TRUE, value = TRUE)

chr.dir.sub.models <- "M:/Models/Bacteria/HSPF/bacteria-sub-model-testing/SourceControl/sub-models"
chr.dir.hspf <- "M:/Models/Bacteria/HSPF/bacteria-sub-model-testing/SourceControl/hspf"
chr.file.sup <- "bigelkwq.sup"
chr.file.uci <- "bigelkwq.uci"
chr.name.wtsd <- "Big Elk Creek"

chr.dirs.sub.models <- list.dirs(path = chr.dir.sub.models, full.names = FALSE)
chr.dirs.sub.models <- grep("General", chr.dirs.sub.models[nchar(chr.dirs.sub.models) > 0], invert = TRUE, value = TRUE)

df.models <- data.frame(model = chr.sub.models, dir = chr.dirs.sub.models, stringsAsFactors = FALSE)

lst.updates <- post.run.models.update.sup.mut(chr.dir.sub.models, df.models, chr.dir.hspf, chr.file.sup, chr.file.uci)

post.write.sup.file(chr.dir.hspf, chr.file.sup, lst.updates[[1]])

post.write.mutsin.files(chr.dir.hspf, chr.file.uci, chr.name.wtsd, lst.updates[[2]])
