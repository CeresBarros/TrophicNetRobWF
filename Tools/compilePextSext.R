#' Make tables of primary and secondary species extinctions per
#' pixel
#'
#' @param masterBL.file character. File path to the baseline "master" table  
#' @param masterScen.file character. File path to the scenario "master" table  
#' @param sppPA.fut.file character. File path to the table of species presences
#'   and absences as projected given (future) climate conditions.
#' @param redo logical. Should tables be recomputed even if they already exist on disk?
#' 
#' @details "master" tables are tables of species (columns) presences and absences
#'    per pixel (rows), given the trophic network of that pixel.
#'    Output tables are saved to `dirname(masterScen.file)`.
#'
#' @return character. Tables of primary and secondary species extinctions (where 1's
#'   correspond to the extinction of that species in a pixel) are saved to disk and
#'   their file paths returned.
#' 
#' @export
compilePextSext <- function(masterBL.file, masterScen.file, sppPA.fut.file, redo = FALSE) {
  outfile <- file.path(dirname(masterScen.file), sub("master", "Pext", basename(masterScen.file)))
  outfile2 <- file.path(dirname(masterScen.file), sub("master", "Sext", basename(masterScen.file)))
  
  if (file.exists(outfile) & file.exists(outfile2) & isFALSE(redo)) {
    message(paste(outfile, "\nand", outfile2, "\nalready exist and 'redo' is", 
                  redo, "\nskipping..."))
    return(c("masterScenPext" = outfile, "masterScenSext" = outfile2))
  }
  
  masterBL <-  readRDS(masterBL.file)
  masterScen <- readRDS(masterScen.file)
  sppPA.fut <- as.data.table(get(load(sppPA.fut.file)))

  ## species that go extinct from the entireterritory/metaweb will not appear on masterScen -- add them back
  sppNames <- grep("^(A|M|B|R)[[:digit:]]{1,3}", colnames(masterBL), value = TRUE)
  
  missingSpp <- setdiff(sppNames, names(masterScen))
  masterScen[, (missingSpp) := 0L]
  
  pixIDs <- masterScen[["PAGENAME"]]
  
  ## make matrix of absences by replacing 1s by 0s and vice-versa
  masterScenAbsences <- as.matrix(masterScen[, ..sppNames])
  rownames(masterScenAbsences) <- pixIDs
  masterScenAbsences[which(masterScenAbsences == 0)] <- -9999
  masterScenAbsences[which(masterScenAbsences == 1)] <- 0
  masterScenAbsences[which(masterScenAbsences == -9999)] <- 1
  
  ## convert baseline master to matrix
  masterBLM <- as.matrix(masterBL[, ..sppNames])
  rownames(masterBLM) <- masterBL[["PAGENAME"]]
  masterBLM <- masterBLM[pixIDs,]  ## we only care about pixels with networks
  
  if (grepl("hd_rcp85",  masterScen.file)) {
    ## make matrices of absences by replacing 1s by 0s and vice-versa
    sppPA.futAbsences <- as.matrix(sppPA.fut[, .SD, .SDcols = sppNames])
    rownames(sppPA.futAbsences) <- sppPA.fut[["PAGENAME"]]
    sppPA.futAbsences[which(sppPA.futAbsences == 0)] <- -9999
    sppPA.futAbsences[which(sppPA.futAbsences == 1)] <- 0
    sppPA.futAbsences[which(sppPA.futAbsences == -9999)] <- 1
    sppPA.futAbsences <- sppPA.futAbsences[pixIDs, ]  ## we only care about pixels with networks
    
    ## convert future climate master to matrix
    sppPA.futM <- as.matrix(sppPA.fut[, .SD, .SDcols = sppNames])
    rownames(sppPA.futM) <- sppPA.fut[["PAGENAME"]]
    sppPA.futM <- sppPA.futM[pixIDs,]
    
    ## PRIMARILY EXTINCT SPECIES
    ## species that are absent in the projected future distributions (sppPA.futAbsences == 1)
    ## but present in the baseline presences/absences (masterBLM == 1) lost suitable climate and are therefore
    ## primary extinctions
    masterScenPext <- masterBLM * sppPA.futAbsences
    
    ## SECONDARILY EXTINCT SPECIES
    ## species that are absent in the scenario master (masterScenAbsences == 1) and present in the projected 
    ## future distributions (sppPA.futM == 1) have suitable climate and are secondary extinctions
    ## IF they were also present in the baseline presences/absences (masterBLM == 1)
    masterScenSext <- sppPA.futM * masterScenAbsences
    masterScenSext <- masterScenSext * masterBLM
  }
  
  if (grepl("_CR_|_EN_|_VU_", masterScen.file)) {
    sppIUCNstatus <- as.data.table(read.table("Spp_traits_habs/IUCN/BARM_IUCNstatus.txt"))
    sppIUCNstatus <- sppIUCNstatus[europeanRegionalRedListCategory %in% c("CR", "EN", "VU"),
                                   .(ID, Spp, europeanRegionalRedListCategory)]
    sppIUCNstatus <- sppIUCNstatus[ID %in% sppNames]   ## some spp not in baseline networks/metaweb.
    
    ## ALL EXTINCTIONS
    ## species that are absent in the scenario master (masterScenAbsences == 1)
    ## and present in the baseline presences/absences (masterBLM == 1) are primary and secondary extinctions
    masterScenAllExt <- masterBLM * masterScenAbsences
    
    ## PRIMARILY EXTINCT SPECIES
    ## off the above species with a targeted endangered status are primary extinctions
    
    if (grepl("_CR_EN_VU_", masterScen.file)) {
      endangered <- sppIUCNstatus$ID
    } else if (grepl("_CR_EN", masterScen.file)) {
      endangered <- sppIUCNstatus[europeanRegionalRedListCategory %in% c("CR", "EN"), ID]
    } else if (grepl("_CR_", masterScen.file)) {
      endangered <- sppIUCNstatus[europeanRegionalRedListCategory %in% c("CR"), ID]
    }
    
    notEndangered <- setdiff(colnames(masterScenAllExt), endangered)
    endangered <- intersect(colnames(masterScenAllExt), endangered) ## filter to spp in table
    
    ## PRIMARILY EXTINCT SPECIES
    ## non-endangered species cannot be *primary* extinctions, endangered spp keep 1s
    masterScenPext <- masterScenAllExt
    masterScenPext[, notEndangered] <- 0  
    
    checkSpp <- intersect(names(which(colSums(masterScenPext) == 0)), endangered)
    if (length(checkSpp)) {
      if (any(colSums(masterBLM[, checkSpp, drop = FALSE]) > 0)) {
        ## if these species were present in the baseline networks, then they should
        ## have appeared with a colSums(masterScenPext) > 0
        stop("Something is wrong. Some endagered species that should have been extinct, where not.")
      }
    }
    
    ## SECONDARILY EXTINCT SPECIES
    ## endangered species cannot be *secondary* extinctions, non-endangered spp keep 1s
    masterScenSext <- masterScenAllExt
    masterScenSext[, endangered] <- 0  
  }
  
  ## check, the same species/pix can't have a 1 be in both
  if (any(masterScenPext == 1 & masterScenSext == 1))
    stop("Something is wrong. Some species appear as both primary and secondary extinctions")
  
  ## check, we can't have secondary extinctions if there were no primary extinctions.
  if (any(rowSums(masterScenPext) == 0 & rowSums(masterScenSext) > 0)) {
    stop("Secondary extinctions detected in pixels tha had no primary extinctions")
  }
  
  ## back to DT, add extra cols, then save
  masterScenPext <- data.table(PAGENAME = rownames(masterScenPext), as.data.table(masterScenPext))
  masterScenSext <- data.table(PAGENAME = rownames(masterScenSext), as.data.table(masterScenSext))
  
  nonSppCols <- setdiff(names(masterScen), sppNames)
  
  masterScenPext <- masterScen[, ..nonSppCols][masterScenPext, on = .(PAGENAME)]
  masterScenSext <- masterScen[, ..nonSppCols][masterScenSext, on = .(PAGENAME)]
  
  saveRDS(masterScenPext, file = outfile)
  saveRDS(masterScenSext, file = outfile2)
  
  return(c("masterScenPext" = outfile, "masterScenSext" = outfile2))
}
