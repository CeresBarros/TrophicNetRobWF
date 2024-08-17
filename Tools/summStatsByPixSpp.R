#' Calculates summary statistics or a given metric across species for each
#' pixel, given another table of species to select per pixel (optional)
#'
#' @param metricDT a `data.table` of a species-level metric (the cell value, with 
#'   species as columns) per pixel (rows). Must have "PAGENAME" column for pix IDs.
#' @param binaryDT optional binary `data.table` of species that over which summary stats
#'   should be computed (the 1's) for each pixel (rows). Must have "PAGENAME" 
#'   column for pix IDs.
#'
#' @return an expanded `metricDT` with added columns "mean", "median", "min", 
#'   "max" and "sd".
#' 
#' @export
summStatsByPixSpp <- function(metricDT, binaryDT = NULL) {
  
  if (!is.null(binaryDT)) {
    metricDTout <- filterByTable(metricDT, binaryDT)
  } else {
    metricDTout <- copy(metricDT)
  }
  
  tetra <- grep("^(A|M|B|R)[[:digit:]]{1,3}", names(metricDTout), value = TRUE)
  metricDTout[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = tetra]
  metricDTout[, median := melt(metricDTout, measure.vars = tetra)[, r := 1:.N, variable][, median(value, na.rm = TRUE), by = r]$V1][]
  metricDTout[, sd := melt(metricDTout, measure.vars = tetra)[, r := 1:.N, variable][, sd(value, na.rm = TRUE), by = r]$V1][]
  metricDTout[, max := melt(metricDTout, measure.vars = tetra)[, r := 1:.N, variable][, max(value, na.rm = TRUE), by = r]$V1][]
  metricDTout[, min := melt(metricDTout, measure.vars = tetra)[, r := 1:.N, variable][, min(value, na.rm = TRUE), by = r]$V1][]
  
  return(metricDTout)
}



#' Filter values of a table based on second binary table
#'
#' @inheritParams summStatsByPixSpp
#' 
#' @return
#' @export
filterByTable <- function(metricDT, binaryDT) {
  tetra <- setdiff(names(metricDT), c("PAGENAME"))
  
  ## checks
  if (!"PAGENAME" %in% names(binaryDT)) 
    stop("binaryDT must have PAGENAME pix ID column")
  
  if (!"PAGENAME" %in% names(metricDT)) 
    stop("metricDT must have PAGENAME pix ID column")
  
  ## convert both to matrices
  matBinary <- binaryDT[, ..tetra] |>
    as.matrix()
  rownames(matBinary) <- binaryDT$PAGENAME
  
  matMetric <- metricDT[, ..tetra] |>
    as.matrix()
  rownames(matMetric) <- metricDT$PAGENAME
  
  ## subset and make same order
  matBinary <- matBinary[rownames(matMetric),]
  matBinary <- matBinary[, colnames(matMetric)]
  
  if (!identical(dim(matBinary), dim(matMetric))) {
    stop("matrices do not have identical dimensions")
  }
  
  ## make metric 0, then NA, where spp are 0s in matBinary
  matMetric <- matMetric * matBinary
  matMetric[matMetric == 0] <- NA
  
  ## now make DT and calculate stats
  metricDTout <- as.data.table(matMetric, keep.rownames = "PAGENAME")
}
