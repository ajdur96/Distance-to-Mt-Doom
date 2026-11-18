# uncertaintyBands.R
# How sure is each ring? Reruns the Arlington cost distance many times, each time
# drawing the uncertain settings from a plausible range, and records how often
# every place falls inside each milestone's ring. The result is a band per ring:
# the 90% line (inside on almost every draw), the 50% line (the typical ring),
# and the 10% line (inside only on generous draws).
#
# What varies, and why:
#   - Land cover multipliers. Classes with measured walking speeds (grassland,
#     cropland, barren, wetland) vary a little; forest, shrub, polar, tropical,
#     and snow classes, which are the author's estimates, vary more.
#   - The altitude rate, across the individual range Wehrlin and Hallen measured
#     (4.6% to 7.5% per 1,000 m, most likely 6.3%).
#   - The Middle-earth scale (middleEarthScaleRange). This moves every ring's
#     distance together; narrow the range once the scale bar is measured.
# Slopes, rivers, bridges, and the grid itself are held fixed.
#
# Run from the end of frodoWalkingIsochrones.R (set runUncertainty <- TRUE); it
# reuses that run's elevation, land cover, river barriers, and milestones.
#
# Speed: averaging 400 fine cells into each 5 km cell is the slow step, so it is
# done once. Because the 5 km pace is a mean, it splits exactly by land cover
# class: for each class c, A_c sums the slope pace of its fine cells and B_c sums
# slope pace x altitude above the start. Any draw's pace is then
#   sum over c of multiplier_c x (A_c + rate x B_c), divided by the cell count,
# with no need to touch the 250 m grid again. Each draw then costs one cost
# distance run (a few minutes on the full study area).

nDraws <- 50                      # 50 gives the 10% and 90% lines to within a few %
bandLevels <- c(0.9, 0.5, 0.1)
frictionSdLog <- c(measured = 0.10, estimated = 0.25)  # spread of each class's extra time
measuredClasses <- c(9, 10, 14, 15, 16, 17)            # grassland, wetland, cropland, barren, urban
altitudeRateRange <- c(min = 0.046, mode = 0.063, max = 0.075)
set.seed(20261123)

if (useRoads) stop("Uncertainty bands are for the walking model; set useRoads <- FALSE.")
if (paceAggregate != "mean") {
  message("Note: the bands use mean averaging (the exact shortcut needs it), ",
          "while the main run uses \"", paceAggregate, "\".")
}
bandDir <- file.path(outDir, "uncertainty")
dir.create(bandDir, recursive = TRUE, showWarnings = FALSE)

# ---- Class sums on the 5 km grid (once) -----------------------------------------
valid <- !is.na(paceFine)  # the main run's walkable fine cells (water and lakes excluded)
lcClass <- ifel(is.na(landCover) | !(landCover %in% landCoverFriction$class), 0, landCover)
lcClass <- mask(lcClass, valid, maskvalues = FALSE)
altitudeAbove <- clamp(demFine - altitudePenaltyStartM, lower = 0, values = TRUE) / 1000
classes <- sort(setdiff(unique(lcClass)[[1]], 18))  # 0 = no land cover data
cellCount <- aggregate(valid, fact = coarseFactor, fun = "sum", na.rm = TRUE)
classSums <- list()
for (k in classes) {
  inClass <- lcClass == k
  pair <- c(ifel(inClass, slopePaceFine, 0), ifel(inClass, slopePaceFine * altitudeAbove, 0))
  classSums[[as.character(k)]] <- aggregate(pair, fact = coarseFactor, fun = "sum", na.rm = TRUE)
  message("Class sums done for land cover class ", k)
}
baseMultiplier <- function(k) if (k == 0) defaultFriction else landCoverFriction$multiplier[k]
paceFromDraw <- function(multipliers, rate) {
  total <- 0
  for (k in classes) {
    s <- classSums[[as.character(k)]]
    total <- total + multipliers[[as.character(k)]] * (s[[1]] + rate * s[[2]])
  }
  pace <- total / cellCount
  pace <- ifel(landFraction < minLandFraction, NA, pace)
  ifel(barrierResult$blocked, NA, pace + barrierResult$ferryAdd)
}

# Check: the shortcut with the base settings must reproduce the main run's pace
baseMult <- setNames(lapply(classes, baseMultiplier), as.character(classes))
check <- paceFromDraw(baseMult, altitudePenaltyPer1000m)
mainPace <- ifel(barrierResult$blocked, NA, ifel(landFraction < minLandFraction, NA,
                 aggregate(paceFine, fact = coarseFactor, fun = "mean", na.rm = TRUE) + barrierResult$ferryAdd))
gap <- global(abs(check - mainPace) / mainPace, "max", na.rm = TRUE)[[1]]
message(sprintf("Shortcut check: largest relative difference from the direct average %.2g", gap))
if (gap > 1e-4) stop("The class-sum shortcut doesn't match the direct average; check the inputs.")

# ---- Draws ------------------------------------------------------------------------
rtriangle <- function(lo, mode, hi) {
  u <- runif(1); f <- (mode - lo) / (hi - lo)
  if (u < f) lo + sqrt(u * (hi - lo) * (mode - lo)) else hi - sqrt((1 - u) * (hi - lo) * (hi - mode))
}
drawMultipliers <- function() {
  setNames(lapply(classes, function(k) {
    m <- baseMultiplier(k)
    sdLog <- frictionSdLog[[if (k %in% measuredClasses) "measured" else "estimated"]]
    1 + (m - 1) * exp(rnorm(1, 0, sdLog))  # varies the extra time, never below open ground
  }), as.character(classes))
}
originCellBands <- cellFromXY(barrierResult$pace, st_coordinates(originAeqd))
insideCount <- NULL
draws <- data.frame()
for (d in seq_len(nDraws)) {
  mult <- drawMultipliers()
  rate <- rtriangle(altitudeRateRange[["min"]], altitudeRateRange[["mode"]], altitudeRateRange[["max"]])
  scale <- if (ringBasis == "displacement") runif(1, middleEarthScaleRange[1], middleEarthScaleRange[2]) else NA
  pace <- paceFromDraw(mult, rate)
  pace[originCellBands] <- 0
  hours <- costDist(pace, target = 0, maxiter = 100) * gridCorrection
  ringKm <- if (ringBasis == "displacement") milestones$meGisRawMi * scale * 1.609344 else milestones$cumKm
  inside <- rast(lapply(ringKm / flatSpeedKmh, function(h) hours <= h))
  insideCount <- if (is.null(insideCount)) inside else insideCount + inside
  multCols <- setNames(as.list(unlist(mult)), paste0("class", names(mult)))  # class0 = no data
  draws <- rbind(draws, data.frame(draw = d, altitudeRate = rate, middleEarthScale = scale, multCols))
  message(sprintf("Draw %d of %d done (altitude %.3f, scale %s)", d, nDraws, rate,
                  if (is.na(scale)) "n/a" else sprintf("%.2f", scale)))
}
probInside <- insideCount / nDraws
names(probInside) <- milestones$name
writeRaster(probInside, file.path(bandDir, "probabilityInside.tif"), overwrite = TRUE)
write.csv(draws, file.path(bandDir, "draws.csv"), row.names = FALSE)

# ---- Band lines and a summary -----------------------------------------------------
bandPolys <- do.call(rbind, lapply(seq_len(nlyr(probInside)), function(i) {
  do.call(rbind, lapply(bandLevels, function(lv) {
    p <- as.polygons(ifel(probInside[[i]] >= lv - 1e-9, 1, NA), dissolve = TRUE)
    if (!nrow(p)) return(NULL)
    p$milestone <- milestones$name[i]
    p$level <- lv
    p[, c("milestone", "level")]
  }))
}))
bandsWgs <- simplifyGeom(project(bandPolys, "EPSG:4326"), tolerance = 0.01)
writeVector(bandsWgs, file.path(bandDir, "ringBands.geojson"), filetype = "GeoJSON", overwrite = TRUE)

# Width of each band as the change in equal-area radius from the 90% to the 10% line
bandArea <- expanse(bandPolys, unit = "km")
eqRadius <- sqrt(bandArea / pi)
summaryTab <- reshape(data.frame(milestone = bandPolys$milestone, level = bandPolys$level, r = eqRadius),
                      idvar = "milestone", timevar = "level", direction = "wide")
summaryTab <- summaryTab[match(milestones$name, summaryTab$milestone), ]
summaryTab$bandWidthKm <- summaryTab$r.0.1 - summaryTab$r.0.9
summaryTab$bandWidthPct <- 100 * summaryTab$bandWidthKm / summaryTab$r.0.5
write.csv(summaryTab, file.path(bandDir, "bandWidths.csv"), row.names = FALSE)
print(data.frame(milestone = summaryTab$milestone, typicalRadiusKm = round(summaryTab$r.0.5),
                 bandWidthKm = round(summaryTab$bandWidthKm),
                 bandWidthPct = round(summaryTab$bandWidthPct)), row.names = FALSE)
message("Uncertainty bands written to ", normalizePath(bandDir))
