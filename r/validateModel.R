# validateModel.R
# Which way of summarizing 250 m walking pace into the 5 km cost grid best matches
# what a cross-country walker would actually do?
#
# In a few test areas, this runs the walking model twice:
#   1. "Reference": cost distance on the full 250 m grid. The walker picks their own
#      line through the terrain, cross-country, with the same Tobler, land cover,
#      and altitude rules as the main model.
#   2. "Coarse": the same pace summarized to 5 km cells by each method in
#      modelParams.R (mean, median, p25), then cost distance on that grid.
# It compares walking hours from each test area's center to points 20 to 70 km away.
# The method whose 5 km hours best match the 250 m hours is the one to use; set
# paceAggregate in modelParams.R to it.
#
# This tests the 5 km shortcut against the model's own finer version. It does not
# test Tobler or the land cover values against real walkers; see the methods notes.
#
# Run from the folder with the R scripts, after frodoWalkingIsochrones.R has
# downloaded terrain tiles (tiles are shared; missing ones are fetched).
# Writes frodoOutputs/validation/aggregationTest.csv and aggregationTest.png.

library(terra)
library(sf)
library(slippymath)
library(rnaturalearth)

modelParamsPath <- Find(file.exists, c("modelParams.R", file.path("r", "modelParams.R")))
if (is.null(modelParamsPath)) stop("Can't find modelParams.R. Set the working directory to the folder with the R scripts.")
source(modelParamsPath)

outDir <- "frodoOutputs"
tileDir <- file.path(outDir, "demTiles")
valDir <- file.path(outDir, "validation")
dir.create(tileDir, recursive = TRUE, showWarnings = FALSE)
dir.create(valDir, recursive = TRUE, showWarnings = FALSE)

# ---- Test areas ----------------------------------------------------------------
# Rugged landscapes of different kinds, plus flat plains as a control. Each is a
# square about 150 km across, placed away from the barrier rivers so that river
# gaps don't muddy the comparison.
windowHalfKm <- 75
sampleMinKm <- 20       # compare walking hours at points this far from the center...
sampleMaxKm <- 70       # ...out to this far
methods <- c("mean", "median", "p25")
windows <- data.frame(
  name = c("Ridge and Valley (PA)", "Blue Ridge (NC)", "Front Range (CO)", "Cascades (WA)",
           "Sierra Nevada (CA)", "Ozarks (AR)", "Great Plains (KS, control)"),
  lon = c(-77.5, -82.9, -105.8, -121.6, -119.4, -92.8, -99.5),
  lat = c(40.6, 35.5, 39.6, 47.5, 37.8, 36.0, 38.5)
)

# ---- Helpers ---------------------------------------------------------------------
fetchTiles <- function(areaWgs) {
  tiles <- bbox_to_tile_grid(st_bbox(areaWgs), zoom = demZoom)$tiles
  files <- file.path(tileDir, sprintf("%d_%d_%d.tif", demZoom, tiles$x, tiles$y))
  for (i in seq_along(files)) {
    if (file.exists(files[i])) next
    url <- sprintf("https://s3.amazonaws.com/elevation-tiles-prod/geotiff/%d/%d/%d.tif", demZoom, tiles$x[i], tiles$y[i])
    part <- paste0(files[i], ".part")
    ok <- tryCatch({
      suppressWarnings(download.file(url, part, mode = "wb", method = "libcurl", quiet = TRUE))
      file.rename(part, files[i])
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) { unlink(part); message("Failed: ", url) }
  }
  files[file.exists(files)]
}

# On an 8-direction grid, paths between the axes and diagonals run up to 8% long.
# The main script divides that out by direction from the start; do the same here
# so both grids are corrected alike.
octileCorrection <- function(r, originXY) {
  dx <- abs(init(r, "x") - originXY[1])
  dy <- abs(init(r, "y") - originXY[2])
  octile <- max(dx, dy) + (sqrt(2) - 1) * min(dx, dy)
  ifel(octile > 0, sqrt(dx^2 + dy^2) / octile, 1)
}

walkingHours <- function(pace, originXY) {
  cell <- cellFromXY(pace, rbind(originXY))
  if (is.na(pace[cell][[1]])) stop("The test area's center is on water; move it.")
  pace[cell] <- 0
  hours <- costDist(pace, target = 0, maxiter = 200)
  hours * octileCorrection(hours, originXY)
}

landV <- makeValid(vect(ne_download(scale = 10, type = "land", category = "physical", returnclass = "sf")))
lakesV <- makeValid(vect(ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf")))

# Fine (250 m) pace and elevation on a square around lon/lat, in a local azimuthal projection
finePace <- function(lon, lat, halfKm, demRaw = NULL, landCover = NULL) {
  crs <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs", lat, lon)
  half <- halfKm * 1000
  template <- rast(ext(-half, half, -half, half), resolution = fineRes, crs = crs)
  areaWgs <- st_transform(st_as_sfc(st_bbox(c(xmin = -half, ymin = -half, xmax = half, ymax = half), crs = st_crs(crs))), 4326)

  if (is.null(demRaw)) demRaw <- vrt(fetchTiles(areaWgs))
  demFine <- project(demRaw, template, method = "bilinear")
  areaV <- vect(areaWgs)
  demFine <- mask(demFine, rasterize(project(crop(landV, areaV), crs), demFine))
  lakesHere <- crop(lakesV, areaV)
  if (nrow(lakesHere)) demFine <- mask(demFine, rasterize(project(lakesHere, crs), demFine), inverse = TRUE)
  landCover <- if (is.null(landCover)) loadLandCover(demFine) else project(landCover, demFine, method = "near")
  list(pace = paceFromDem(demFine, landCover), dem = demFine)
}

testWindow <- function(w, demRaw = NULL, landCover = NULL) {
  fine <- finePace(w$lon, w$lat, windowHalfKm, demRaw, landCover)
  paceFine <- fine$pace
  demFine <- fine$dem

  origin <- c(0, 0)
  reference <- walkingHours(paceFine, origin)

  # Compare at the 5 km cell centers in a ring 20-70 km out
  coarseTemplate <- aggregate(paceFine, fact = coarseFactor, fun = "mean", na.rm = TRUE)
  xy <- crds(as.points(coarseTemplate))
  distKm <- sqrt(rowSums(xy^2)) / 1000
  xy <- xy[distKm >= sampleMinKm & distKm <= sampleMaxKm, , drop = FALSE]
  refHours <- extract(reference, xy)[, 1]

  out <- lapply(methods, function(m) {
    coarse <- walkingHours(aggregatePace(paceFine, method = m), origin)
    h <- extract(coarse, xy)[, 1]
    ok <- is.finite(h) & is.finite(refHours) & refHours > 0
    err <- (h[ok] - refHours[ok]) / refHours[ok]
    data.frame(area = w$name, method = m, points = sum(ok),
               medianErrorPct = round(100 * median(err), 1),            # bias: + means the 5 km grid is too slow
               meanAbsErrorPct = round(100 * mean(abs(err)), 1),       # typical size of the miss
               rmsePct = round(100 * sqrt(mean(err^2)), 1))
  })
  relief <- global(demFine, fun = function(v) diff(quantile(v, c(0.05, 0.95), na.rm = TRUE)))[1, 1]
  res <- do.call(rbind, out)
  res$reliefM <- round(relief)
  res
}

plotResults <- function(results, file) {
  png(file, width = 1500, height = 900, res = 150)
  par(mar = c(9, 5, 4, 1), bg = "#f4ead2")
  areas <- unique(results$area)
  m <- t(sapply(areas, function(a) results$medianErrorPct[results$area == a][match(methods, results$method[results$area == a])]))
  colnames(m) <- methods
  cols <- c(mean = "#7a1f1a", median = "#2f5a49", p25 = "#6a5232")
  lim <- range(c(m, 0), na.rm = TRUE); lim <- lim + c(-0.08, 0.3) * diff(lim)  # room on top for the legend
  barplot(t(m), beside = TRUE, col = cols[methods], border = NA, las = 2, cex.names = 0.8, ylim = lim,
          ylab = "5 km hours vs. 250 m hours (median % difference)",
          main = "Which 5 km summary matches cross-country walking at 250 m?\n(0 = perfect match; above 0 = 5 km grid too slow)")
  abline(h = 0, lwd = 1.5)
  legend("topright", horiz = TRUE, legend = methods,
         fill = cols[methods], border = NA, bty = "n")
  dev.off()
}

# ---- Grand Canyon crossing test ----------------------------------------------------
# The Colorado's flow (about 420 m3/s) is far below the 1,500 m3/s barrier rule,
# but the Grand Canyon has just two footbridges (Phantom Ranch) between Navajo
# Bridge and Hoover Dam. This asks whether terrain alone already makes the canyon
# as hard as it really is, at 250 m and at 5 km, and how much a river barrier
# with a single gap at Phantom Ranch would change the 5 km answer.
#
# Benchmark: the corridor trails from the North Kaibab trailhead to the Bright
# Angel trailhead, about 23.5 miles, take hikers roughly 10-15 hours of walking
# (North Kaibab 5-7 h, Bright Angel 5-8 h; NPS figures via earthtrekkers.com).
# Those are graded trails; a cross-country walker should not be faster.
canyon <- list(
  center = c(lon = -112.10, lat = 36.14), halfKm = 40,
  from = c(lon = -112.0570, lat = 36.2168),  # North Kaibab trailhead (North Rim)
  to = c(lon = -112.1438, lat = 36.0573),    # Bright Angel trailhead (South Rim)
  bridge = c(lon = -112.0892, lat = 36.1009),  # Black Bridge, Phantom Ranch
  benchmarkHours = c(10, 15)
)
canyonTest <- function(demRaw = NULL, landCover = NULL, riverLine = NULL) {
  paceFine <- finePace(canyon$center[["lon"]], canyon$center[["lat"]], canyon$halfKm, demRaw, landCover)$pace
  crs <- crs(paceFine)
  toXY <- function(p) crds(project(vect(rbind(p), crs = "EPSG:4326"), crs))[1, ]
  fromXY <- toXY(canyon$from); targetXY <- toXY(canyon$to)
  hoursAt <- function(pace) extract(walkingHours(pace, fromXY), rbind(targetXY))[1, 1]
  straightKm <- sqrt(sum((targetXY - fromXY)^2)) / 1000

  rows <- list(data.frame(model = "250 m, terrain only", hours = hoursAt(paceFine)))
  for (m in methods) {
    rows[[length(rows) + 1]] <- data.frame(model = paste0("5 km ", m, ", terrain only"),
                                           hours = hoursAt(aggregatePace(paceFine, method = m)))
  }
  # Option B preview: the Colorado as a barrier, open only at Phantom Ranch
  if (is.null(riverLine)) {
    nr <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
    riverLine <- vect(nr[nr$name %in% "Colorado", ])
  }
  riverHere <- crop(project(riverLine, crs), ext(paceFine))
  paceMean <- aggregatePace(paceFine, method = paceAggregate)
  barrier <- !is.na(rasterize(buffer(riverHere, riverBufferM), paceMean, touches = TRUE))
  gap <- !is.na(rasterize(buffer(vect(rbind(toXY(canyon$bridge)), crs = crs), crossingRadiusKm * 1000),
                          paceMean, touches = TRUE))
  rows[[length(rows) + 1]] <- data.frame(model = paste0("5 km ", paceAggregate, ", river barrier + Phantom Ranch gap"),
                                         hours = hoursAt(ifel(barrier & !gap, NA, paceMean)))
  out <- do.call(rbind, rows)
  out$hours <- round(out$hours, 1)
  out$flatEquivalentKm <- round(out$hours * flatSpeedKmh, 1)
  out$straightLineKm <- round(straightKm, 1)
  out$benchmarkHours <- paste(canyon$benchmarkHours, collapse = "-")
  out
}

# ---- Run ---------------------------------------------------------------------------
if (!exists("validateNoRun")) {
  results <- do.call(rbind, lapply(seq_len(nrow(windows)), function(i) {
    message("Testing ", windows$name[i], " ...")
    testWindow(windows[i, ])
  }))
  write.csv(results, file.path(valDir, "aggregationTest.csv"), row.names = FALSE)
  plotResults(results, file.path(valDir, "aggregationTest.png"))
  overall <- aggregate(cbind(meanAbsErrorPct, rmsePct) ~ method, data = results[!grepl("control", results$area), ], FUN = mean)
  print(results)
  message("\nAverage over the rugged areas (lower is better):")
  print(overall)
  message("Best match: ", overall$method[which.min(overall$meanAbsErrorPct)],
          ". Set paceAggregate in modelParams.R to it, and rerun the main scripts.")

  message("\nGrand Canyon crossing, North Kaibab trailhead to Bright Angel trailhead:")
  canyonResults <- canyonTest()
  write.csv(canyonResults, file.path(valDir, "grandCanyonTest.csv"), row.names = FALSE)
  print(canyonResults, row.names = FALSE)
  message("If the terrain-only 5 km hours fall well under the ", paste(canyon$benchmarkHours, collapse = "-"),
          " h benchmark, the 5 km grid is smoothing the canyon away: a case for the Colorado barrier.")
}
