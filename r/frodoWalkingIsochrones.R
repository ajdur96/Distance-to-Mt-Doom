# frodoWalkingIsochrones.R
# Walking-effort isochrones from Bag End (Data Trust office, Arlington, VA), one ring per milestone of
# Frodo's journey. Distances: Fonstad, The Atlas of Middle-earth, as summarized
# at lord-of-the-steps.ru/en/blog/frodos-route.

library(terra)
library(sf)
library(slippymath)
library(rnaturalearth)

# The walking model (land cover, altitude, Tobler, rivers, bridges) is shared
# with continentExport.R and lives in modelParams.R, next to this script.
modelParamsPath <- Find(file.exists, c("modelParams.R", file.path("r", "modelParams.R")))
if (is.null(modelParamsPath)) stop("Can't find modelParams.R. Set the working directory to the folder with the R scripts.")
source(modelParamsPath)

# ---- Parameters ----------------------------------------------------------
originLon <- -77.0846    # Bag End: 1310 N Courthouse Rd, Arlington (Data Trust office)
originLat <- 38.8888
studyRadiusKm <- 3000     # must exceed the farthest milestone (2,895 km)
includeBoatLeg <- TRUE    # Anduin boat leg counted as if walked
outDir <- "frodoOutputs"
ringBasis <- "displacement"  # "displacement" = how far from home; "walked" = cumulative distance walked
useRoads <- FALSE         # TRUE runs the road variant and writes to frodoOutputsRoads
roadGradeCap <- 0.07      # roads switchback, so walkers on them never face more than ~7% grade
runUncertainty <- FALSE   # TRUE adds probability bands around each ring (uncertaintyBands.R; slow)

milestones <- data.frame(
  name = c("Bucklebury", "Bree", "Weathertop", "Rivendell", "Moria",
           "Lothlorien", "Amon Hen", "Dead Marshes", "Minas Morgul",
           "Cirith Ungol", "Mount Doom"),
  # Fonstad's leg lengths, except Weathertop-Rivendell: raised from 350 to 380 km
  # because the measured map scale puts its straight line at 364 km (modelParams.R)
  legKm = c(115, 100, 170, 380, 550, 195, 625, 255, 280, 35, 190),
  legMode = c(rep("walk", 6), "boat", rep("walk", 4)),
  # Straight-line distance from Hobbiton, raw ME-GIS miles (modelParams.R)
  meGisRawMi = meGisRawMi
)
# middleEarthScale lives in modelParams.R, shared with buildRoads.R
milestones$displacementMi <- milestones$meGisRawMi * middleEarthScale
if (!includeBoatLeg) milestones$legKm[milestones$legMode == "boat"] <- 0
milestones$cumKm <- cumsum(milestones$legKm)
milestones <- milestones[!duplicated(milestones$cumKm), ]  # drops Amon Hen if no boat leg

# tobler() and flatSpeedKmh come from modelParams.R
milestones$displacementKm <- milestones$displacementMi * 1.609344
milestones$ringKm <- if (ringBasis == "displacement") milestones$displacementKm else milestones$cumKm
milestones$thresholdHours <- milestones$ringKm / flatSpeedKmh

# ---- Study area in an azimuthal equidistant projection on Arlington -------
# Distances measured from the center are true in this projection.
aeqdCrs <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m +no_defs",
                   originLat, originLon)
originWgs <- st_sfc(st_point(c(originLon, originLat)), crs = 4326)
originAeqd <- st_transform(originWgs, aeqdCrs)
studyArea <- st_buffer(originAeqd, studyRadiusKm * 1000)
studyAreaWgs <- st_transform(studyArea, 4326)

# ---- DEM: download AWS terrain tiles directly ----------------------------
# Bypasses elevatr, whose connectivity check fails on this network. Uses the
# Windows network stack (wininet), which works where curl does not.
tileDir <- file.path(outDir, "demTiles")
dir.create(tileDir, recursive = TRUE, showWarnings = FALSE)

tiles <- bbox_to_tile_grid(st_bbox(studyAreaWgs), zoom = demZoom)$tiles
studyArea3857 <- st_transform(studyAreaWgs, 3857)
tileHitsStudy <- vapply(seq_len(nrow(tiles)), function(i) {
  tb <- unclass(tile_bbox(tiles$x[i], tiles$y[i], demZoom))  # slippymath attaches an old-style crs
  tilePoly <- st_as_sfc(st_bbox(c(xmin = tb[["xmin"]], ymin = tb[["ymin"]],
                                  xmax = tb[["xmax"]], ymax = tb[["ymax"]]), crs = 3857))
  st_intersects(tilePoly, studyArea3857, sparse = FALSE)[1, 1]
}, logical(1))
tiles <- tiles[tileHitsStudy, ]
message(nrow(tiles), " tiles to fetch (already downloaded tiles are skipped)")

tileFiles <- file.path(tileDir, sprintf("%d_%d_%d.tif", demZoom, tiles$x, tiles$y))
for (i in seq_len(nrow(tiles))) {
  if (file.exists(tileFiles[i])) next
  tileUrl <- sprintf("https://s3.amazonaws.com/elevation-tiles-prod/geotiff/%d/%d/%d.tif",
                     demZoom, tiles$x[i], tiles$y[i])
  tempTile <- paste0(tileFiles[i], ".part")  # rename on success, so a crash never leaves a broken tile
  ok <- tryCatch({
    suppressWarnings(download.file(tileUrl, tempTile, mode = "wb",
                                   method = "wininet", quiet = TRUE))
    file.rename(tempTile, tileFiles[i])
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    unlink(tempTile)
    message("Failed: ", tileUrl)
  }
  if (i %% 100 == 0) message(i, " of ", nrow(tiles), " tiles checked")
}

tileFiles <- tileFiles[file.exists(tileFiles)]
message(length(tileFiles), " tiles available for the mosaic")
demRaw <- vrt(tileFiles)
message(sprintf("Native tile cell size: %.0f m at the equator, about %.0f m at Bag End's latitude",
                res(demRaw)[1], res(demRaw)[1] * cos(originLat * pi / 180)))

templateRast <- rast(ext(vect(studyArea)), resolution = fineRes, crs = aeqdCrs)
demFine <- project(demRaw, templateRast, method = "bilinear")

# ---- Water mask: ocean (outside land) and lakes become NA barriers --------
cropToStudy <- function(sfObj) {
  v <- makeValid(vect(sfObj))
  project(crop(v, vect(studyAreaWgs)), aeqdCrs)
}
landV <- cropToStudy(ne_download(scale = 10, type = "land", category = "physical",
                                 returnclass = "sf"))
lakesV <- cropToStudy(ne_download(scale = 10, type = "lakes", category = "physical",
                                  returnclass = "sf"))
demFine <- mask(demFine, rasterize(landV, demFine))
demFine <- mask(demFine, rasterize(lakesV, demFine), inverse = TRUE)

# Probe points that must be water: open Atlantic and the middle of Lake Erie
waterProbes <- project(vect(cbind(c(-70, -81.2), c(37, 42.2)), crs = "EPSG:4326"), aeqdCrs)
probeValues <- extract(demFine, waterProbes)[, 2]
message(sprintf("Land polygons: %d, lake polygons: %d, Atlantic probe: %s, Lake Erie probe: %s",
                nrow(landV), nrow(lakesV), probeValues[1], probeValues[2]))
if (any(!is.na(probeValues))) stop("Water mask did not apply: a water probe is still walkable.")

# ---- Pace at fine resolution: slope x land cover x altitude ---------------
# Each piece comes from modelParams.R (sources there). The pieces are kept, since
# the optional uncertainty bands at the end vary them.
slopePaceFine <- slopePace(demFine)       # symmetric Tobler, hours per meter
landCover <- loadLandCover(demFine)       # NALCMS 2020, prepared by prepLandCover2020.R
paceFine <- slopePaceFine * frictionFromLandCover(landCover) * altitudeMultiplier(demFine)

# ---- Aggregate pace (not speed) to the coarse cost grid -------------------
paceCoarse <- aggregatePace(paceFine)  # method set by paceAggregate in modelParams.R
landFraction <- aggregate(walkableFine(paceFine, demFine), fact = coarseFactor, fun = "mean")  # land, minus walls if maxSlopeDeg is set
paceCoarse <- ifel(landFraction < minLandFraction, NA, paceCoarse)

# ---- Major rivers as barriers, crossable only at bridge cities ------------
# Rivers, bridge cities, and the rules for both live in modelParams.R. Rivers
# outside this study area (the Columbia, the Fraser) are skipped automatically.
barrierRivers <- loadBarrierRivers()
barrierResult <- applyRiverBarriers(paceCoarse, barrierRivers, vect(studyAreaWgs), aeqdCrs)
paceCoarse <- barrierResult$pace
riversV <- barrierResult$rivers
crossingPts <- barrierResult$crossings

# ---- Optional roads variant ----------------------------------------------
# Roads don't make walking faster than good open ground (Tobler's baseline is
# already path speed). They remove obstacles: no land cover penalty, and grades
# capped by switchbacks. Applied per 5 km cell, so a thin road isn't averaged away.
if (useRoads) {
  outDir <- "frodoOutputsRoads"
  roadsRaw <- vect(ne_download(scale = 10, type = "roads", category = "cultural", returnclass = "sf"))
  if ("type" %in% names(roadsRaw)) roadsRaw <- roadsRaw[!grepl("ferry", roadsRaw$type, ignore.case = TRUE)]
  roadsV <- project(crop(roadsRaw, vect(studyAreaWgs)), aeqdCrs)
  roadCells <- rasterize(roadsV, paceCoarse, touches = TRUE)

  demCoarse <- aggregate(demFine, fact = coarseFactor, fun = "mean", na.rm = TRUE)
  roadGrade <- clamp(tan(terrain(demCoarse, v = "slope", unit = "radians")),
                     upper = roadGradeCap, values = TRUE)
  altitudeCoarse <- altitudeMultiplier(demCoarse)
  roadPace <- (1 / tobler(roadGrade) + 1 / tobler(-roadGrade)) / 2 / 1000 * altitudeCoarse

  # Only improve cells that are already walkable, so roads never reopen rivers or lakes
  paceCoarse <- ifel(!is.na(roadCells) & !is.na(paceCoarse),
                     min(paceCoarse, roadPace, na.rm = TRUE), paceCoarse)
  message(sprintf("Roads variant: %d road segments applied", nrow(roadsV)))
}

# ---- Cost distance from Arlington ----------------------------------------
originCell <- cellFromXY(paceCoarse, st_coordinates(originAeqd))
if (is.na(paceCoarse[originCell][[1]])) {
  stop("Origin falls on a water/NA cell; nudge originLon or originLat.")
}
paceCoarse[originCell] <- 0
rawHours <- costDist(paceCoarse, target = 0, maxiter = 100)  # uncorrected; used to trace hero paths

# ---- Remove the octagon artifact of 8-direction movement -----------------
# On a uniform grid, 8-direction paths measure "octile" distance, which runs up
# to ~8% long between the axes and diagonals. Dividing out that error, cell by
# cell, by the direction from Bag End leaves terrain and water detours intact.
originXY <- st_coordinates(originAeqd)
dx <- abs(init(rawHours, "x") - originXY[1])
dy <- abs(init(rawHours, "y") - originXY[2])
octileDist <- max(dx, dy) + (sqrt(2) - 1) * min(dx, dy)
gridCorrection <- ifel(octileDist > 0, sqrt(dx^2 + dy^2) / octileDist, 1)
hoursFromOrigin <- rawHours * gridCorrection

# ---- Sanity check: a fairly flat Midwest point should be near ratio 1 -----
testWgs <- st_sfc(st_point(c(-86.16, 39.77)), crs = 4326)  # Indianapolis
testAeqd <- st_transform(testWgs, aeqdCrs)
testHours <- hoursFromOrigin[cellFromXY(hoursFromOrigin, st_coordinates(testAeqd))][[1]]
straightKm <- as.numeric(st_distance(originAeqd, testAeqd)) / 1000
cat(sprintf("Indianapolis: %.0f h modeled vs %.0f h flat straight line (ratio %.2f)\n",
            testHours, straightKm / flatSpeedKmh, testHours / (straightKm / flatSpeedKmh)))

# ---- Milestone rings -----------------------------------------------------
breaks <- c(0, milestones$thresholdHours)
rcl <- cbind(head(breaks, -1), breaks[-1], seq_len(nrow(milestones)))
bands <- classify(hoursFromOrigin, rcl, include.lowest = TRUE, others = NA)

ringPolys <- as.polygons(bands)
names(ringPolys) <- "milestoneId"
ringPolys$milestone <- milestones$name[ringPolys$milestoneId]
ringPolys$cumKm <- milestones$cumKm[ringPolys$milestoneId]
ringPolys$ringKm <- milestones$ringKm[ringPolys$milestoneId]

ringLines <- as.contour(hoursFromOrigin, levels = milestones$thresholdHours)
ringLines$milestone <- milestones$name[match(round(ringLines$level, 6),
                                             round(milestones$thresholdHours, 6))]

# ---- Quick look ----------------------------------------------------------
plot(bands, col = hcl.colors(nrow(milestones), "YlOrRd", rev = TRUE), legend = FALSE)
plot(ringLines, add = TRUE)
plot(vect(originAeqd), add = TRUE, pch = 19)

# Coastlines, country borders, and state/province lines for context
countryLinesV <- project(crop(vect(ne_download(scale = 50, type = "admin_0_boundary_lines_land",
                                               category = "cultural", returnclass = "sf")),
                              vect(studyAreaWgs)), aeqdCrs)
stateLinesV <- project(crop(vect(ne_download(scale = 50, type = "admin_1_states_provinces_lines",
                                             category = "cultural", returnclass = "sf")),
                            vect(studyAreaWgs)), aeqdCrs)
plot(stateLinesV, add = TRUE, col = "gray45", lwd = 0.5)
plot(countryLinesV, add = TRUE, col = "gray15", lwd = 1.5)
plot(landV, add = TRUE, border = "gray15", col = NA, lwd = 0.8)

# ---- Save intermediates so you never have to rerun the download ----------
dir.create(outDir, showWarnings = FALSE)
writeRaster(hoursFromOrigin, file.path(outDir, "hoursFromArlington.tif"), overwrite = TRUE)
writeVector(ringPolys, file.path(outDir, "frodoRings.gpkg"), overwrite = TRUE)
writeVector(ringLines, file.path(outDir, "frodoRingLines.gpkg"), overwrite = TRUE)
# writeVector(ringPolys, file.path(outDir, "frodoRings.shp"), overwrite = TRUE)

# ---- Places along each ring ----------------------------------------------
# A place "sits on" a ring when its modeled walking time is within
# placeToleranceKm of flat walking from the milestone threshold. Picks are
# greedy by population, skipping any place too close to one already chosen,
# so each ring's labels spread around the ring instead of clustering.
placeToleranceKm <- 25
nPlacesPerRing <- 5
minPlaceSpacingKm <- 150   # upper limit; small rings use tighter spacing
maxScaleRank <- 7          # Natural Earth SCALERANK: 0 = most prominent; higher = more obscure

placesRaw <- vect(ne_download(scale = 10, type = "populated_places",
                              category = "cultural", returnclass = "sf"))
neededFields <- c("NAME", "ADM1NAME", "ADM0NAME", "POP_MAX", "SCALERANK")
if (!all(neededFields %in% names(placesRaw))) {
  stop("Populated places fields differ from expected; check names(placesRaw).")
}
placesV <- project(crop(placesRaw, vect(studyAreaWgs)), aeqdCrs)
placesV$hours <- extract(hoursFromOrigin, placesV)[, 2]
placesV <- placesV[!is.na(placesV$hours)]

pickSpreadPlaces <- function(candidates, n, minSpacingM) {
  candidates <- candidates[order(candidates$SCALERANK, -candidates$POP_MAX)]  # fame first, then size
  keep <- integer(0)
  for (i in seq_len(nrow(candidates))) {
    if (length(keep) == 0 ||
        all(distance(candidates[i], candidates[keep]) > minSpacingM)) {
      keep <- c(keep, i)
    }
    if (length(keep) == n) break
  }
  candidates[keep]
}

ringPlacesList <- lapply(seq_len(nrow(milestones)), function(i) {
  offKm <- abs(placesV$hours - milestones$thresholdHours[i]) * flatSpeedKmh
  candidates <- placesV[offKm <= placeToleranceKm]
  if (nrow(candidates) == 0) return(NULL)
  famous <- candidates[candidates$SCALERANK <= maxScaleRank]
  if (nrow(famous) > 0) candidates <- famous  # fall back to obscure names only if nothing famous is on the ring
  # Spacing shrinks for small rings: about half the circumference split among the labels
  ringSpacingKm <- min(minPlaceSpacingKm, 2 * pi * milestones$ringKm[i] / (2 * nPlacesPerRing))
  picked <- pickSpreadPlaces(candidates, nPlacesPerRing, ringSpacingKm * 1000)
  picked$milestone <- milestones$name[i]
  picked$cumKm <- milestones$cumKm[i]
  picked[, c("NAME", "ADM1NAME", "ADM0NAME", "POP_MAX", "SCALERANK", "milestone", "cumKm")]
})
ringPlaces <- do.call(rbind, Filter(Negate(is.null), ringPlacesList))

missingRings <- setdiff(milestones$name, ringPlaces$milestone)
if (length(missingRings) > 0) {
  message("No places found for: ", paste(missingRings, collapse = ", "),
          ". Try raising placeToleranceKm.")
}

ringPlacesWgs <- project(ringPlaces, "EPSG:4326")
ringPlacesTable <- cbind(as.data.frame(ringPlacesWgs), crds(ringPlacesWgs))
names(ringPlacesTable)[names(ringPlacesTable) %in% c("x", "y")] <- c("lon", "lat")
print(ringPlacesTable[, c("milestone", "NAME", "ADM1NAME", "ADM0NAME")])

# ---- Hero destination candidates: famous places on the Mount Doom ring ------
doomHours <- tail(milestones$thresholdHours, 1)
doomOffKm <- abs(placesV$hours - doomHours) * flatSpeedKmh
doomCandidates <- placesV[doomOffKm <= placeToleranceKm]
doomCandidatesWgs <- project(doomCandidates, "EPSG:4326")
doomXY <- crds(doomCandidates)
doomTable <- data.frame(
  NAME = doomCandidates$NAME, ADM1NAME = doomCandidates$ADM1NAME,
  SCALERANK = doomCandidates$SCALERANK, POP_MAX = doomCandidates$POP_MAX,
  bearingDeg = round((atan2(doomXY[, 1] - originXY[1], doomXY[, 2] - originXY[2]) * 180 / pi) %% 360),
  lon = round(crds(doomCandidatesWgs)[, 1], 4), lat = round(crds(doomCandidatesWgs)[, 2], 4)
)
doomTable <- doomTable[order(doomTable$SCALERANK, -doomTable$POP_MAX), ]
cat("\nFamous places on the Mount Doom ring (bearing from Bag End: 0 = north, 270 = west):\n")
print(head(doomTable, 20), row.names = FALSE)

writeVector(ringPlaces, file.path(outDir, "frodoRingPlaces.gpkg"), overwrite = TRUE)
write.csv(ringPlacesTable, file.path(outDir, "frodoRingPlaces.csv"), row.names = FALSE)

# ---- Web-ready copies for the HTML story (WGS84, simplified) --------------
ringPolysWeb <- project(simplifyGeom(ringPolys, tolerance = 2000), "EPSG:4326")
writeVector(ringPolysWeb, file.path(outDir, "frodoRings.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# ---- Mountain glyph points for the parchment map -------------------------
# Local relief (max minus min elevation) per glyph cell. Cells with enough
# relief become points; the HTML draws each as a small inked peak, sized by relief.
glyphCellKm <- 20
minReliefM <- 400
glyphFact <- round(glyphCellKm * 1000 / fineRes)

reliefMax <- aggregate(demFine, fact = glyphFact, fun = "max", na.rm = TRUE)
reliefMin <- aggregate(demFine, fact = glyphFact, fun = "min", na.rm = TRUE)
relief <- reliefMax - reliefMin
names(relief) <- "reliefM"

glyphPts <- as.points(relief)
glyphPts <- glyphPts[glyphPts$reliefM >= minReliefM]
glyphPts$peakM <- extract(reliefMax, glyphPts)[, 2]
message(nrow(glyphPts), " mountain glyphs exported; adjust minReliefM to taste.")

writeVector(project(glyphPts, "EPSG:4326"), file.path(outDir, "mountainGlyphs.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# ---- Landmarks from Wikidata ----------------------------------------------
# Anything with coordinates and Wikipedia articles: battlefields, waterfalls,
# peaks, bridges, ghost towns, odd roadside attractions. Noteworthiness is the
# number of Wikipedia language editions covering it (sitelinks).
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite', type = 'binary')")
options(HTTPUserAgent = "frodoWalkingIsochrones/1.0 (personal mapping project)")

landmarkMinSitelinks <- 3
landmarkTileDeg <- 2
landmarkRadiusKm <- max(milestones$ringKm) + 300
landmarkDir <- file.path("frodoOutputs", "wikidataTiles")  # shared by both variants
dir.create(landmarkDir, recursive = TRUE, showWarnings = FALSE)

# Edit these to taste. Settlements are flagged, not deleted; noise is dropped.
settlementPattern <- paste("city|town|village|hamlet|municipality|census-designated|borough",
                           "township|county|unincorporated community|human settlement",
                           "neighborhood|neighbourhood|suburb|capital|locality",
                           "unincorporated area|planned community|rural community|indian reserve|first nations reserve",
                           sep = "|")
noisePattern <- paste("radio station|television station|school|college|university|business",
                      "company|enterprise|shopping|hotel|restaurant|office building|airport",
                      "railway station|bus station|hospital|weather station|post office",
                      "sports team|baseball team|football team|basketball team|hockey team|soccer club",
                      "sports season|season|event edition|pay-per-view|tournament|fair",
                      "newspaper|website|magazine|political party|movement|diocese|archdiocese",
                      "district of|school district|campaign|rapid transit",
                      "congressional district|electoral district|riding|metropolitan area|statistical area",
                      "prison|penitentiary|correctional",
                      "covert operation|military operation|intelligence operation", sep = "|")
# Big or abstract things whose single Wikidata point is arbitrary (a state's
# centroid, a river's midpoint, a language's "home"): dropped.
arealPattern <- paste("U\\.S\\. state|province|territory|region|colony|country|administrative",
                      "political border|language|organization|organisation|consortium|publisher",
                      "film|television series|novel|road|highway|route|river|stream|mountain range|peninsula",
                      "ethnic group|tribe|sporting event|Olympic|retirement community",
                      "air force base|airbase|military base|watershed|drainage basin",
                      "unrecognized state|historical country|watercourse|conflict|\\bwar\\b",
                      "parish of|civil parish|unorganized area|unorganized territory", sep = "|")
# Tragedies tied to real people: flagged so you decide case by case.
sensitivePattern <- paste("terrorist|bomb|massacre|shooting|civil disorder|riot|lynching",
                          "murder|assassination|genocide|unrest|attack|kidnapping|hostage",
                          "demonstration|rally|accident|crash|disaster|earthquake|wildfire|flood",
                          "witch trial|execution|outbreak|pandemic|epidemic", sep = "|")
# Exact labels to keep even if they match a pattern above, e.g. c("Jimmy Carter rabbit incident")
keepAnyway <- character(0)
# Exact labels to remove no matter what, e.g. c("Bunny Man")
dropAnyway <- character(0)
hideSensitive <- TRUE   # keep flagged items in the files, but leave them out of the printed lists

sparqlTemplate <- 'SELECT ?item ?itemLabel ?coord ?sitelinks ?typeLabel WHERE {
  SERVICE wikibase:box {
    ?item wdt:P625 ?coord .
    bd:serviceParam wikibase:cornerSouthWest "Point(%.4f %.4f)"^^geo:wktLiteral .
    bd:serviceParam wikibase:cornerNorthEast "Point(%.4f %.4f)"^^geo:wktLiteral .
  }
  ?item wikibase:sitelinks ?sitelinks .
  FILTER(?sitelinks >= %d)
  OPTIONAL { ?item wdt:P31 ?type . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en,fr,es". }
}'

lastWikidataError <- NA_character_
fetchLandmarkTile <- function(lon0, lat0, size, depth = 0) {
  tileFile <- file.path(landmarkDir, sprintf("wd_%.3f_%.3f_%.3f.rds", lon0, lat0, size))
  if (file.exists(tileFile)) return(readRDS(tileFile))
  query <- sprintf(sparqlTemplate, lon0, lat0, lon0 + size, lat0 + size, landmarkMinSitelinks)
  queryUrl <- paste0("https://query.wikidata.org/sparql?format=json&query=",
                     URLencode(query, reserved = TRUE))
  tempJson <- tempfile(fileext = ".json")
  httpWarning <- ""
  ok <- tryCatch({
    withCallingHandlers(
      download.file(queryUrl, tempJson, mode = "wb", method = "wininet", quiet = TRUE),
      warning = function(w) {
        # Ignore the harmless wininet deprecation notice; keep real warnings (HTTP status codes)
        if (!grepl("deprecated", conditionMessage(w))) httpWarning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      })
    TRUE
  }, error = function(e) {
    lastWikidataError <<- paste(conditionMessage(e), httpWarning)
    FALSE
  })
  Sys.sleep(1)  # be polite to the public query service
  # A query that times out mid-stream can still return a truncated file, so a
  # response that won't parse counts as a failure too (and gets split and retried).
  bindings <- NULL
  if (ok) {
    bindings <- tryCatch(jsonlite::fromJSON(tempJson)$results$bindings, error = function(e) {
      lastWikidataError <<- paste("Unreadable response (likely a timeout):", conditionMessage(e))
      "parseFailed"
    })
    if (identical(bindings, "parseFailed")) ok <- FALSE
  }
  if (!ok) {
    if (depth < 2) {  # likely a timeout in a dense tile: split into quarters and retry
      half <- size / 2
      parts <- lapply(list(c(0, 0), c(half, 0), c(0, half), c(half, half)), function(o)
        fetchLandmarkTile(lon0 + o[1], lat0 + o[2], half, depth + 1))
      if (any(vapply(parts, is.null, logical(1)))) return(NULL)  # never cache an incomplete tile
      result <- do.call(rbind, parts)
      saveRDS(result, tileFile)
      return(result)
    }
    message(sprintf("Failed tile at %.2f, %.2f: %s", lon0, lat0, lastWikidataError))
    return(NULL)
  }
  result <- if (length(bindings) == 0 || nrow(bindings) == 0) {
    data.frame(item = character(0), label = character(0), coord = character(0),
               sitelinks = numeric(0), type = character(0))
  } else {
    data.frame(item = bindings$item$value, label = bindings$itemLabel$value,
               coord = bindings$coord$value, sitelinks = as.numeric(bindings$sitelinks$value),
               type = if (!is.null(bindings$typeLabel)) bindings$typeLabel$value else NA_character_)
  }
  saveRDS(result, tileFile)
  result
}

# Preflight: one tiny query near Bag End. If this fails, stop instead of
# hammering hundreds of tiles with retries.
preflight <- fetchLandmarkTile(-77.2, 38.8, 0.2, depth = 2)
if (is.null(preflight)) stop("Wikidata preflight query failed: ", lastWikidataError)
message(sprintf("Wikidata preflight OK: %d items near Bag End", nrow(preflight)))

landmarkArea <- st_transform(st_buffer(originAeqd, landmarkRadiusKm * 1000), 4326)
lb <- st_bbox(landmarkArea)
tileGridLL <- expand.grid(lon0 = seq(floor(lb[["xmin"]]), lb[["xmax"]], by = landmarkTileDeg),
                          lat0 = seq(floor(lb[["ymin"]]), lb[["ymax"]], by = landmarkTileDeg))
tileKeep <- vapply(seq_len(nrow(tileGridLL)), function(i) {
  tileBox <- st_as_sfc(st_bbox(c(xmin = tileGridLL$lon0[i], ymin = tileGridLL$lat0[i],
                                 xmax = tileGridLL$lon0[i] + landmarkTileDeg,
                                 ymax = tileGridLL$lat0[i] + landmarkTileDeg), crs = 4326))
  lengths(st_intersects(tileBox, landmarkArea)) > 0
}, logical(1))
tileGridLL <- tileGridLL[tileKeep, ]
message(nrow(tileGridLL), " Wikidata tiles to query (cached tiles are skipped)")

landmarkRows <- vector("list", nrow(tileGridLL))
for (i in seq_len(nrow(tileGridLL))) {
  landmarkRows[[i]] <- fetchLandmarkTile(tileGridLL$lon0[i], tileGridLL$lat0[i], landmarkTileDeg)
  if (i %% 25 == 0) message(i, " of ", nrow(tileGridLL), " tiles done")
}
landmarkAll <- do.call(rbind, Filter(Negate(is.null), landmarkRows))

# One row per item, with all its types combined
landmarkTypes <- tapply(landmarkAll$type, landmarkAll$item,
                        function(x) paste(sort(unique(na.omit(x))), collapse = "; "))
landmarks <- landmarkAll[!duplicated(landmarkAll$item), c("item", "label", "coord", "sitelinks")]
landmarks$type <- as.character(landmarkTypes[landmarks$item])
coordNums <- regmatches(landmarks$coord, regexec("Point\\(([-0-9.eE]+) ([-0-9.eE]+)\\)", landmarks$coord))
landmarks$lon <- as.numeric(vapply(coordNums, function(m) if (length(m) == 3) m[2] else NA_character_, ""))
landmarks$lat <- as.numeric(vapply(coordNums, function(m) if (length(m) == 3) m[3] else NA_character_, ""))
landmarks <- landmarks[!is.na(landmarks$lon) & !grepl("^Q[0-9]+$", landmarks$label), ]  # drop unlabeled items

landmarks$isSettlement <- (grepl(settlementPattern, landmarks$type, ignore.case = TRUE) &
  !grepl("ghost town", landmarks$type, ignore.case = TRUE)) |
  (grepl("plantation", landmarks$type, ignore.case = TRUE) & grepl("Plantation$", landmarks$label))  # Maine townships, not Monticello
landmarks <- landmarks[!(landmarks$label %in% dropAnyway), ]
isKept <- landmarks$label %in% keepAnyway
landmarks <- landmarks[isKept | !grepl(noisePattern, landmarks$type, ignore.case = TRUE), ]
isKept <- landmarks$label %in% keepAnyway
landmarks <- landmarks[isKept | !grepl(arealPattern, landmarks$type, ignore.case = TRUE), ]
landmarks$isSensitive <- !(landmarks$label %in% keepAnyway) &
  grepl(sensitivePattern, landmarks$type, ignore.case = TRUE)

landmarksV <- project(vect(landmarks, geom = c("lon", "lat"), crs = "EPSG:4326", keepgeom = TRUE), aeqdCrs)
landmarksV$hours <- extract(hoursFromOrigin, landmarksV)[, 2]
landmarksV <- landmarksV[!is.na(landmarksV$hours)]

# Nearest ring for each landmark, and how far off it is in flat walking km
ringDiffs <- outer(landmarksV$hours, milestones$thresholdHours, "-")
nearestRingIdx <- apply(abs(ringDiffs), 1, which.min)
landmarksV$nearestRing <- milestones$name[nearestRingIdx]
landmarksV$ringOffKm <- round(abs(ringDiffs[cbind(seq_along(nearestRingIdx), nearestRingIdx)]) * flatSpeedKmh, 1)

message(sprintf("%d landmarks kept (%d flagged as settlements)",
                nrow(landmarksV), sum(landmarksV$isSettlement)))
nearRings <- as.data.frame(landmarksV)
nearRings <- nearRings[nearRings$ringOffKm <= placeToleranceKm & !nearRings$isSettlement, ]
if (hideSensitive) nearRings <- nearRings[!nearRings$isSensitive, ]
nearRings <- nearRings[order(match(nearRings$nearestRing, milestones$name), -nearRings$sitelinks), ]
cat("\nTop non-settlement landmarks near each ring:\n")
for (ringName in milestones$name) {
  top <- head(nearRings[nearRings$nearestRing == ringName, c("label", "type", "sitelinks")], 10)
  cat("\n", ringName, "\n", sep = "")
  if (nrow(top) > 0) print(top, row.names = FALSE) else cat("  (none)\n")
}

writeVector(landmarksV, file.path(outDir, "landmarks.gpkg"), overwrite = TRUE)
write.csv(as.data.frame(landmarksV), file.path(outDir, "landmarks.csv"), row.names = FALSE)

valueAt <- function(r, xy) {
  v <- extract(r, matrix(xy, ncol = 2))
  v[1, ncol(v)]
}
gradientStepM <- 2500

# ---- Chain picker: one landmark per ring, legs matching Frodo's ------------
# A layered shortest path (the Viterbi idea): each ring is a layer of candidate
# landmarks, and the picker finds the chain whose hops best match Frodo's real
# leg lengths, with a bonus for fame. A hop can be direct or pass through one
# detour landmark, which is how short displacement gains absorb long walks.
# Choosing uses a cheap estimate (straight-line distance times the local
# terrain ratio); true legs are built afterward for the chain you approve.
nRingCandidates <- 60      # landmarks considered per ring, spread evenly around it
candidateSectors <- 12     # ring split into this many slices; each contributes its most notable landmarks
sharedLandmarkPenaltyKm <- 150  # cost for reusing a landmark an earlier chain already used
legWiggleKm <- 2           # leg errors inside this band cost nothing
famePenaltyKm <- 50        # fame cost = famePenaltyKm / log2(1 + sitelinks); higher favors fame
detourMinSitelinks <- 5    # detour landmarks must be at least this notable
detourPoolSize <- 4000     # most notable detour candidates kept
chainDirections <- c(North = 0, East = 90, South = 180, West = 270)  # one chain per starting direction
directionHalfWidthDeg <- 60  # a chain's early waypoints must sit within this many degrees of its direction
directionRings <- 3          # rings the direction rule applies to; after that, chains roam freely
estimateScale <- 1.11      # true legs ran ~11% longer than estimates in the first test; recalibrate after each build
turnPenaltyKm <- 100       # cost of a full U-turn; turns of 90 degrees or less are free
barrierPenaltyKm <- 200    # added when a hop's straight line crosses water or a river barrier
barrierSamples <- 100      # points checked along each straight line
# Pin any ring to a landmark you choose, by exact label. Unpinned rings are picked.
# e.g. pinnedWaypoints <- c("Mount Doom" = "Some Landmark", "Lothlorien" = "Lake Placid")
pinnedWaypoints <- character(0)

lm <- as.data.frame(landmarksV, geom = "XY")
lm <- lm[!lm$isSensitive, ]
lm$effortKm <- lm$hours * flatSpeedKmh
lm$euclidKm <- sqrt((lm$x - originXY[1])^2 + (lm$y - originXY[2])^2) / 1000
lm$terrainRatio <- ifelse(lm$euclidKm > 5, pmax(lm$effortKm / lm$euclidKm, 1), 1)
lm$baseFameCostKm <- famePenaltyKm / log2(1 + lm$sitelinks)
lm$fameCostKm <- lm$baseFameCostKm
lm$usedByChain <- FALSE

ringOffAll <- abs(outer(lm$hours, milestones$thresholdHours, "-")) * flatSpeedKmh
bearingFromHome <- (atan2(lm$x - originXY[1], lm$y - originXY[2]) * 180 / pi) %% 360
makeLayers <- function(direction = NA) lapply(seq_len(nrow(milestones)), function(k) {
  ringName <- milestones$name[k]
  if (ringName %in% names(pinnedWaypoints)) {
    hit <- which(lm$label == pinnedWaypoints[[ringName]])
    if (length(hit) == 0) stop("Pinned landmark not found by exact label: ", pinnedWaypoints[[ringName]])
    if (ringOffAll[hit[1], k] > placeToleranceKm) {
      message(sprintf("Note: pinned %s is %.0f km off the %s ring", pinnedWaypoints[[ringName]],
                      ringOffAll[hit[1], k], ringName))
    }
    return(hit[1])
  }
  near <- which(ringOffAll[, k] <= placeToleranceKm)
  if (!is.na(direction) && k <= directionRings) {
    gap <- abs(((bearingFromHome[near] - direction) + 180) %% 360 - 180)
    near <- near[gap <= directionHalfWidthDeg]
  }
  pick <- near[!lm$isSettlement[near]]
  if (length(pick) < 5) pick <- near  # sparse ring: let towns in
  # Take the most notable landmarks from every slice of the ring, not just the
  # most notable overall, which would all sit in the dense Northeast
  pick <- pick[order(-lm$sitelinks[pick])]
  slice <- floor(bearingFromHome[pick] / (360 / candidateSectors))
  perSlice <- ceiling(nRingCandidates / candidateSectors)
  unlist(lapply(split(pick, slice), head, perSlice), use.names = FALSE)
})

detourPool <- which(!lm$isSettlement & lm$sitelinks >= detourMinSitelinks)
detourPool <- detourPool[order(-lm$sitelinks[detourPool])][seq_len(min(detourPoolSize, length(detourPool)))]

# Estimated walking effort (km) between every point in set A and set B
estKm <- function(ax, ay, ar, bx, by, br) {
  sqrt(outer(ax, bx, "-")^2 + outer(ay, by, "-")^2) / 1000 * outer(ar, br, "+") / 2 * estimateScale
}
# Does the straight line between each A and each B cross water or a barrier?
# The distance estimate can't see the St. Lawrence estuary; this can.
paceGridVals <- values(paceCoarse, mat = FALSE)
crossesBarrier <- function(ax, ay, bx, by) {
  blocked <- matrix(FALSE, length(ax), length(bx))
  for (t in seq(0.02, 0.98, length.out = barrierSamples)) {
    sx <- outer(ax, bx, function(a, b) a + t * (b - a))
    sy <- outer(ay, by, function(a, b) a + t * (b - a))
    cells <- cellFromXY(paceCoarse, cbind(as.vector(sx), as.vector(sy)))
    blocked <- blocked | matrix(is.na(paceGridVals[cells]), length(ax), length(bx))
  }
  blocked
}

# Penalty for turning back on yourself: 0 for turns up to 90 degrees, full at 180
turnCost <- function(cosTurn) {
  out <- turnPenaltyKm * pmax(-cosTurn, 0)
  out[is.na(out)] <- 0
  out
}

runPicker <- function(layers) {
  prevX <- originXY[1]; prevY <- originXY[2]
  prevR <- median(lm$terrainRatio[layers[[1]]])
  prevCost <- 0
  prevInX <- 0; prevInY <- 0  # direction each previous node was entered from (unit vector)
  prevIdx <- NA_integer_      # landmark index of each previous node (Bag End has none)
  backPointer <- detourChoice <- legEstimate <- vector("list", length(layers))

  for (k in seq_along(layers)) {
    b <- layers[[k]]
    nA <- length(prevX); nB <- length(b)
    targetKm <- milestones$legKm[k]
    direct <- estKm(prevX, prevY, prevR, lm$x[b], lm$y[b], lm$terrainRatio[b])
    abX <- -outer(prevX, lm$x[b], "-"); abY <- -outer(prevY, lm$y[b], "-")
    directCost <- pmax(abs(direct - targetKm) - legWiggleKm, 0) +
      turnCost((prevInX * abX + prevInY * abY) / sqrt(abX^2 + abY^2)) +
      barrierPenaltyKm * crossesBarrier(prevX, prevY, lm$x[b], lm$y[b])
    directCost[outer(prevIdx, b, "==") %in% TRUE] <- Inf  # never stay on the same landmark

    # Detours must sit between the previous ring (with some slack) and this ring
    lowHours <- if (k == 1) 0 else milestones$thresholdHours[k - 1] * 0.8
    pool <- detourPool[lm$hours[detourPool] >= lowHours & lm$hours[detourPool] <= milestones$thresholdHours[k]]
    detourCost <- matrix(Inf, nA, nB); detourIdx <- matrix(NA_integer_, nA, nB); detourEst <- matrix(NA_real_, nA, nB)
    if (length(pool) > 0) {
      toPool <- estKm(prevX, prevY, prevR, lm$x[pool], lm$y[pool], lm$terrainRatio[pool])
      poolToB <- estKm(lm$x[pool], lm$y[pool], lm$terrainRatio[pool], lm$x[b], lm$y[b], lm$terrainRatio[b])
      mbX <- -outer(lm$x[pool], lm$x[b], "-"); mbY <- -outer(lm$y[pool], lm$y[b], "-")
      mbLen <- sqrt(mbX^2 + mbY^2)
      poolToBBlocked <- crossesBarrier(lm$x[pool], lm$y[pool], lm$x[b], lm$y[b]) * barrierPenaltyKm
      poolToBBlocked[outer(pool, b, "==")] <- Inf  # a detour can't be the ring landmark itself
      toPoolBlocked <- crossesBarrier(prevX, prevY, lm$x[pool], lm$y[pool]) * barrierPenaltyKm
      for (a in seq_len(nA)) {
        viaTotal <- poolToB + toPool[a, ]  # pool x B
        amX <- lm$x[pool] - prevX[a]; amY <- lm$y[pool] - prevY[a]
        amLen <- sqrt(amX^2 + amY^2)
        turnAtA <- turnCost((prevInX[(a - 1) %% length(prevInX) + 1] * amX +
                             prevInY[(a - 1) %% length(prevInY) + 1] * amY) / amLen)
        turnAtM <- turnCost((amX * mbX + amY * mbY) / (amLen * mbLen))
        viaCost <- pmax(abs(viaTotal - targetKm) - legWiggleKm, 0) + lm$fameCostKm[pool] + turnAtA + turnAtM +
          poolToBBlocked + toPoolBlocked[a, ]
        if (!is.na(prevIdx[a])) viaCost[pool == prevIdx[a], ] <- Inf  # nor the landmark you just left
        best <- apply(viaCost, 2, which.min)
        detourCost[a, ] <- viaCost[cbind(best, seq_len(nB))]
        detourIdx[a, ] <- pool[best]
        detourEst[a, ] <- viaTotal[cbind(best, seq_len(nB))]
      }
    }
    useDetour <- detourCost < directCost
    legCost <- ifelse(useDetour, detourCost, directCost) +
      matrix(lm$fameCostKm[b], nA, nB, byrow = TRUE)
    totalCost <- legCost + prevCost
    bestPrev <- apply(totalCost, 2, which.min)
    pick <- cbind(bestPrev, seq_len(nB))
    backPointer[[k]] <- bestPrev
    detourChoice[[k]] <- ifelse(useDetour[pick], detourIdx[pick], NA_integer_)
    legEstimate[[k]] <- ifelse(useDetour[pick], detourEst[pick], direct[pick])
    prevCost <- totalCost[pick]
    enteredFromX <- ifelse(useDetour[pick], lm$x[detourIdx[pick]], prevX[bestPrev])
    enteredFromY <- ifelse(useDetour[pick], lm$y[detourIdx[pick]], prevY[bestPrev])
    inX <- lm$x[b] - enteredFromX; inY <- lm$y[b] - enteredFromY
    inLen <- sqrt(inX^2 + inY^2); inLen[inLen == 0] <- 1
    prevInX <- inX / inLen; prevInY <- inY / inLen
    prevX <- lm$x[b]; prevY <- lm$y[b]; prevR <- lm$terrainRatio[b]
    prevIdx <- b
  }
  list(prevCost = prevCost, backPointer = backPointer,
       detourChoice = detourChoice, legEstimate = legEstimate)
}

# One chain per starting direction, each the best-scoring route in its sector
chainRows <- list()
chainCounter <- 0
for (dirName in names(chainDirections)) {
  # Landmarks earlier chains used cost extra, so chains find their own routes and endings
  lm$fameCostKm <- lm$baseFameCostKm + sharedLandmarkPenaltyKm * lm$usedByChain
  dirLayers <- makeLayers(chainDirections[[dirName]])
  if (any(lengths(dirLayers) == 0)) {
    message(sprintf("No %s chain: no candidates on %s", dirName,
                    paste(milestones$name[lengths(dirLayers) == 0], collapse = ", ")))
    next
  }
  fit <- runPicker(dirLayers)
  j <- which.min(fit$prevCost)
  if (!is.finite(fit$prevCost[j])) {
    message(sprintf("No %s chain: every route was blocked", dirName))
    next
  }
  chainCounter <- chainCounter + 1
  endScore <- round(fit$prevCost[j])
  for (k in rev(seq_along(dirLayers))) {
    w <- dirLayers[[k]][j]
    d <- fit$detourChoice[[k]][j]
    lm$usedByChain[c(w, d[!is.na(d)])] <- TRUE
    chainRows[[length(chainRows) + 1]] <- data.frame(
      chainId = chainCounter, chainDirection = dirName, step = k, ring = milestones$name[k],
      label = lm$label[w], type = lm$type[w], sitelinks = lm$sitelinks[w],
      lon = lm$lon[w], lat = lm$lat[w],
      detourLabel = if (is.na(d)) "" else lm$label[d],
      detourLon = if (is.na(d)) NA else lm$lon[d], detourLat = if (is.na(d)) NA else lm$lat[d],
      targetLegKm = milestones$legKm[k], estLegKm = round(fit$legEstimate[[k]][j]),
      estErrorKm = round(fit$legEstimate[[k]][j] - milestones$legKm[k]),
      chainScore = endScore
    )
    j <- fit$backPointer[[k]][j]
  }
}
proposedChains <- do.call(rbind, chainRows)
proposedChains <- proposedChains[order(proposedChains$chainId, proposedChains$step), ]
chainFile <- file.path(outDir, "proposedChains.csv")
write.csv(proposedChains, chainFile, row.names = FALSE)
for (cid in unique(proposedChains$chainId)) {
  cat(sprintf("\nChain %d, heading %s (score %d)\n", cid,
              proposedChains$chainDirection[proposedChains$chainId == cid][1],
              proposedChains$chainScore[proposedChains$chainId == cid][1]))
  print(proposedChains[proposedChains$chainId == cid,
                       c("ring", "label", "detourLabel", "targetLegKm", "estLegKm", "estErrorKm")],
        row.names = FALSE)
}

# ---- Build the true legs for every proposed chain ---------------------------
# Edit proposedChains.csv to swap landmarks: change a label and blank its
# lon/lat to have coordinates looked up by exact label, then rerun this section.
chainsAll <- read.csv(chainFile, stringsAsFactors = FALSE)
buildChainIds <- sort(unique(chainsAll$chainId))  # or a subset, e.g. c(1, 3)
allLandmarks <- as.data.frame(landmarksV)
lookupLonLat <- function(lbl) {
  hit <- allLandmarks[allLandmarks$label == lbl, ]
  if (nrow(hit) == 0) stop("Landmark not found by label: ", lbl)
  c(hit$lon[1], hit$lat[1])
}
toAeqd <- function(lonLat) {
  as.numeric(st_coordinates(st_transform(st_sfc(st_point(lonLat), crs = 4326), aeqdCrs)))
}

# Bag End's cell was zeroed for the original run; restore a real pace there
paceBase <- paceCoarse * 1
if (isTRUE(paceBase[originCell][[1]] == 0)) {
  nbrVals <- unlist(paceBase[adjacent(paceBase, originCell, directions = "queen")[1, ]])
  paceBase[originCell] <- mean(nbrVals[nbrVals > 0], na.rm = TRUE)
}

surfaceFrom <- function(pc, fromXY) {
  pc <- pc * 1
  cell <- cellFromXY(pc, matrix(fromXY, ncol = 2))
  if (is.na(pc[cell][[1]])) stop("A waypoint sits on an unwalkable cell.")
  pc[cell] <- 0
  raw <- costDist(pc, target = 0, maxiter = 100)
  dx <- abs(init(raw, "x") - fromXY[1]); dy <- abs(init(raw, "y") - fromXY[2])
  octile <- max(dx, dy) + (sqrt(2) - 1) * min(dx, dy)
  list(raw = raw, hours = raw * ifel(octile > 0, sqrt(dx^2 + dy^2) / octile, 1))
}

traceDescent <- function(startXY, goalXY, rawR, hoursR) {
  rawVals <- values(rawR, mat = FALSE)
  aspectR <- terrain(hoursR, v = "aspect", unit = "radians")
  pos <- as.numeric(startXY)
  hoursNow <- valueAt(hoursR, pos)
  pathXY <- matrix(pos, ncol = 2)
  bestHours <- hoursNow; stall <- 0
  for (step in 1:20000) {
    if (sqrt(sum((pos - goalXY)^2)) < 3 * gradientStepM) break
    moved <- FALSE
    aspect <- valueAt(aspectR, pos)
    if (!is.na(aspect)) {
      cand <- pos + gradientStepM * c(sin(aspect), cos(aspect))
      candHours <- valueAt(hoursR, cand)
      if (!is.na(candHours) && candHours < hoursNow) { pos <- cand; hoursNow <- candHours; moved <- TRUE }
    }
    if (!moved) {
      cell <- cellFromXY(rawR, matrix(pos, ncol = 2))
      nbrs <- adjacent(rawR, cell, directions = "queen")[1, ]
      nbrs <- nbrs[!is.na(nbrs) & !is.na(rawVals[nbrs])]
      if (length(nbrs) == 0) stop("Leg path hit a dead end.")
      pos <- as.numeric(xyFromCell(rawR, nbrs[which.min(rawVals[nbrs])]))
      hoursNow <- valueAt(hoursR, pos)
    }
    pathXY <- rbind(pathXY, pos)
    if (hoursNow < bestHours - 0.01) { bestHours <- hoursNow; stall <- 0 } else {
      stall <- stall + 1
      if (stall > 200) stop("Leg path stalled; it may be circling a barrier.")
    }
  }
  rbind(pathXY, goalXY)
}

buildSegment <- function(fromXY, toXY, budgetKm) {
  marginM <- max(250000, budgetKm * 1000 * 0.6)
  cropExt <- intersect(ext(min(fromXY[1], toXY[1]) - marginM, max(fromXY[1], toXY[1]) + marginM,
                           min(fromXY[2], toXY[2]) - marginM, max(fromXY[2], toXY[2]) + marginM),
                       ext(paceBase))
  surf <- surfaceFrom(crop(paceBase, cropExt), fromXY)
  if (is.na(valueAt(surf$hours, toXY))) surf <- surfaceFrom(paceBase, fromXY)  # crop cut off the route
  path <- traceDescent(toXY, fromXY, surf$raw, surf$hours)
  list(pathXY = path[nrow(path):1, , drop = FALSE],
       effortKm = valueAt(surf$hours, toXY) * flatSpeedKmh)
}

buildChain <- function(buildChainId) {
  chain <- chainsAll[chainsAll$chainId == buildChainId, ]
  chain <- chain[order(chain$step), ]
  stops <- data.frame(label = "Bag End", ring = NA_character_, role = "start",
                      x = originXY[1], y = originXY[2], step = 0)
  for (r in seq_len(nrow(chain))) {
    if (nzchar(chain$detourLabel[r])) {
      dll <- if (is.na(chain$detourLon[r])) lookupLonLat(chain$detourLabel[r]) else c(chain$detourLon[r], chain$detourLat[r])
      dxy <- toAeqd(dll)
      stops <- rbind(stops, data.frame(label = chain$detourLabel[r], ring = NA, role = "detour",
                                       x = dxy[1], y = dxy[2], step = chain$step[r]))
    }
    wll <- if (is.na(chain$lon[r])) lookupLonLat(chain$label[r]) else c(chain$lon[r], chain$lat[r])
    wxy <- toAeqd(wll)
    stops <- rbind(stops, data.frame(label = chain$label[r], ring = chain$ring[r], role = "ring",
                                     x = wxy[1], y = wxy[2], step = chain$step[r]))
  }

  segmentPaths <- list(); segmentEffort <- numeric(0)
  for (s in 2:nrow(stops)) {
    budget <- milestones$legKm[stops$step[s]]
    message(sprintf("Building segment %d of %d: %s to %s", s - 1, nrow(stops) - 1, stops$label[s - 1], stops$label[s]))
    seg <- buildSegment(c(stops$x[s - 1], stops$y[s - 1]), c(stops$x[s], stops$y[s]), budget)
    segmentPaths[[s - 1]] <- if (s == 2) seg$pathXY else seg$pathXY[-1, , drop = FALSE]
    segmentEffort[s - 1] <- seg$effortKm
  }

  legSummary <- aggregate(segmentEffort, by = list(step = stops$step[-1]), FUN = sum)
  names(legSummary)[2] <- "trueLegKm"
  legSummary$ring <- milestones$name[legSummary$step]
  legSummary$targetLegKm <- milestones$legKm[legSummary$step]
  legSummary$trueErrorKm <- round(legSummary$trueLegKm - legSummary$targetLegKm)
  legSummary$trueLegKm <- round(legSummary$trueLegKm)
  estForChain <- chain$estLegKm[match(legSummary$step, chain$step)]
  cat(sprintf("\nTrue / estimated leg ratio (median): %.2f  (current estimateScale %.2f; multiply them for a new value)\n",
              median(legSummary$trueLegKm / estForChain, na.rm = TRUE), estimateScale))
  cat("\nTrue legs for chain", buildChainId, "heading", chain$chainDirection[1], "\n")
  print(legSummary[, c("ring", "targetLegKm", "trueLegKm", "trueErrorKm")], row.names = FALSE)

  chainXY <- do.call(rbind, segmentPaths)
  chainLine <- vect(chainXY, type = "lines", crs = aeqdCrs)
  chainLine$chainId <- buildChainId
  chainLine$direction <- chain$chainDirection[1]
  chainStops <- vect(as.matrix(stops[, c("x", "y")]), crs = aeqdCrs)
  values(chainStops) <- stops[, c("label", "ring", "role", "step")]
  chainStops$chainId <- buildChainId
  legSummary$chainId <- buildChainId
  legSummary$direction <- chain$chainDirection[1]

  densePts <- as.points(densify(chainLine, interval = 2000))
  denseXY <- crds(densePts)
  chainProfile <- data.frame(distKm = cumsum(c(0, sqrt(rowSums(diff(denseXY)^2)))) / 1000,
                             elevM = extract(demFine, densePts)[, 2])
  denseLonLat <- crds(project(densePts, "EPSG:4326"))
  chainProfile$lon <- denseLonLat[, 1]; chainProfile$lat <- denseLonLat[, 2]
  chainProfile$stop <- NA_character_
  for (s in seq_len(nrow(stops))) {
    nearestPt <- which.min((denseXY[, 1] - stops$x[s])^2 + (denseXY[, 2] - stops$y[s])^2)
    chainProfile$stop[nearestPt] <- stops$label[s]
  }


  writeVector(chainLine, file.path(outDir, sprintf("chain%d_path.gpkg", buildChainId)), overwrite = TRUE)
  writeVector(project(simplifyGeom(chainLine, tolerance = 2000), "EPSG:4326"),
              file.path(outDir, sprintf("chain%d_path.geojson", buildChainId)), filetype = "GeoJSON", overwrite = TRUE)
  writeVector(project(chainStops, "EPSG:4326"), file.path(outDir, sprintf("chain%d_stops.geojson", buildChainId)),
              filetype = "GeoJSON", overwrite = TRUE)
  write.csv(legSummary, file.path(outDir, sprintf("chain%d_legs.csv", buildChainId)), row.names = FALSE)
  write.csv(chainProfile, file.path(outDir, sprintf("chain%d_profile.csv", buildChainId)), row.names = FALSE)
  list(line = chainLine, stops = chainStops, legs = legSummary)
}

builtChains <- lapply(buildChainIds, buildChain)

# ---- Compare all chains on one map -------------------------------------------
chainColors <- c("#8B1A1A", "#1F4E79", "#2E6B30", "#B8860B", "#5B2C83", "#1B6F6A")
colorFor <- function(i) chainColors[(i - 1) %% length(chainColors) + 1]
zoomExt <- ext(trim(bands)) + 150000
plot(bands, col = hcl.colors(nrow(milestones), "YlOrRd", rev = TRUE), legend = FALSE,
     alpha = 0.55, ext = zoomExt)
plot(ringLines, add = TRUE, col = "gray35", lwd = 0.6)
plot(stateLinesV, add = TRUE, col = "gray55", lwd = 0.4)
plot(countryLinesV, add = TRUE, col = "gray15", lwd = 1.2)
plot(landV, add = TRUE, border = "gray15", col = NA, lwd = 0.6)
for (i in seq_along(builtChains)) {
  plot(builtChains[[i]]$line, add = TRUE, col = colorFor(i), lwd = 2.5)
  plot(builtChains[[i]]$stops, add = TRUE, col = colorFor(i), bg = "white", cex = 0.9,
       pch = ifelse(builtChains[[i]]$stops$role == "ring", 21, 24))
}
plot(vect(originAeqd), add = TRUE, pch = 19, cex = 1.3)
legend("bottomright", bty = "n", lwd = 2.5, cex = 0.8, col = sapply(seq_along(builtChains), colorFor),
       legend = sapply(builtChains, function(b) paste("Chain", b$line$chainId, "heading", b$line$direction)))

allPaths <- do.call(rbind, lapply(builtChains, `[[`, "line"))
allStops <- do.call(rbind, lapply(builtChains, `[[`, "stops"))
allLegs <- do.call(rbind, lapply(builtChains, `[[`, "legs"))
writeVector(project(simplifyGeom(allPaths, tolerance = 2000), "EPSG:4326"),
            file.path(outDir, "allChains_paths.geojson"), filetype = "GeoJSON", overwrite = TRUE)
writeVector(project(allStops, "EPSG:4326"), file.path(outDir, "allChains_stops.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)
write.csv(allLegs, file.path(outDir, "allChains_legs.csv"), row.names = FALSE)

cat("\nLeg error (km) by chain, side by side:\n")
errorWide <- reshape(allLegs[, c("ring", "step", "direction", "trueErrorKm")], idvar = c("step", "ring"),
                     timevar = "direction", direction = "wide")
names(errorWide) <- sub("trueErrorKm.", "", names(errorWide), fixed = TRUE)
print(errorWide[order(errorWide$step), names(errorWide) != "step"], row.names = FALSE)

# ---- Hero paths: walk downhill on the cost surface back to Bag End ---------
# The cost surface already holds the cheapest time from Bag End to every cell,
# so the best route home from any destination is steepest descent on it.
# Fill in from the Mount Doom candidate table above, e.g.
# data.frame(name = c("PlaceA", "PlaceB"), lon = c(...), lat = c(...))
heroDestinations <- data.frame(name = character(0), lon = numeric(0), lat = numeric(0))

rawHoursValues <- values(rawHours, mat = FALSE)
valueAt <- function(r, xy) {
  v <- extract(r, matrix(xy, ncol = 2))
  v[1, ncol(v)]
}

# Cell-to-cell descent alone produces ruler-straight paths along grid axes and
# diagonals. Instead, step a fixed distance down the gradient of the corrected
# surface (its aspect points downhill), which bends naturally with terrain.
# Near barriers and coasts, where the gradient is undefined, fall back to the
# cheapest neighboring cell for that step.
gradientStepM <- 2500
hoursAspect <- terrain(hoursFromOrigin, v = "aspect", unit = "radians")

traceHeroPath <- function(startXY) {
  pos <- as.numeric(startXY)
  hoursNow <- valueAt(hoursFromOrigin, pos)
  if (is.na(hoursNow)) stop("Hero destination is not on a reachable cell.")
  pathXY <- matrix(pos, ncol = 2)
  bestHours <- hoursNow
  stallSteps <- 0
  for (k in 1:20000) {
    if (sqrt(sum((pos - originXY)^2)) < 3 * gradientStepM) break
    stepped <- FALSE
    aspect <- valueAt(hoursAspect, pos)
    if (!is.na(aspect)) {
      candidate <- pos + gradientStepM * c(sin(aspect), cos(aspect))
      candidateHours <- valueAt(hoursFromOrigin, candidate)
      if (!is.na(candidateHours) && candidateHours < hoursNow) {
        pos <- candidate
        hoursNow <- candidateHours
        stepped <- TRUE
      }
    }
    if (!stepped) {
      cell <- cellFromXY(rawHours, matrix(pos, ncol = 2))
      nbrs <- adjacent(rawHours, cell, directions = "queen")[1, ]
      nbrs <- nbrs[!is.na(nbrs) & !is.na(rawHoursValues[nbrs])]
      if (length(nbrs) == 0) stop("Hero path hit a dead end.")
      pos <- as.numeric(xyFromCell(rawHours, nbrs[which.min(rawHoursValues[nbrs])]))
      hoursNow <- valueAt(hoursFromOrigin, pos)
    }
    pathXY <- rbind(pathXY, pos)
    if (hoursNow < bestHours - 0.01) {
      bestHours <- hoursNow
      stallSteps <- 0
    } else {
      stallSteps <- stallSteps + 1
      if (stallSteps > 200) stop("Hero path stalled; it may be circling a barrier.")
    }
  }
  pathXY <- rbind(pathXY, originXY)
  pathXY[nrow(pathXY):1, , drop = FALSE]  # Bag End first
}

if (nrow(heroDestinations) == 0) {
  message("No hero destinations set yet; skipping hero paths.")
} else {
  heroResults <- lapply(seq_len(nrow(heroDestinations)), function(i) {
    destWgs <- st_sfc(st_point(c(heroDestinations$lon[i], heroDestinations$lat[i])), crs = 4326)
    pathXY <- traceHeroPath(st_coordinates(st_transform(destWgs, aeqdCrs)))
    pathLine <- vect(pathXY, type = "lines", crs = aeqdCrs)
    pathLine$destination <- heroDestinations$name[i]

    # Profile samples every 2 km along the path
    densePts <- as.points(densify(pathLine, interval = 2000))
    denseXY <- crds(densePts)
    profile <- data.frame(
      destination = heroDestinations$name[i],
      distKm = cumsum(c(0, sqrt(rowSums(diff(denseXY)^2)))) / 1000,
      elevM = extract(demFine, densePts)[, 2],
      hours = extract(hoursFromOrigin, densePts)[, 2]
    )
    lonLat <- crds(project(densePts, "EPSG:4326"))
    profile$lon <- lonLat[, 1]
    profile$lat <- lonLat[, 2]
    profile$milestone <- NA_character_
    for (m in seq_len(nrow(milestones))) {
      hit <- which(profile$hours >= milestones$thresholdHours[m])[1]
      if (!is.na(hit)) profile$milestone[hit] <- milestones$name[m]
    }
    message(sprintf("%s: path %.0f km long, reaches %s", heroDestinations$name[i],
                    max(profile$distKm), tail(na.omit(profile$milestone), 1)))
    list(line = pathLine, profile = profile)
  })

  heroPaths <- do.call(rbind, lapply(heroResults, `[[`, "line"))
  heroProfiles <- do.call(rbind, lapply(heroResults, `[[`, "profile"))
  plot(heroPaths, add = TRUE, col = "black", lwd = 2)

  writeVector(heroPaths, file.path(outDir, "heroPaths.gpkg"), overwrite = TRUE)
  writeVector(project(simplifyGeom(heroPaths, tolerance = 3000), "EPSG:4326"),
              file.path(outDir, "heroPaths.geojson"), filetype = "GeoJSON", overwrite = TRUE)
  write.csv(heroProfiles, file.path(outDir, "heroProfiles.csv"), row.names = FALSE)
}

# ---- Bridges and barrier rivers for the HTML map ---------------------------
writeVector(project(crossingPts, "EPSG:4326"), file.path(outDir, "bridges.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)
writeVector(project(simplifyGeom(riversV, tolerance = 2000), "EPSG:4326"),
            file.path(outDir, "barrierRivers.geojson"), filetype = "GeoJSON", overwrite = TRUE)

# ---- Web export: everything the parchment HTML page needs ---------------------
# Writes one folder of small, simplified, lon/lat files. Zip frodoOutputs/web
# and upload it to build the page.
webChainIds <- c(1, 3)          # North and South
maxWebGlyphs <- 3000            # most prominent mountain glyphs kept for the page
webDir <- file.path(outDir, "web")
dir.create(webDir, recursive = TRUE, showWarnings = FALSE)
webExt <- ext(trim(bands)) + 300000

toWeb <- function(v, file, tolM = 0, cropIt = TRUE) {
  if (cropIt) v <- crop(v, webExt)
  if (tolM > 0) v <- simplifyGeom(v, tolerance = tolM)
  writeVector(project(v, "EPSG:4326"), file.path(webDir, file), filetype = "GeoJSON",
              overwrite = TRUE, options = c("COORDINATE_PRECISION=4", "RFC7946=YES"))
}

# Basemap
toWeb(landV, "land.geojson", tolM = 1500)
toWeb(lakesV, "lakes.geojson", tolM = 1500)
toWeb(stateLinesV, "stateLines.geojson", tolM = 1500)
toWeb(countryLinesV, "countryLines.geojson", tolM = 1500)

# Rings, rivers, bridges
toWeb(ringPolys, "rings.geojson", tolM = 3000)
toWeb(ringLines, "ringLines.geojson", tolM = 2000)
toWeb(riversV, "barrierRivers.geojson", tolM = 1500)
toWeb(crossingPts, "bridges.geojson")

# Mountain glyphs: the most prominent ones inside the map
glyphWeb <- crop(glyphPts, webExt)
if (nrow(glyphWeb) > maxWebGlyphs) glyphWeb <- glyphWeb[order(-glyphWeb$reliefM)][seq_len(maxWebGlyphs)]
toWeb(glyphWeb, "mountainGlyphs.geojson", cropIt = FALSE)

# Hero chains: paths, stops, and elevation profiles
toWeb(allPaths[allPaths$chainId %in% webChainIds], "chainPaths.geojson", tolM = 1500, cropIt = FALSE)
toWeb(allStops[allStops$chainId %in% webChainIds], "chainStops.geojson", cropIt = FALSE)
webProfiles <- do.call(rbind, lapply(webChainIds, function(id) {
  prof <- read.csv(file.path(outDir, sprintf("chain%d_profile.csv", id)))
  prof$chainId <- id
  prof$distKm <- round(prof$distKm, 1); prof$elevM <- round(prof$elevM)
  prof$lon <- round(prof$lon, 4); prof$lat <- round(prof$lat, 4)
  prof
}))
jsonlite::write_json(webProfiles, file.path(webDir, "chainProfiles.json"), dataframe = "columns", na = "null")
jsonlite::write_json(allLegs[allLegs$chainId %in% webChainIds, ], file.path(webDir, "chainLegs.json"),
                     dataframe = "rows")

# Top landmarks near each ring, for ring labels
ringLandmarks <- do.call(rbind, lapply(milestones$name, function(ringName) {
  head(nearRings[nearRings$nearestRing == ringName, c("label", "type", "sitelinks", "lon", "lat", "nearestRing")], 8)
}))
jsonlite::write_json(ringLandmarks, file.path(webDir, "ringLandmarks.json"), dataframe = "rows")

# Milestones
jsonlite::write_json(data.frame(name = milestones$name, legKm = milestones$legKm, cumKm = milestones$cumKm,
                                walkedMi = round(milestones$cumKm / 1.609344),
                                displacementMi = milestones$displacementMi,
                                thresholdHours = milestones$thresholdHours),
                     file.path(webDir, "milestones.json"), dataframe = "rows", digits = 6)

# Pace grid for the interactive "start anywhere" section. Pace is stored as
# one byte per 5 km cell on a log scale (0 = impassable), so the browser can
# rerun the cost-distance itself.
paceWebVals <- values(paceBase, mat = FALSE)
logPace <- log(paceWebVals)
logMin <- min(logPace, na.rm = TRUE); logMax <- max(logPace, na.rm = TRUE)
paceBytes <- ifelse(is.na(logPace), 0L, as.integer(round((logPace - logMin) / (logMax - logMin) * 254)) + 1L)
writeBin(as.raw(paceBytes), file.path(webDir, "paceGrid.bin"))
paceExt <- as.vector(ext(paceBase))
jsonlite::write_json(list(
  ncol = ncol(paceBase), nrow = nrow(paceBase), resM = res(paceBase)[1],
  xmin = paceExt[["xmin"]], ymax = paceExt[["ymax"]],
  logPaceMin = logMin, logPaceMax = logMax, paceUnits = "hours per meter",
  projection = list(type = "azimuthal equidistant", lon0 = originLon, lat0 = originLat),
  flatSpeedKmh = flatSpeedKmh
), file.path(webDir, "paceGrid.json"), auto_unbox = TRUE, digits = 10)

webFiles <- list.files(webDir, full.names = TRUE)
message(sprintf("Web export: %d files, %.1f MB total, in %s",
                length(webFiles), sum(file.size(webFiles)) / 1e6, normalizePath(webDir)))

# ---- Optional: uncertainty bands around each ring ------------------------------
if (runUncertainty) source(file.path(dirname(modelParamsPath), "uncertaintyBands.R"))
