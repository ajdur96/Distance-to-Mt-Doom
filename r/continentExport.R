# continentExport.R
# Everything the "Your Journey" and "Frodo's Journey" tabs need beyond the
# Arlington run: a continent-wide pace grid for in-browser journeys, a slim
# landmark set with Wikipedia first sentences, a continent basemap, and
# licensed Wikimedia Commons images for the North road's ring landmarks.
#
# Order matters: the quick, network-bound steps run first (each cached and
# resumable), and the long raster steps run last, so a network hiccup never
# wastes hours of processing.

library(terra)
library(sf)
library(slippymath)
library(rnaturalearth)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite', type = 'binary')")
sf_use_s2(FALSE)  # planar geometry for simple bounding box tests
options(HTTPUserAgent = "frodoWalkingIsochrones/1.0 (personal mapping project)")

# ---- Parameters ----------------------------------------------------------
# Default extent: the contiguous US, southern Canada, and Mexico. Alaska and
# the Arctic add a lot of processing for very few readers; widen if you like.
continentBox <- c(xmin = -130, ymin = 14, xmax = -52, ymax = 60)
demZoom <- 8
fineRes <- 250
coarseFactor <- 20        # 250 m x 20 = 5 km grid, same as the Arlington run
minLandFraction <- 0.5
landCoverPath <- "C:/Users/avery/Downloads/land_cover_2010v2_250m_tif/NA_NALCMS_landcover_2010v2_250m/data/NA_NALCMS_landcover_2010v2_250m.tif"  # use the path that worked in the main script
defaultFriction <- 1.3
altitudePenaltyStartM <- 1500
altitudePenaltyPer1000m <- 0.10
riverBufferM <- 3000
crossingRadiusKm <- 8
landmarkTileDeg <- 2
landmarkMinSitelinks <- 3
landmarksPerCell <- 3     # most notable landmarks kept per 50 km cell, so every region has some
landmarkCellKm <- 50
maxWebGlyphs <- 4000

outDir <- "frodoOutputs"
contDir <- file.path(outDir, "continent")
webDir <- file.path(contDir, "web")
tileDir <- file.path(outDir, "demTiles")            # shared with the Arlington run
wdDir <- file.path(contDir, "wikidataTilesV2")      # new query shape, so a new cache
wpDir <- file.path(contDir, "wikipediaBatches")
imgDir <- file.path(webDir, "images")
for (d in c(contDir, webDir, tileDir, wdDir, wpDir, imgDir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Spherical Lambert azimuthal equal area on North America. Defined on a sphere
# so the browser's D3 projection matches it exactly.
laeaCrs <- "+proj=laea +lat_0=45 +lon_0=-100 +R=6371007.181 +units=m +no_defs"

tobler <- function(grade) 6 * exp(-3.5 * abs(grade + 0.05))
flatSpeedKmh <- tobler(0)

# ---- Small helpers ---------------------------------------------------------
# Download with libcurl. This works with the HTTPS Wikimedia endpoints
# and avoids the WinINet method, which failed for Wikimedia Commons.
fetchToFile <- function(url, dest) {
  tryCatch({
    status <- withCallingHandlers(
      download.file(
        url,
        dest,
        mode = "wb",
        method = "libcurl",
        quiet = TRUE
      ),
      warning = function(w) invokeRestart("muffleWarning")
    )
    isTRUE(status == 0)
  }, error = function(e) {
    message("Download failed: ", conditionMessage(e))
    FALSE
  })
}
fetchJson <- function(url) {
  tmp <- tempfile(fileext = ".json")
  if (!fetchToFile(url, tmp)) return(NULL)
  tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
}
toWeb <- function(v, file, tolDeg = 0) {
  v <- crop(v, ext(continentBox[["xmin"]], continentBox[["xmax"]], continentBox[["ymin"]], continentBox[["ymax"]]))
  if (tolDeg > 0) v <- simplifyGeom(v, tolerance = tolDeg)
  writeVector(v, file.path(webDir, file), filetype = "GeoJSON", overwrite = TRUE,
              options = c("COORDINATE_PRECISION=4", "RFC7946=YES"))
}
stripHtml <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", x)))

boxWgs <- st_as_sfc(st_bbox(continentBox, crs = 4326))
# Add points every half degree along the box edges, so the edges curve correctly
# when projected (done by hand, since st_segmentize needs the lwgeom package)
boxCoords <- with(as.list(continentBox), rbind(
  cbind(seq(xmin, xmax, by = 0.5), ymin),
  cbind(xmax, seq(ymin, ymax, by = 0.5)),
  cbind(seq(xmax, xmin, by = -0.5), ymax),
  cbind(xmin, seq(ymax, ymin, by = -0.5))
))
boxLaea <- st_transform(st_sfc(st_polygon(list(rbind(boxCoords, boxCoords[1, ]))), crs = 4326), laeaCrs)

# ---- Preflight: can we reach every service this script needs? -------------
for (u in c("https://query.wikidata.org/sparql?format=json&query=ASK%7B%7D",
            "https://en.wikipedia.org/w/api.php?action=query&meta=siteinfo&format=json",
            "https://commons.wikimedia.org/w/api.php?action=query&meta=siteinfo&format=json")) {
  if (is.null(fetchJson(u))) stop("Cannot reach ", sub("^https://([^/]+).*", "\\1", u),
                                  ". Try another network before running this script.")
}
message("Preflight OK: Wikidata, Wikipedia, and Commons are reachable")

# ---- Basemap for the continent (quick) -------------------------------------
landSf <- ne_download(scale = 10, type = "land", category = "physical", returnclass = "sf")
lakesSf <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf")
landV <- makeValid(vect(landSf)); lakesV <- makeValid(vect(lakesSf))
toWeb(landV, "land.geojson", tolDeg = 0.02)
toWeb(lakesV, "lakes.geojson", tolDeg = 0.02)
toWeb(vect(ne_download(scale = 50, type = "admin_1_states_provinces_lines", category = "cultural", returnclass = "sf")),
      "stateLines.geojson", tolDeg = 0.02)
toWeb(vect(ne_download(scale = 50, type = "admin_0_boundary_lines_land", category = "cultural", returnclass = "sf")),
      "countryLines.geojson", tolDeg = 0.02)
message("Basemap written")

# ---- Images for the North road's ring landmarks (quick) --------------------
# Wikidata's image property (P18) always points to Wikimedia Commons, which
# only hosts freely licensed files; the license and author are kept for credit.
northStops <- vect(file.path(outDir, "chain1_stops.geojson"))
northRing <- as.data.frame(northStops)[northStops$role == "ring", c("label", "ring", "step")]
arlingtonLandmarks <- read.csv(file.path(outDir, "landmarks.csv"), stringsAsFactors = FALSE)
northRing$qid <- sub(".*/", "", arlingtonLandmarks$item[match(northRing$label, arlingtonLandmarks$label)])

imageCredits <- list()
for (r in seq_len(nrow(northRing))) {
  qid <- northRing$qid[r]
  if (is.na(qid)) { message("No Wikidata id for ", northRing$label[r]); next }
  claims <- fetchJson(sprintf("https://www.wikidata.org/w/api.php?action=wbgetclaims&entity=%s&property=P18&format=json", qid))
  fileName <- tryCatch(claims$claims$P18[[1]]$mainsnak$datavalue$value, error = function(e) NULL)
  if (is.null(fileName)) { message("No image on Wikidata for ", northRing$label[r]); next }
  info <- fetchJson(paste0("https://commons.wikimedia.org/w/api.php?action=query&prop=imageinfo",
                           "&iiprop=url%7Cextmetadata&iiurlwidth=640&format=json&titles=",
                           URLencode(paste0("File:", fileName), reserved = TRUE)))
  page <- tryCatch(info$query$pages[[1]]$imageinfo[[1]], error = function(e) NULL)
  if (is.null(page)) { message("No Commons info for ", fileName); next }
  meta <- page$extmetadata
  thumbFile <- sprintf("step%02d_%s.jpg", northRing$step[r], qid)
  if (!file.exists(file.path(imgDir, thumbFile))) fetchToFile(page$thumburl, file.path(imgDir, thumbFile))
  imageCredits[[length(imageCredits) + 1]] <- list(
    step = northRing$step[r], label = northRing$label[r], file = thumbFile,
    commonsPage = page$descriptionurl,
    author = if (!is.null(meta$Artist$value)) stripHtml(meta$Artist$value) else "Unknown",
    license = if (!is.null(meta$LicenseShortName$value)) meta$LicenseShortName$value else "See Commons page",
    licenseUrl = if (!is.null(meta$LicenseUrl$value)) meta$LicenseUrl$value else page$descriptionurl
  )
}
jsonlite::write_json(imageCredits, file.path(webDir, "northImages.json"), auto_unbox = TRUE, pretty = TRUE)
message(length(imageCredits), " of ", nrow(northRing), " ring landmarks have an image")

# ---- Landmarks from Wikidata, now with English Wikipedia titles -----------
# Same filters as the Arlington run, plus a label check: Wikidata sometimes
# types a massacre only as a "battle", so labels and English titles are checked too.
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
massacreLabelPattern <- "massacre"

sparqlTemplate <- 'SELECT ?item ?itemLabel ?coord ?sitelinks ?typeLabel ?enTitle WHERE {
  SERVICE wikibase:box {
    ?item wdt:P625 ?coord .
    bd:serviceParam wikibase:cornerSouthWest "Point(%.4f %.4f)"^^geo:wktLiteral .
    bd:serviceParam wikibase:cornerNorthEast "Point(%.4f %.4f)"^^geo:wktLiteral .
  }
  ?item wikibase:sitelinks ?sitelinks .
  FILTER(?sitelinks >= %d)
  OPTIONAL { ?item wdt:P31 ?type . }
  OPTIONAL { ?article schema:about ?item ; schema:isPartOf <https://en.wikipedia.org/> ; schema:name ?enTitle . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en,fr,es". }
}'

fetchLandmarkTile <- function(lon0, lat0, size, depth = 0) {
  tileFile <- file.path(wdDir, sprintf("wd_%.3f_%.3f_%.3f.rds", lon0, lat0, size))
  if (file.exists(tileFile)) return(readRDS(tileFile))
  query <- sprintf(sparqlTemplate, lon0, lat0, lon0 + size, lat0 + size, landmarkMinSitelinks)
  tmp <- tempfile(fileext = ".json")
  ok <- fetchToFile(paste0("https://query.wikidata.org/sparql?format=json&query=", URLencode(query, reserved = TRUE)), tmp)
  Sys.sleep(1)
  bindings <- NULL
  if (ok) {
    bindings <- tryCatch(jsonlite::fromJSON(tmp)$results$bindings, error = function(e) "parseFailed")
    if (identical(bindings, "parseFailed")) ok <- FALSE  # truncated by a timeout
  }
  if (!ok) {
    if (depth < 2) {
      half <- size / 2
      parts <- lapply(list(c(0, 0), c(half, 0), c(0, half), c(half, half)), function(o)
        fetchLandmarkTile(lon0 + o[1], lat0 + o[2], half, depth + 1))
      if (any(vapply(parts, is.null, logical(1)))) return(NULL)
      result <- do.call(rbind, parts)
      saveRDS(result, tileFile)
      return(result)
    }
    message(sprintf("Failed tile at %.2f, %.2f; rerun to retry", lon0, lat0))
    return(NULL)
  }
  valueOr <- function(col) if (!is.null(bindings[[col]])) bindings[[col]]$value else NA_character_
  result <- if (length(bindings) == 0 || nrow(bindings) == 0) {
    data.frame(item = character(0), label = character(0), coord = character(0),
               sitelinks = numeric(0), type = character(0), enTitle = character(0))
  } else {
    data.frame(item = bindings$item$value, label = bindings$itemLabel$value, coord = bindings$coord$value,
               sitelinks = as.numeric(bindings$sitelinks$value), type = valueOr("typeLabel"),
               enTitle = valueOr("enTitle"))
  }
  saveRDS(result, tileFile)
  result
}

tileGridLL <- expand.grid(lon0 = seq(continentBox[["xmin"]], continentBox[["xmax"]] - 1e-9, by = landmarkTileDeg),
                          lat0 = seq(continentBox[["ymin"]], continentBox[["ymax"]] - 1e-9, by = landmarkTileDeg))
# Skip tiles with no land at all (open ocean)
landBox <- st_crop(st_make_valid(landSf), st_bbox(boxWgs))
tilePolys <- st_sfc(lapply(seq_len(nrow(tileGridLL)), function(i) st_as_sfc(st_bbox(c(
  xmin = tileGridLL$lon0[i], ymin = tileGridLL$lat0[i],
  xmax = tileGridLL$lon0[i] + landmarkTileDeg, ymax = tileGridLL$lat0[i] + landmarkTileDeg)))[[1]]), crs = 4326)
tileGridLL <- tileGridLL[lengths(st_intersects(tilePolys, landBox)) > 0, ]
message(nrow(tileGridLL), " Wikidata tiles to query (cached tiles are skipped)")

landmarkRows <- vector("list", nrow(tileGridLL))
for (i in seq_len(nrow(tileGridLL))) {
  landmarkRows[[i]] <- fetchLandmarkTile(tileGridLL$lon0[i], tileGridLL$lat0[i], landmarkTileDeg)
  if (i %% 25 == 0) message(i, " of ", nrow(tileGridLL), " tiles done")
}
if (any(vapply(landmarkRows, is.null, logical(1)))) stop("Some Wikidata tiles failed; rerun this section to retry them.")
landmarkAll <- do.call(rbind, landmarkRows)

landmarkTypes <- tapply(landmarkAll$type, landmarkAll$item,
                        function(x) paste(sort(unique(na.omit(x))), collapse = "; "))
lmk <- landmarkAll[!duplicated(landmarkAll$item), c("item", "label", "coord", "sitelinks", "enTitle")]
lmk$type <- as.character(landmarkTypes[lmk$item])
coordNums <- regmatches(lmk$coord, regexec("Point\\(([-0-9.eE]+) ([-0-9.eE]+)\\)", lmk$coord))
lmk$lon <- as.numeric(vapply(coordNums, function(m) if (length(m) == 3) m[2] else NA_character_, ""))
lmk$lat <- as.numeric(vapply(coordNums, function(m) if (length(m) == 3) m[3] else NA_character_, ""))
lmk <- lmk[!is.na(lmk$lon) & !grepl("^Q[0-9]+$", lmk$label) & !is.na(lmk$enTitle), ]

isSettlement <- (grepl(settlementPattern, lmk$type, ignore.case = TRUE) &
                 !grepl("ghost town", lmk$type, ignore.case = TRUE)) |
  (grepl("plantation", lmk$type, ignore.case = TRUE) & grepl("Plantation$", lmk$label))
dropIt <- isSettlement |
  grepl(noisePattern, lmk$type, ignore.case = TRUE) |
  grepl(arealPattern, lmk$type, ignore.case = TRUE) |
  grepl(sensitivePattern, lmk$type, ignore.case = TRUE) |
  grepl(massacreLabelPattern, lmk$label, ignore.case = TRUE) |
  grepl(massacreLabelPattern, lmk$enTitle, ignore.case = TRUE) |
  lmk$label %in% dropAnyway
lmk <- lmk[!dropIt | lmk$label %in% keepAnyway, ]

# Keep the most notable few per 50 km cell, so quiet regions still get landmarks
lmkXY <- st_coordinates(st_transform(st_as_sf(lmk, coords = c("lon", "lat"), crs = 4326, remove = FALSE), laeaCrs))
lmk$cell <- paste(floor(lmkXY[, 1] / (landmarkCellKm * 1000)), floor(lmkXY[, 2] / (landmarkCellKm * 1000)))
lmk <- lmk[order(-lmk$sitelinks), ]
lmk <- lmk[ave(seq_len(nrow(lmk)), lmk$cell, FUN = seq_along) <= landmarksPerCell, ]
message(nrow(lmk), " landmarks kept for the Your Journey tab")

# ---- First sentences from English Wikipedia ---------------------------------
# Wikipedia's API returns intro extracts for up to 20 articles per request.
# Batches are cached by position, so if you change the landmark settings above,
# delete the wikipediaBatches folder before rerunning.
cleanSentence <- function(x) {
  x <- gsub("\\s*\\([^()]*(pronounc|listen|/|;)[^()]*\\)", "", x, ignore.case = TRUE)  # drop pronunciation asides
  trimws(gsub("\\s+", " ", x))
}
titles <- unique(lmk$enTitle)
batches <- split(titles, ceiling(seq_along(titles) / 20))
sentences <- character(0)
for (b in seq_along(batches)) {
  batchFile <- file.path(wpDir, sprintf("batch_%05d_%d.rds", b, length(batches[[b]])))
  if (file.exists(batchFile)) {
    got <- readRDS(batchFile)
  } else {
    res <- fetchJson(paste0("https://en.wikipedia.org/w/api.php?action=query&prop=extracts&exintro=1",
                            "&explaintext=1&exsentences=1&redirects=1&format=json&formatversion=2&titles=",
                            paste(vapply(batches[[b]], URLencode, "", reserved = TRUE), collapse = "%7C")))
    Sys.sleep(0.5)
    if (is.null(res)) { message("Wikipedia batch ", b, " failed; rerun to retry"); next }
    # Follow normalized titles and redirects back to the titles we asked for
    finalOf <- setNames(batches[[b]], batches[[b]])
    for (n in res$query$normalized) finalOf[finalOf == n$from] <- n$to
    for (r in res$query$redirects) finalOf[finalOf == r$from] <- r$to
    pages <- res$query$pages
    extractOf <- setNames(vapply(pages, function(p) if (is.null(p$extract)) NA_character_ else p$extract, ""),
                          vapply(pages, function(p) p$title, ""))
    got <- setNames(cleanSentence(extractOf[finalOf]), names(finalOf))
    saveRDS(got, batchFile)
  }
  sentences <- c(sentences, got)
  if (b %% 25 == 0) message(b, " of ", length(batches), " Wikipedia batches done")
}
lmk$sentence <- unname(sentences[lmk$enTitle])
lmk <- lmk[!is.na(lmk$sentence) & nzchar(lmk$sentence), ]

lmkWeb <- data.frame(label = lmk$label, lon = round(lmk$lon, 4), lat = round(lmk$lat, 4),
                     sitelinks = lmk$sitelinks, type = sub(";.*", "", lmk$type),
                     wikiTitle = lmk$enTitle, sentence = lmk$sentence)
jsonlite::write_json(lmkWeb, file.path(webDir, "landmarks.json"), dataframe = "columns")
message(sprintf("%d landmarks with Wikipedia sentences written (%.1f MB)",
                nrow(lmkWeb), file.size(file.path(webDir, "landmarks.json")) / 1e6))

# ---- Terrain tiles for the continent (long, resumable) ---------------------
# Tiles already downloaded for the Arlington run are reused from the same folder.
tiles <- bbox_to_tile_grid(st_bbox(boxWgs), zoom = demZoom)$tiles
tileSfc <- st_sfc(lapply(seq_len(nrow(tiles)), function(i) {
  tb <- unclass(tile_bbox(tiles$x[i], tiles$y[i], demZoom))
  st_as_sfc(st_bbox(c(xmin = tb[["xmin"]], ymin = tb[["ymin"]], xmax = tb[["xmax"]], ymax = tb[["ymax"]]), crs = 3857))[[1]]
}), crs = 3857)
tiles <- tiles[lengths(st_intersects(st_transform(tileSfc, 4326), landBox)) > 0, ]  # skip open ocean
message(nrow(tiles), " terrain tiles needed (already downloaded tiles are skipped)")
tileFiles <- file.path(tileDir, sprintf("%d_%d_%d.tif", demZoom, tiles$x, tiles$y))
for (i in seq_len(nrow(tiles))) {
  if (file.exists(tileFiles[i])) next
  tileUrl <- sprintf("https://s3.amazonaws.com/elevation-tiles-prod/geotiff/%d/%d/%d.tif", demZoom, tiles$x[i], tiles$y[i])
  part <- paste0(tileFiles[i], ".part")
  if (fetchToFile(tileUrl, part)) file.rename(part, tileFiles[i]) else unlink(part)
  if (i %% 100 == 0) message(i, " of ", nrow(tiles), " tiles checked")
}
tileFiles <- tileFiles[file.exists(tileFiles)]
message(length(tileFiles), " tiles available for the mosaic")

demRaw <- vrt(tileFiles)
templateRast <- rast(ext(vect(boxLaea)), resolution = fineRes, crs = laeaCrs)
demFine <- project(demRaw, templateRast, method = "bilinear")

# ---- Water mask -------------------------------------------------------------
boxExt <- ext(continentBox[["xmin"]], continentBox[["xmax"]], continentBox[["ymin"]], continentBox[["ymax"]])
landL <- project(crop(landV, boxExt), laeaCrs)
lakesL <- project(crop(lakesV, boxExt), laeaCrs)
demFine <- mask(demFine, rasterize(landL, demFine))
demFine <- mask(demFine, rasterize(lakesL, demFine), inverse = TRUE)

# ---- Walking pace: slope, land cover, altitude ------------------------------
grade <- tan(terrain(demFine, v = "slope", unit = "radians"))
paceFine <- (1 / tobler(grade) + 1 / tobler(-grade)) / 2 / 1000  # hours per meter
landCoverFriction <- data.frame(
  class = 1:19,
  label = c("Temperate needleleaf forest", "Taiga needleleaf forest",
            "Tropical broadleaf evergreen forest", "Tropical broadleaf deciduous forest",
            "Temperate broadleaf deciduous forest", "Mixed forest",
            "Tropical shrubland", "Temperate shrubland",
            "Tropical grassland", "Temperate grassland",
            "Polar shrub lichen moss", "Polar grass lichen moss", "Polar barren lichen moss",
            "Wetland", "Cropland", "Barren land", "Urban", "Water", "Snow and ice"),
  multiplier = c(1.5, 1.6, 2.0, 1.6, 1.4, 1.5, 1.3, 1.3, 1.1, 1.1,
                 1.4, 1.3, 1.3, 2.5, 1.1, 1.2, 1.0, NA, 2.0)
)
landCover <- project(rast(landCoverPath), demFine, method = "near")
frictionFine <- classify(landCover, cbind(landCoverFriction$class, landCoverFriction$multiplier),
                         others = defaultFriction)
frictionFine <- ifel(is.na(landCover), defaultFriction, frictionFine)
altitudeMultiplier <- 1 + altitudePenaltyPer1000m *
  clamp(demFine - altitudePenaltyStartM, lower = 0, values = TRUE) / 1000
paceFine <- paceFine * frictionFine * altitudeMultiplier

paceCoarse <- aggregate(paceFine, fact = coarseFactor, fun = "mean", na.rm = TRUE)
landFraction <- aggregate(!is.na(paceFine), fact = coarseFactor, fun = "mean")
paceCoarse <- ifel(landFraction < minLandFraction, NA, paceCoarse)

# ---- Rivers as barriers, crossable at bridge cities --------------------------
bridges <- data.frame(
  name = c("Bemidji", "St. Cloud", "Minneapolis", "La Crosse", "Dubuque", "Davenport",
           "Burlington", "Quincy", "St. Louis", "Cape Girardeau", "Memphis", "Greenville",
           "Vicksburg", "Natchez", "Baton Rouge", "New Orleans",
           "Great Falls", "Williston", "Bismarck", "Pierre", "Yankton", "Sioux City",
           "Omaha", "St. Joseph", "Kansas City", "Jefferson City", "St. Charles",
           "Pittsburgh", "Wheeling", "Parkersburg", "Huntington", "Portsmouth", "Cincinnati",
           "Louisville", "Evansville", "Paducah", "Cairo",
           "Thousand Islands", "Ogdensburg", "Cornwall", "Montreal", "Trois-Rivieres", "Quebec City"),
  river = c(rep("Mississippi", 16), rep("Missouri", 11), rep("Ohio", 10), rep("St. Lawrence", 6)),
  lon = c(-94.88, -94.16, -93.26, -91.24, -90.66, -90.58, -91.10, -91.41, -90.18, -89.52,
          -90.05, -91.06, -90.88, -91.40, -91.19, -90.07,
          -111.30, -103.62, -100.78, -100.35, -97.40, -96.40, -95.94, -94.85, -94.58, -92.17, -90.48,
          -80.00, -80.72, -81.56, -82.45, -83.00, -84.51, -85.76, -87.57, -88.60, -89.18,
          -75.98, -75.49, -74.73, -73.57, -72.55, -71.27),
  lat = c(47.47, 45.56, 44.98, 43.80, 42.50, 41.52, 40.81, 39.93, 38.63, 37.31,
          35.15, 33.41, 32.35, 31.56, 30.45, 29.95,
          47.50, 48.15, 46.81, 44.37, 42.87, 42.50, 41.26, 39.77, 39.10, 38.58, 38.79,
          40.44, 40.06, 39.27, 38.42, 38.73, 39.10, 38.25, 37.97, 37.08, 37.00,
          44.35, 44.70, 45.02, 45.50, 46.34, 46.75)
)
barrierPatterns <- c("Mississippi" = "^Mississippi$", "Missouri" = "^Missouri$",
                     "Ohio" = "^Ohio$", "St. Lawrence" = "Lawrence|Laurent")

riversRaw <- vect(ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf"))
riverMatches <- sapply(barrierPatterns, function(pattern) grepl(pattern, riversRaw$name, ignore.case = TRUE))
isBarrier <- rowSums(riverMatches) > 0
riversV <- riversRaw[isBarrier]
riversV$river <- names(barrierPatterns)[apply(riverMatches[isBarrier, , drop = FALSE], 1, which.max)]
riversV <- project(crop(riversV, boxExt), laeaCrs)

bridgesSf <- st_transform(st_as_sf(bridges, coords = c("lon", "lat"), crs = 4326), laeaCrs)
riversSf <- st_as_sf(riversV)
snapXY <- matrix(NA_real_, nrow(bridges), 2); snapDistKm <- rep(NA_real_, nrow(bridges))
for (b in seq_len(nrow(bridges))) {
  ownRiver <- riversSf[riversSf$river == bridges$river[b], ]
  connector <- st_nearest_points(bridgesSf[b, ], st_union(ownRiver))
  snapXY[b, ] <- st_coordinates(connector)[2, 1:2]
  snapDistKm[b] <- as.numeric(st_length(connector)) / 1000
}
keepBridge <- snapDistKm <= 25
crossingPts <- vect(snapXY[keepBridge, , drop = FALSE], crs = laeaCrs)
crossingPts$name <- bridges$name[keepBridge]; crossingPts$river <- bridges$river[keepBridge]

riverBarrier <- rasterize(buffer(riversV, riverBufferM), paceCoarse, touches = TRUE)
crossingZones <- rasterize(buffer(crossingPts, crossingRadiusKm * 1000), paceCoarse, touches = TRUE)
riverBarrier <- mask(riverBarrier, crossingZones, inverse = TRUE)
paceCoarse <- mask(paceCoarse, riverBarrier, inverse = TRUE)

# ---- Pace grid for the browser -----------------------------------------------
# One byte per 5 km cell, log scale, 0 = impassable. The scale is clipped at the
# 99.99th percentile so a stray extreme cell can't waste the byte's precision.
paceVals <- values(paceCoarse, mat = FALSE)
logPace <- log(paceVals)
logMin <- min(logPace, na.rm = TRUE)
logMax <- as.numeric(quantile(logPace, 0.9999, na.rm = TRUE))
scaled <- pmin(pmax((logPace - logMin) / (logMax - logMin), 0), 1)
paceBytes <- ifelse(is.na(logPace), 0L, as.integer(round(scaled * 254)) + 1L)
writeBin(as.raw(paceBytes), file.path(webDir, "paceGrid.bin"))
paceExt <- as.vector(ext(paceCoarse))
jsonlite::write_json(list(
  ncol = ncol(paceCoarse), nrow = nrow(paceCoarse), resM = res(paceCoarse)[1],
  xmin = paceExt[["xmin"]], ymax = paceExt[["ymax"]],
  logPaceMin = logMin, logPaceMax = logMax, paceUnits = "hours per meter",
  projection = list(type = "lambert azimuthal equal area (sphere)", lon0 = -100, lat0 = 45, radius = 6371007.181),
  flatSpeedKmh = flatSpeedKmh
), file.path(webDir, "paceGrid.json"), auto_unbox = TRUE, digits = 10)

# ---- Mountain glyphs, rivers, and bridges for the continent map ---------------
glyphFact <- round(20000 / fineRes)
reliefMax <- aggregate(demFine, fact = glyphFact, fun = "max", na.rm = TRUE)
relief <- reliefMax - aggregate(demFine, fact = glyphFact, fun = "min", na.rm = TRUE)
names(relief) <- "reliefM"
glyphPts <- as.points(relief)
glyphPts <- glyphPts[glyphPts$reliefM >= 400]
if (nrow(glyphPts) > maxWebGlyphs) glyphPts <- glyphPts[order(-glyphPts$reliefM)][seq_len(maxWebGlyphs)]
glyphLL <- crds(project(glyphPts, "EPSG:4326"))
jsonlite::write_json(unname(cbind(round(glyphLL, 3), round(glyphPts$reliefM))),
                     file.path(webDir, "mountainGlyphs.json"))
toWeb(project(simplifyGeom(riversV, tolerance = 2000), "EPSG:4326"), "barrierRivers.geojson")
toWeb(project(crossingPts, "EPSG:4326"), "bridges.geojson")

webFiles <- list.files(webDir, full.names = TRUE, recursive = TRUE)
message(sprintf("Continent export done: %d files, %.1f MB total, in %s",
                length(webFiles), sum(file.size(webFiles)) / 1e6, normalizePath(webDir)))
