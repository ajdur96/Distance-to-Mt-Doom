# fetchCrossings.R
# One-time prep: find every real bridge and ferry over the barrier rivers in
# OpenStreetMap, so river gaps open where people can actually cross instead of at
# hand-placed city points. Writes frodoOutputs/crossings/osmCrossings.csv, which
# loadCrossings() in modelParams.R picks up automatically on the next run.
#
# How it works:
#   1. Walk along every barrier river and list the 1-degree tiles it passes through.
#   2. Ask the Overpass API, tile by tile, for bridges carrying a road or path and
#      for ferry routes, at least minCrossingLengthM long. Each tile is cached, so
#      rerunning after a failure only fetches what's missing.
#   3. Keep only ways that actually cross a barrier river (findRiverCrossings),
#      and merge twin spans and close neighbors (mergeCrossings).
# Hand-listed bridges with no OSM crossing nearby are still used (mostly roads
# over dams, which OSM rarely tags as bridges); the main scripts report them.
#
# Data (c) OpenStreetMap contributors, ODbL. Credit OSM in the methods text.

library(terra)
library(sf)
library(rnaturalearth)
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
sf_use_s2(FALSE)
source(Find(file.exists, c("modelParams.R", file.path("r", "modelParams.R"))))

tileDeg <- 1        # tile size in degrees
tilePadDeg <- 0.05  # overlap, so bridges on a tile edge are never missed
tileDir <- file.path(dirname(crossingsPath), "osmTiles")
dir.create(tileDir, recursive = TRUE, showWarnings = FALSE)
mirrors <- c("https://overpass-api.de/api/interpreter",
             "https://overpass.kumi.systems/api/interpreter",
             "https://overpass.private.coffee/api/interpreter",
             "https://maps.mail.ru/osm/tools/overpass/api/interpreter")
# wininet works on the Windows network where curl does not (as for the DEM tiles)
downloadMethod <- if (.Platform$OS.type == "windows") "wininet" else "libcurl"
skipHighway <- c("proposed", "construction", "abandoned", "disused", "razed", "platform")

# ---- 1. Tiles along the rivers -------------------------------------------------
rivers <- loadBarrierRivers()
riverLines <- suppressWarnings(st_cast(st_cast(st_transform(st_as_sf(rivers), crossingCrs),
                                               "MULTILINESTRING"), "LINESTRING"))
samples <- st_coordinates(st_transform(st_line_sample(riverLines, density = 1 / 2000), 4326))
tiles <- unique(data.frame(w = floor(samples[, "X"] / tileDeg) * tileDeg,
                           s = floor(samples[, "Y"] / tileDeg) * tileDeg))
tiles$file <- file.path(tileDir, sprintf("tile_%g_%g.json", tiles$w, tiles$s))
message(nrow(tiles), " tiles along the barrier rivers; ", sum(file.exists(tiles$file)), " already cached")

# ---- 2. Overpass, one tile at a time -------------------------------------------
overpassQuery <- function(w, s) {
  bb <- sprintf("%.3f,%.3f,%.3f,%.3f", s - tilePadDeg, w - tilePadDeg,
                s + tileDeg + tilePadDeg, w + tileDeg + tilePadDeg)
  minLen <- minCrossingLengthM
  paste0('[out:json][timeout:300];(',
         'way["highway"]["bridge"]["bridge"!="no"](', bb, ')(if: length() > ', minLen, ');',
         'way["route"="ferry"](', bb, ')(if: length() > ', minLen, ');',
         ');out tags geom;')
}
readTile <- function(path) {
  j <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j) || is.null(j$elements)) return(NULL)
  if (!is.null(j$remark) && grepl("error", j$remark, ignore.case = TRUE)) return(NULL)  # timed out partway
  j
}
fetchTile <- function(w, s, dest) {
  url <- function(m) paste0(m, "?data=", URLencode(overpassQuery(w, s), reserved = TRUE))
  for (attempt in 1:3) {
    for (m in mirrors) {
      tmp <- paste0(dest, ".part")
      ok <- tryCatch({
        suppressWarnings(download.file(url(m), tmp, mode = "wb", method = downloadMethod, quiet = TRUE))
        TRUE
      }, error = function(e) FALSE)
      if (ok && !is.null(readTile(tmp))) return(file.rename(tmp, dest))
      unlink(tmp)
    }
    Sys.sleep(20 * attempt)  # every mirror busy: back off, then try them all again
  }
  FALSE
}
todo <- which(!file.exists(tiles$file))
for (k in seq_along(todo)) {
  i <- todo[k]
  ok <- fetchTile(tiles$w[i], tiles$s[i], tiles$file[i])
  if (!ok) message(sprintf("Failed tile %g, %g (rerun the script to retry)", tiles$w[i], tiles$s[i]))
  if (k %% 10 == 0) message(k, " of ", length(todo), " tiles fetched")
  Sys.sleep(1)  # be polite to the public servers
}
missing <- !file.exists(tiles$file)
if (any(missing)) {
  stop(sum(missing), " tiles could not be fetched. Rerun to retry; cached tiles are kept.")
}

# ---- 3. Ways that cross a barrier river -----------------------------------------
ways <- list()
for (f in tiles$file) {
  for (el in readTile(f)$elements) {
    if (el$type != "way" || length(el$geometry) < 2) next
    id <- as.character(el$id)
    if (!is.null(ways[[id]])) next  # the same way from a neighboring tile
    tg <- el$tags
    if (!is.null(tg$highway) && tg$highway %in% skipHighway) next
    xy <- do.call(rbind, lapply(el$geometry, function(p) c(p$lon, p$lat)))
    ways[[id]] <- list(osmId = paste0("way/", id),
                       name = if (!is.null(tg$name)) tg$name else if (!is.null(tg$ref)) tg$ref else NA_character_,
                       type = if (identical(tg$route, "ferry")) "ferry" else "bridge",
                       geom = st_linestring(xy))
  }
}
waysSf <- st_sf(osmId = vapply(ways, `[[`, "", "osmId"),
                name = vapply(ways, function(w) as.character(w$name), ""),
                type = vapply(ways, `[[`, "", "type"),
                geometry = st_sfc(lapply(ways, `[[`, "geom"), crs = 4326))
message(sprintf("%d bridge ways and %d ferry ways at least %d m long near the rivers",
                sum(waysSf$type == "bridge"), sum(waysSf$type == "ferry"), minCrossingLengthM))

found <- rbind(findRiverCrossings(waysSf[waysSf$type == "bridge", ], rivers, "bridge"),
               findRiverCrossings(waysSf[waysSf$type == "ferry", ], rivers, "ferry"))
crossings <- mergeCrossings(found)
write.csv(crossings, crossingsPath, row.names = FALSE)
st_write(st_as_sf(crossings, coords = c("lon", "lat"), crs = 4326),
         sub("\\.csv$", ".geojson", crossingsPath), delete_dsn = TRUE, quiet = TRUE)
# Every way that was judged to cross, for checking in QGIS against imagery
crossed <- waysSf[waysSf$osmId %in% found$osmId, ]
st_write(crossed, file.path(dirname(crossingsPath), "crossingWays.geojson"), delete_dsn = TRUE, quiet = TRUE)

# ---- Summary: OSM crossings next to the hand list ---------------------------------
rivs <- sort(unique(c(crossings$river, bridges$river)))
summary <- data.frame(river = rivs,
  osmBridges = vapply(rivs, function(r) sum(crossings$river == r & crossings$type == "bridge"), 0),
  osmFerries = vapply(rivs, function(r) sum(crossings$river == r & crossings$type == "ferry"), 0),
  handList = vapply(rivs, function(r) sum(bridges$river == r), 0))
print(summary, row.names = FALSE)
cat(sprintf("Wrote %d crossings (%d bridges, %d ferries) to %s\n", nrow(crossings),
            sum(crossings$type == "bridge"), sum(crossings$type == "ferry"), crossingsPath))
cat("Check crossingWays.geojson in QGIS for anything that isn't really a river crossing.\n")
