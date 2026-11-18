# buildRoads.R
# Builds both hero roads for the Frodo's Journey tab from their locked-in stops:
#   1. Arlington: Bag End at the Data Trust office (Route C, with Linville Falls and Ocala)
#   2. Hobbit Trail, Oregon, to the Hollywood Sign
# Each leg is walked on the continent pace grid (the same walking model), traced
# downhill along its cost surface, and sampled for elevation from the terrain
# tiles already on disk. Takes several minutes per road.

library(terra)
library(sf)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite', type = 'binary')")

outDir <- "frodoOutputs"
contWeb <- file.path(outDir, "continent", "web")
tileDir <- file.path(outDir, "demTiles")
buildDir <- file.path(outDir, "builtRoads")
dir.create(buildDir, recursive = TRUE, showWarnings = FALSE)

milestoneNames <- c("Bucklebury", "Bree", "Weathertop", "Rivendell", "Moria", "Lothlorien",
                    "Amon Hen", "Dead Marshes", "Minas Morgul", "Cirith Ungol", "Mount Doom")
frodoLegKm <- c(115, 100, 170, 350, 550, 195, 625, 255, 280, 35, 190)
gradientStepM <- 2500

# ---- The two roads: every stop in order (step 0 is Bag End; detours share
# the step of the ring landmark they lead to) -------------------------------
stopRow <- function(step, role, label, lon, lat) data.frame(step = step, role = role, label = label, lon = lon, lat = lat)
roads <- list(
  list(chainId = 1, direction = "Arlington", stops = rbind(
    stopRow(0, "start", "Bag End", -77.0846, 38.8888),
    stopRow(1, "detour", "Space Shuttle Discovery", -77.4452, 38.9114),
    stopRow(1, "ring", "Battle of Harpers Ferry", -77.7302, 39.3228),
    stopRow(2, "ring", "Clarks Knob", -77.7506, 40.0478),
    stopRow(3, "ring", "Laurel Hill", -79.1644, 40.1608),
    stopRow(4, "detour", "Wheeling Suspension Bridge", -80.7273, 40.0702),
    stopRow(4, "ring", "Hocking Hills State Park", -82.5389, 39.4306),
    stopRow(5, "ring", "Linville Falls", -81.9268, 35.9496),
    stopRow(6, "ring", "Kuwohi", -83.4986, 35.5628),
    stopRow(7, "detour", "Springer Mountain", -84.1936, 34.6267),
    stopRow(7, "ring", "Lake Blackshear", -83.942, 31.848),
    stopRow(8, "ring", "Okefenokee Swamp", -82.1397, 30.7381),
    stopRow(9, "detour", "Florida Museum of Natural History", -82.37, 29.6361),
    stopRow(9, "ring", "Manatee Springs State Park", -82.9771, 29.4904),
    stopRow(10, "ring", "Lower Suwannee National Wildlife Refuge", -83.1, 29.3167),
    stopRow(11, "ring", "Ocala National Forest", -81.8217, 29.1736)
  )),
  list(chainId = 2, direction = "Hobbit Trail", stops = rbind(
    stopRow(0, "start", "Hobbit Trail", -124.105, 44.17),
    stopRow(1, "detour", "Siuslaw National Forest", -123.917, 44.3333),
    stopRow(1, "ring", "Tahkenitch Lake", -124.1323, 43.7791),
    stopRow(2, "ring", "Charles and Anna Drain House", -123.311, 43.6598),
    stopRow(3, "ring", "Rogue River Ranch", -123.882, 42.7186),
    stopRow(4, "detour", "Buncom, Oregon", -122.998, 42.1739),
    stopRow(4, "ring", "Klamath National Forest", -123.3333, 41.5003),
    stopRow(5, "detour", "Mount Eddy", -122.4792, 41.32),
    stopRow(5, "ring", "Vya, Nevada", -119.8606, 41.5922),
    stopRow(6, "ring", "Black Rock Desert", -119.0639, 40.8831),
    stopRow(7, "detour", "Bodie, California", -119.0122, 38.2122),
    stopRow(7, "ring", "Yosemite Falls", -119.5967, 37.7566),
    stopRow(8, "ring", "New Idria, California", -120.6733, 36.4169),
    stopRow(9, "ring", "Los Padres National Forest", -119.751, 34.6667),
    stopRow(10, "ring", "Santa Ynez Valley", -119.858, 34.5473),
    stopRow(11, "ring", "Hollywood Sign", -118.3217, 34.1341)
  ))
)

# ---- The continent pace grid, decoded from the web export ------------------
meta <- jsonlite::fromJSON(file.path(contWeb, "paceGrid.json"))
bytes <- as.integer(readBin(file.path(contWeb, "paceGrid.bin"), "raw", n = meta$ncol * meta$nrow))
paceVals <- ifelse(bytes == 0, NA, exp(meta$logPaceMin + (bytes - 1) / 254 * (meta$logPaceMax - meta$logPaceMin)))
laeaCrs <- sprintf("+proj=laea +lat_0=%s +lon_0=%s +R=%s +units=m +no_defs",
                   meta$projection$lat0, meta$projection$lon0, meta$projection$radius)
paceGrid <- rast(nrows = meta$nrow, ncols = meta$ncol, crs = laeaCrs,
                 extent = ext(meta$xmin, meta$xmin + meta$ncol * meta$resM, meta$ymax - meta$nrow * meta$resM, meta$ymax))
values(paceGrid) <- paceVals
flatSpeedKmh <- meta$flatSpeedKmh

# ---- Walking one leg: cost surface from its start, then downhill to its end ---
valueAt <- function(r, xy) { v <- extract(r, matrix(xy, ncol = 2)); v[1, ncol(v)] }
surfaceFrom <- function(pc, fromXY) {
  pc <- pc * 1
  cell <- cellFromXY(pc, matrix(fromXY, ncol = 2))
  if (is.na(pc[cell][[1]])) {  # a landmark on a lake shore or river: start from the nearest walkable cell
    near <- adjacent(pc, cell, directions = 16)[1, ]
    near <- near[!is.na(near) & !is.na(values(pc, mat = FALSE)[near])]
    if (length(near) == 0) stop("A stop sits on an unwalkable cell.")
    cell <- near[1]
  }
  pc[cell] <- 0
  raw <- costDist(pc, target = 0, maxiter = 100)
  dx <- abs(init(raw, "x") - fromXY[1]); dy <- abs(init(raw, "y") - fromXY[2])
  octile <- max(dx, dy) + (sqrt(2) - 1) * min(dx, dy)
  list(raw = raw, hours = raw * ifel(octile > 0, sqrt(dx^2 + dy^2) / octile, 1))
}
traceDescent <- function(startXY, goalXY, rawR, hoursR) {
  rawVals <- values(rawR, mat = FALSE)
  aspectR <- terrain(hoursR, v = "aspect", unit = "radians")
  pos <- as.numeric(startXY); hoursNow <- valueAt(hoursR, pos)
  pathXY <- matrix(pos, ncol = 2); bestHours <- hoursNow; stall <- 0
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
                           min(fromXY[2], toXY[2]) - marginM, max(fromXY[2], toXY[2]) + marginM), ext(paceGrid))
  surf <- surfaceFrom(crop(paceGrid, cropExt), fromXY)
  if (is.na(valueAt(surf$hours, toXY))) surf <- surfaceFrom(paceGrid, fromXY)
  path <- traceDescent(toXY, fromXY, surf$raw, surf$hours)
  list(pathXY = path[nrow(path):1, , drop = FALSE], effortKm = valueAt(surf$hours, toXY) * flatSpeedKmh)
}


# Coastal and lakeshore stops can fall on cells the model treats as water;
# move each to the nearest walkable cell (within about 25 km) before walking
paceValues <- values(paceGrid, mat = FALSE)
snapToWalkable <- function(xy) {
  cell <- cellFromXY(paceGrid, matrix(xy, ncol = 2))
  if (!is.na(paceValues[cell])) return(xy)
  rc <- rowColFromCell(paceGrid, cell)
  for (radius in 1:5) {
    rows <- (rc[1] - radius):(rc[1] + radius); cols <- (rc[2] - radius):(rc[2] + radius)
    grid <- expand.grid(r = rows[rows >= 1 & rows <= nrow(paceGrid)], c = cols[cols >= 1 & cols <= ncol(paceGrid)])
    cells <- cellFromRowCol(paceGrid, grid$r, grid$c)
    cells <- cells[!is.na(paceValues[cells])]
    if (length(cells) > 0) {
      centers <- xyFromCell(paceGrid, cells)
      return(as.numeric(centers[which.min((centers[, 1] - xy[1])^2 + (centers[, 2] - xy[2])^2), ]))
    }
  }
  stop("No walkable cell near a stop at ", paste(round(xy), collapse = ", "))
}

demRaw <- vrt(list.files(tileDir, pattern = "\\.tif$", full.names = TRUE))
allProfiles <- list(); allLegs <- list(); allStops <- list()

for (road in roads) {
  message("==== Building the ", road$direction, " road ====")
  stops <- road$stops
  xy <- st_coordinates(st_transform(st_as_sf(stops, coords = c("lon", "lat"), crs = 4326), laeaCrs))
  walkXY <- t(apply(xy, 1, snapToWalkable))
  segments <- list(); effort <- numeric(0)
  for (s in 2:nrow(stops)) {
    message(sprintf("Leg %d of %d: %s to %s", s - 1, nrow(stops) - 1, stops$label[s - 1], stops$label[s]))
    seg <- buildSegment(walkXY[s - 1, ], walkXY[s, ], frodoLegKm[stops$step[s]])
    segments[[s - 1]] <- if (s == 2) seg$pathXY else seg$pathXY[-1, , drop = FALSE]
    effort[s - 1] <- seg$effortKm
  }
  legs <- aggregate(effort, by = list(step = stops$step[-1]), FUN = sum)
  names(legs)[2] <- "trueLegKm"
  legs$ring <- milestoneNames[legs$step]
  legs$targetLegKm <- frodoLegKm[legs$step]
  legs$trueErrorKm <- round(legs$trueLegKm - legs$targetLegKm)
  legs$trueLegKm <- round(legs$trueLegKm)
  legs$chainId <- road$chainId; legs$direction <- road$direction
  print(legs[, c("ring", "targetLegKm", "trueLegKm", "trueErrorKm")], row.names = FALSE)
  message(sprintf("Total: %d km of walking effort, against Frodo's 2,865 km", sum(legs$trueLegKm)))

  pathLine <- vect(do.call(rbind, segments), type = "lines", crs = laeaCrs)
  densePts <- as.points(densify(pathLine, interval = 2000))
  denseXY <- crds(densePts)
  profile <- data.frame(distKm = round(cumsum(c(0, sqrt(rowSums(diff(denseXY)^2)))) / 1000, 1),
                        elevM = round(extract(demRaw, project(densePts, crs(demRaw)))[, 2]))
  profile$elevM[!is.na(profile$elevM) & profile$elevM < 0] <- 0
  lonLat <- crds(project(densePts, "EPSG:4326"))
  profile$lon <- round(lonLat[, 1], 4); profile$lat <- round(lonLat[, 2], 4)
  profile$stop <- NA_character_
  for (s in seq_len(nrow(stops))) {
    profile$stop[which.min((denseXY[, 1] - walkXY[s, 1])^2 + (denseXY[, 2] - walkXY[s, 2])^2)] <- stops$label[s]
  }
  profile$chainId <- road$chainId
  allProfiles[[length(allProfiles) + 1]] <- profile
  allLegs[[length(allLegs) + 1]] <- legs
  allStops[[length(allStops) + 1]] <- data.frame(stops[, c("label", "role", "step", "lon", "lat")],
                                                 ring = ifelse(stops$role == "ring", milestoneNames[pmax(stops$step, 1)], NA),
                                                 chainId = road$chainId)
}

jsonlite::write_json(do.call(rbind, allProfiles), file.path(buildDir, "chainProfiles.json"), dataframe = "columns", na = "null")
jsonlite::write_json(do.call(rbind, allLegs), file.path(buildDir, "chainLegs.json"), dataframe = "rows")
stopsOut <- st_as_sf(do.call(rbind, allStops), coords = c("lon", "lat"), crs = 4326)
st_write(stopsOut, file.path(buildDir, "chainStops.geojson"), delete_dsn = TRUE, quiet = TRUE)
message("Both roads written to ", normalizePath(buildDir), ". Zip that folder and upload it.")
