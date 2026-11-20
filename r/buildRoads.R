# buildRoads.R
# Builds both hero roads for the Frodo's Journey tab from their locked-in stops:
#   1. Arlington: Bag End at the Data Trust office (Route C, with Linville Falls and Ocala)
#   2. Hobbit Trail, Oregon, to the Hollywood Sign
# Each leg is walked on the continent pace grid (the same walking model), traced
# downhill along its cost surface, and sampled for elevation from the terrain
# tiles already on disk. Takes several minutes per road.
#
# The Lothlorien-to-Amon Hen leg is a ride, not a walk: the Fellowship went down
# the Anduin by boat. Our walker gets a car ride instead, held to the same kind of
# limits a river sets (see "The ride" below).

library(terra)
library(sf)
library(rnaturalearth)
sf_use_s2(FALSE)
source(Find(file.exists, c("modelParams.R", file.path("r", "modelParams.R"))))  # Middle-earth distances, scale
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite', type = 'binary')")

outDir <- "frodoOutputs"
contWeb <- file.path(outDir, "continent", "web")
tileDir <- file.path(outDir, "demTiles")
buildDir <- file.path(outDir, "builtRoads")
dir.create(buildDir, recursive = TRUE, showWarnings = FALSE)

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

# ---- The ride: Lothlorien to Amon Hen -----------------------------------------------
# The Anduin set three limits on the Fellowship, and the ride keeps all three:
#   - Main channel only. The ride stays on major highways (Natural Earth's major
#     highways, beltways and bypasses, plus US federal routes); no back roads.
#   - Downstream only. Along the way the route may never come back toward the
#     Lothlorien stop by more than awaySlackKm, nor back toward home by more than
#     homeSlackKm (straight-line distances, against the farthest point reached so
#     far). This rules out doubling back while leaving highways free to wiggle.
#   - Same distance. The ride covers Frodo's river distance (625 km) within
#     rideToleranceFrac, in real road kilometres, not walking effort.
# The ride must end inside the Amon Hen ring (within ringBandFrac of its hours
# from home). Short walks on and off the highway (up to rampMaxKm each) are
# walked like any other leg. The Fellowship walked down to the boats too.
# Every route that fits is listed in rideCandidates_<road>.geojson, with the
# landmarks near each end, for picking the Amon Hen stop.
rideStep <- 7
rideToleranceFrac <- 0.05
awaySlackKm <- 5      # adjust if too few routes fit; the run reports how much slack routes use
homeSlackKm <- 20
ringBandFrac <- 0.05
rampMaxKm <- 30
rideBinKm <- 0.5      # route lengths are tracked in 0.5 km steps
ringHoursFor <- function(step) meGisRawMi[step] * middleEarthScale * 1.609344 / flatSpeedKmh

loadHighways <- function() {
  ne <- ne_download(scale = 10, type = "roads", category = "cultural", returnclass = "sf")
  keep <- ne$continent %in% "North America" & !(ne$type %in% "Ferry Route") &
    (ne$type %in% c("Major Highway", "Beltway", "Bypass") | ne$level %in% "Federal")
  hw <- st_transform(suppressWarnings(st_cast(st_cast(ne[keep, c("name", "type", "level")], "MULTILINESTRING"), "LINESTRING")), laeaCrs)
  # Split the lines wherever highways meet, so every junction is a node. (This
  # also joins highways that only cross on a bridge; at this scale that's rare.)
  noded <- st_cast(st_line_merge(st_union(st_geometry(hw))), "LINESTRING")
  noded <- st_cast(st_union(noded), "LINESTRING")
  mids <- suppressWarnings(st_point_on_surface(noded))
  nameOf <- hw$name[st_nearest_feature(mids, hw)]
  ends <- t(vapply(noded, function(g) { xy <- st_coordinates(g); c(xy[1, 1:2], xy[nrow(xy), 1:2]) }, numeric(4)))
  key <- function(x, y) paste(round(x / 250), round(y / 250))
  k1 <- key(ends[, 1], ends[, 2]); k2 <- key(ends[, 3], ends[, 4])
  nodeKeys <- unique(c(k1, k2))
  nodeXY <- rbind(ends[, 1:2], ends[, 3:4])[match(nodeKeys, c(k1, k2)), ]
  list(edges = data.frame(from = match(k1, nodeKeys), to = match(k2, nodeKeys),
                          lenKm = as.numeric(st_length(noded)) / 1000, route = nameOf),
       coords = lapply(noded, function(g) st_coordinates(g)[, 1:2, drop = FALSE]),
       nodeXY = nodeXY, geom = noded)
}

# Every highway route from near fromXY that obeys the three limits and ends in the ring
findRides <- function(hw, fromXY, homeXY, hoursHome, ringHours, targetKm) {
  distKm <- function(xy, p) sqrt((xy[, 1] - p[1])^2 + (xy[, 2] - p[2])^2) / 1000
  reachKm <- targetKm * (1 + rideToleranceFrac) + rampMaxKm
  nodeIn <- distKm(hw$nodeXY, fromXY) <= reachKm
  e <- hw$edges; e$id <- seq_len(nrow(e))
  e <- e[nodeIn[e$from] | nodeIn[e$to], ]
  # A direction is allowed if, along the whole edge, it never comes back more than the slack
  okDir <- function(v) {
    a <- distKm(v, fromXY); h <- distKm(v, homeXY)
    all(a >= a[1] - awaySlackKm) && all(h >= h[1] - homeSlackKm)
  }
  fwd <- vapply(e$id, function(i) okDir(hw$coords[[i]]), logical(1))
  bwd <- vapply(e$id, function(i) okDir(hw$coords[[i]][nrow(hw$coords[[i]]):1, , drop = FALSE]), logical(1))
  d <- rbind(data.frame(from = e$from[fwd], to = e$to[fwd], id = e$id[fwd], lenKm = e$lenKm[fwd], rev = FALSE),
             data.frame(from = e$to[bwd], to = e$from[bwd], id = e$id[bwd], lenKm = e$lenKm[bwd], rev = TRUE))
  d$bin <- pmax(1L, as.integer(round(d$lenKm / rideBinKm)))
  nodes <- sort(unique(c(d$from, d$to)))
  d$f <- match(d$from, nodes); d$t <- match(d$to, nodes)
  maxBin <- as.integer(ceiling(targetKm * (1 + rideToleranceFrac) / rideBinKm))
  minBin <- as.integer(floor(targetKm * (1 - rideToleranceFrac) / rideBinKm))
  reach <- matrix(FALSE, length(nodes), maxBin + 1)
  pred <- matrix(0L, length(nodes), maxBin + 1)
  starts <- which(distKm(hw$nodeXY[nodes, , drop = FALSE], fromXY) <= rampMaxKm)
  if (!length(starts)) return(NULL)
  reach[starts, 1] <- TRUE
  outOf <- split(seq_len(nrow(d)), factor(d$f, levels = seq_along(nodes)))
  for (b in 0:(maxBin - 1)) {
    active <- which(reach[, b + 1])
    if (!length(active)) next
    k <- unlist(outOf[active], use.names = FALSE)
    nb <- b + d$bin[k]
    k <- k[nb <= maxBin]; nb <- nb[nb <= maxBin]
    cell <- cbind(d$t[k], nb + 1)
    fresh <- !reach[cell] & !duplicated(cell)
    reach[cell[fresh, , drop = FALSE]] <- TRUE
    pred[cell[fresh, , drop = FALSE]] <- k[fresh]
  }
  # Ends: a node reached at a length inside the tolerance, inside the ring band
  inRange <- reach[, (minBin:maxBin) + 1, drop = FALSE]
  endNodes <- which(rowSums(inRange) > 0)
  if (!length(endNodes)) return(NULL)
  ringErr <- valueAtMany(hoursHome, hw$nodeXY[nodes[endNodes], , drop = FALSE]) / ringHours - 1
  endNodes <- endNodes[!is.na(ringErr) & abs(ringErr) <= ringBandFrac]
  rides <- list()
  for (n in endNodes) {
    bins <- (minBin:maxBin)[inRange[n, ]]
    b <- bins[which.min(abs(bins * rideBinKm - targetKm))]
    steps <- integer(0); node <- n
    while (pred[node, b + 1] > 0) {
      k <- pred[node, b + 1]; steps <- c(k, steps); b <- b - d$bin[k]; node <- d$f[k]
    }
    if (b != 0) next
    pieces <- lapply(steps, function(k) { v <- hw$coords[[d$id[k]]]; if (d$rev[k]) v[nrow(v):1, , drop = FALSE] else v })
    xy <- do.call(rbind, pieces)
    a <- distKm(xy, fromXY); h <- distKm(xy, homeXY)
    backAway <- max(cummax(a) - a); backHome <- max(cummax(h) - h)
    if (backAway > awaySlackKm || backHome > homeSlackKm) next  # slack used up across several edges
    routes <- rle(hw$edges$route[d$id[steps]])$values
    rides[[length(rides) + 1]] <- list(
      xy = xy, entryXY = xy[1, ], exitXY = xy[nrow(xy), ],
      rideKm = sum(d$lenKm[steps]), backAwayKm = backAway, backHomeKm = backHome,
      ringErrPct = 100 * (valueAtMany(hoursHome, xy[nrow(xy), , drop = FALSE]) / ringHours - 1),
      routes = paste(routes[!is.na(routes)], collapse = " > "))
  }
  rides
}
valueAtMany <- function(r, xy) { v <- extract(r, xy); v[, ncol(v)] }

writeRideCandidates <- function(rides, roadName, landmarks) {
  ends <- do.call(rbind, lapply(seq_along(rides), function(i) with(rides[[i]], data.frame(
    rideId = i, x = exitXY[1], y = exitXY[2], rideKm = round(rideKm), ringErrPct = round(ringErrPct, 1),
    backAwayKm = round(backAwayKm, 1), backHomeKm = round(backHomeKm, 1), routes = routes))))
  endsSf <- st_as_sf(ends, coords = c("x", "y"), crs = laeaCrs)
  lmk <- st_transform(st_as_sf(landmarks, coords = c("lon", "lat"), crs = 4326), laeaCrs)
  near <- st_is_within_distance(lmk, endsSf, rampMaxKm * 1000)
  hit <- which(lengths(near) > 0)
  picks <- do.call(rbind, lapply(hit, function(i) {
    j <- near[[i]][which.min(as.numeric(st_distance(lmk[i, ], endsSf[near[[i]], ])))]
    data.frame(landmark = lmk$label[i], sitelinks = lmk$sitelinks[i],
               offRampKm = round(as.numeric(st_distance(lmk[i, ], endsSf[j, ])) / 1000, 1), ends[j, ],
               lon = st_coordinates(st_transform(lmk[i, ], 4326))[1], lat = st_coordinates(st_transform(lmk[i, ], 4326))[2])
  }))
  file <- file.path(buildDir, sprintf("rideCandidates_%s.geojson", gsub("[^A-Za-z]", "", roadName)))
  if (!is.null(picks) && nrow(picks)) {
    picks <- picks[order(-picks$sitelinks), ]
    st_write(st_as_sf(picks, coords = c("lon", "lat"), crs = 4326), file, delete_dsn = TRUE, quiet = TRUE)
    message(sprintf("%d landmarks lie within %d km of a ride's end; best known:", nrow(picks), rampMaxKm))
    print(head(picks[, c("landmark", "offRampKm", "rideKm", "ringErrPct", "routes")], 10), row.names = FALSE)
  } else message("No landmarks lie near any ride's end.")
  message(sprintf("Slack used: up to %.1f of %d km back toward Lothlorien, %.1f of %d km back toward home; %d of %d routes use over 80%% of a slack.",
                  max(ends$backAwayKm), awaySlackKm, max(ends$backHomeKm), homeSlackKm,
                  sum(ends$backAwayKm > 0.8 * awaySlackKm | ends$backHomeKm > 0.8 * homeSlackKm), nrow(ends)))
}

demRaw <- vrt(list.files(tileDir, pattern = "\\.tif$", full.names = TRUE))
allProfiles <- list(); allLegs <- list(); allStops <- list()

highways <- NULL
landmarks <- as.data.frame(jsonlite::fromJSON(file.path(contWeb, "landmarks.json")))

for (road in roads) {
  message("==== Building the ", road$direction, " road ====")
  stops <- road$stops
  midRide <- stops$step == rideStep & stops$role == "detour"
  if (any(midRide)) {
    message("Dropping detours during the ride (no stops on the Anduin): ", paste(stops$label[midRide], collapse = ", "))
    stops <- stops[!midRide, ]
  }
  xy <- st_coordinates(st_transform(st_as_sf(stops, coords = c("lon", "lat"), crs = 4326), laeaCrs))
  walkXY <- t(apply(xy, 1, snapToWalkable))
  segments <- list(); modes <- character(0); effort <- numeric(0); rideKm <- numeric(0)
  addPiece <- function(pathXY, mode) {
    segments[[length(segments) + 1]] <<- if (length(segments)) pathXY[-1, , drop = FALSE] else pathXY
    modes[length(modes) + 1] <<- mode
  }
  for (s in 2:nrow(stops)) {
    message(sprintf("Leg %d of %d: %s to %s", s - 1, nrow(stops) - 1, stops$label[s - 1], stops$label[s]))
    if (stops$step[s] == rideStep) {
      if (is.null(highways)) highways <- loadHighways()
      hoursHome <- surfaceFrom(paceGrid, walkXY[1, ])$hours
      rides <- findRides(highways, walkXY[s - 1, ], walkXY[1, ], hoursHome, ringHoursFor(rideStep), frodoLegKm[rideStep])
      if (length(rides)) {
        message(sprintf("%d highway rides fit the rules.", length(rides)))
        writeRideCandidates(rides, road$direction, landmarks)
        gap <- vapply(rides, function(r) sqrt(sum((r$exitXY - walkXY[s, ])^2)) / 1000, numeric(1))
      }
      if (length(rides) && min(gap) <= rampMaxKm) {
        r <- rides[[which.min(gap)]]
        on <- buildSegment(walkXY[s - 1, ], snapToWalkable(r$entryXY), 0)   # walk down to the highway
        off <- buildSegment(snapToWalkable(r$exitXY), walkXY[s, ], 0)      # and from it to Amon Hen
        addPiece(on$pathXY, "walk")
        addPiece(rbind(on$pathXY[nrow(on$pathXY), ], r$xy), "ride")
        addPiece(rbind(r$exitXY, off$pathXY), "walk")
        effort[s - 1] <- on$effortKm + off$effortKm
        rideKm[s - 1] <- r$rideKm
        message(sprintf("Ride: %.0f km on %s, ending %.1f%% off the ring; slack used %.1f km (Lothlorien), %.1f km (home)",
                        r$rideKm, r$routes, r$ringErrPct, r$backAwayKm, r$backHomeKm))
        next
      }
      warning("No ride fits the rules and ends within ", rampMaxKm, " km of ", stops$label[s],
              ". Walking this leg instead; pick a new Amon Hen stop from the ride candidates.", call. = FALSE)
    }
    seg <- buildSegment(walkXY[s - 1, ], walkXY[s, ], frodoLegKm[stops$step[s]])
    addPiece(seg$pathXY, "walk")
    effort[s - 1] <- seg$effortKm
    rideKm[s - 1] <- 0
  }
  legs <- aggregate(data.frame(walkKm = effort, rideKm = rideKm), by = list(step = stops$step[-1]), FUN = sum)
  legs$mode <- ifelse(legs$rideKm > 0, "ride", "walk")
  # A ride leg is matched on road km; its short walks on and off are reported apart
  legs$trueLegKm <- ifelse(legs$mode == "ride", legs$rideKm, legs$walkKm)
  legs$rampWalkKm <- ifelse(legs$mode == "ride", round(legs$walkKm), 0)
  legs$ring <- milestoneNames[legs$step]
  legs$targetLegKm <- frodoLegKm[legs$step]
  legs$trueErrorKm <- round(legs$trueLegKm - legs$targetLegKm)
  legs$trueLegKm <- round(legs$trueLegKm)
  legs$chainId <- road$chainId; legs$direction <- road$direction
  legs$walkKm <- NULL; legs$rideKm <- NULL
  print(legs[, c("ring", "mode", "targetLegKm", "trueLegKm", "trueErrorKm", "rampWalkKm")], row.names = FALSE)
  message(sprintf("Total: %d km of walking effort and %d km ridden, against Frodo's %d km walked and %d km by boat",
                  sum(legs$trueLegKm[legs$mode == "walk"]) + sum(legs$rampWalkKm), sum(legs$trueLegKm[legs$mode == "ride"]),
                  sum(frodoLegKm[-rideStep]), frodoLegKm[rideStep]))

  # Sample every piece at 2 km, keeping whether it was walked or ridden
  pieces <- lapply(seq_along(segments), function(i) {
    pts <- as.points(densify(vect(segments[[i]], type = "lines", crs = laeaCrs), interval = 2000))
    list(pts = pts, mode = rep(modes[i], nrow(pts)))
  })
  densePts <- do.call(rbind, lapply(pieces, `[[`, "pts"))
  denseXY <- crds(densePts)
  profile <- data.frame(distKm = round(cumsum(c(0, sqrt(rowSums(diff(denseXY)^2)))) / 1000, 1),
                        elevM = round(extract(demRaw, project(densePts, crs(demRaw)))[, 2]),
                        mode = unlist(lapply(pieces, `[[`, "mode")))
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
