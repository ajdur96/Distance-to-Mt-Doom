# modelParams.R
# The walking model's shared settings and river-barrier helpers, in one place.
# Sourced by frodoWalkingIsochrones.R (the Arlington run) and continentExport.R
# (the continent pace grid behind Your Journey and both roads), so the two can
# never quietly disagree. Change the model here, then rerun both scripts.
#
# Needs terra, sf, and rnaturalearth loaded first (both scripts do this).

# ---- Paths and grid ----------------------------------------------------------
landCoverPath <- "C:/Users/avery/Downloads/land_cover_2010v2_250m_tif/NA_NALCMS_landcover_2010v2_250m/data/NA_NALCMS_landcover_2010v2_250m.tif"
demZoom <- 8              # terrain tile zoom; finer than fineRes at these latitudes
fineRes <- 250            # meters; resolution used for slope, land cover, and altitude
coarseFactor <- 20        # 250 m x 20 = 5 km cost grid
minLandFraction <- 0.5    # coarse cells with less land than this become barriers

# ---- Walking speed: slope, land cover, altitude ------------------------------
# Tobler's hiking function: grade = rise / run, returns km/h. This is on-path
# speed; the land cover multipliers below are the only off-path penalty.
tobler <- function(grade) 6 * exp(-3.5 * abs(grade + 0.05))
flatSpeedKmh <- tobler(0)  # about 5.04 km/h

# Land cover friction (NALCMS 19 classes): multipliers on walking time relative
# to open, path-like ground. Because the rings measure walking time, these follow
# time-based (velocity) terrain coefficients where they exist:
#   - Wood, N.J., and Schmidtlein, M.C. (2012), Anisotropic path modeling to assess
#     pedestrian-evacuation potential from Cascadia-related tsunamis in the US
#     Pacific Northwest, Natural Hazards 62:275-300. Land cover speed values
#     adapted from Soule and Goldman (1972), US Army terrain coefficients.
#   - de Gruchy, M., Caswell, E., and Edwards, J. (2017), Velocity-Based Terrain
#     Coefficients for Time-Based Models of Human Movement, Internet Archaeology 45.
#     Measured walking speed directly: bog 1.79, loose sand 1.19, lawn 1.03, tall grass 1.35.
# Wetland follows both sources (about 1.8). Barren, grassland, and cropland match
# the measured velocity values. Forest, shrub, polar, tropical, and snow classes
# have little published data and remain the author's estimates.
defaultFriction <- 1.3    # used where land cover has no data
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
                 1.4, 1.3, 1.3, 1.8, 1.1, 1.2, 1.0, NA, 2.0)
)

altitudePenaltyStartM <- 1500   # thin air starts slowing walkers above this
altitudePenaltyPer1000m <- 0.10 # +10% walking time per 1,000 m above the start

# ---- Rivers as barriers, crossable only at bridge cities -----------------------
# The rule: every river averaging at least 1,500 cubic meters per second (about
# 53,000 cubic feet per second) at its mouth is a barrier, within the map's extent.
# Such rivers can't be waded, and their bridges are far enough apart to force real
# detours; smaller rivers are assumed crossable on their many bridges. The floor
# sits in a natural gap among US rivers, between the Snake (about 1,560) and the
# Arkansas (about 1,260). The Mackenzie and Yukon qualify but lie north of the
# continent map (60 N). The Grijalva (about 1,160 on its own) falls just short.
riverBufferM <- 3000      # river barrier half width, thick enough to stop diagonal leaks
crossingRadiusKm <- 8     # gap opened in the river barrier at each bridge
maxBridgeSnapKm <- 25     # a bridge farther than this from its river is dropped

# Natural Earth 10 m river names to match (rivers_lake_centerlines$name)
barrierPatterns <- c(
  "Mississippi" = "^Mississippi$", "Missouri" = "^Missouri$", "Ohio" = "^Ohio$",
  "St. Lawrence" = "Lawrence|Laurent",
  "Columbia" = "^Columbia$", "Fraser" = "^Fraser$", "Atchafalaya" = "^Atchafalaya$",
  "Niagara" = "^Niagara$", "Detroit" = "^Detroit$", "St. Clair" = "^(St\\.?|Saint) Clair$",
  "Tennessee" = "^Tennessee$", "Red" = "^Red$", "Snake" = "^Snake$",
  "Ottawa" = "^Ottawa$", "Nelson" = "^Nelson$", "Peace" = "^Peace$", "Slave" = "^Slave$",
  "Liard" = "^Liard$", "La Grande" = "^La Grande$", "Koksoak" = "^Koksoak$",
  "Churchill" = "^Churchill$", "Usumacinta" = "^Usumacinta$"
  # The Mobile and Saguenay come only from hand-drawn lines below: Natural Earth
  # has no Mobile line, and its "Saguenay" lines are the rivers above Lac Saint-Jean.
)
# Natural Earth labels most of the Snake River "Columbia" (from the Tri-Cities to
# Wyoming). Segments of `from` lying wholly inside `box` are renamed `to`.
riverRelabel <- list(
  list(from = "Columbia", to = "Snake", box = c(-119.2, -110, 42, 47.5))
)
# Where a name is shared, keep only segments lying wholly inside this box
# (lon min, lon max, lat min, lat max). Natural Earth labels the Snake River
# "Columbia" from the Tri-Cities to Wyoming, and there is a second Fraser River
# in Labrador; neither should become a barrier.
riverBounds <- list(
  "Columbia" = c(-125, -115.5, 45, 53),
  "Fraser" = c(-125, -118, 48.5, 55),
  "Red" = c(-101, -91, 30, 35),          # the southern Red, not the Red River of the North
  "Churchill" = c(-66, -60, 52, 54.5)    # Labrador's Churchill, not Manitoba's (1,200)
)
# Keep only this stretch of a river (crop to the box). A river's average flow is
# measured at its mouth; near its source it is far smaller and bridged often.
# These cuts are deliberately forgiving, keeping most of each river:
#   - Mississippi from Minneapolis down (above it, a small river bridged often)
#   - Missouri from Great Falls down (above it, a mountain river with reservoirs)
#   - Red below Texarkana (above it, a shallow, braided prairie river)
#   - Snake from Hells Canyon down to the Columbia
riverClip <- list(
  "Mississippi" = c(-95, -88.5, 28.5, 45.05),
  "Missouri" = c(-111.35, -90, 38, 49),
  "Red" = c(-94.1, -91.5, 30.9, 33.9),
  "Snake" = c(-119.2, -116.3, 44.2, 46.8)
)

# Lines drawn by hand, (lon, lat) pairs in order downstream.
#   use = "always":   added even when Natural Earth has the river (to extend it)
#   use = "fallback": added only if Natural Earth has no line by that name
# These follow the main channel to within a few km, which the 3 km half-width
# buffer covers. Check them against the basemap after a run.
riverLine <- function(river, use, coords) {
  list(river = river, use = use, coords = matrix(coords, ncol = 2, byrow = TRUE))
}
handDrawnRivers <- list(
  # Natural Earth's Columbia stops about 50 km short of the Pacific. This carries
  # it through the estuary past Astoria, snapped to Natural Earth's own shoreline.
  riverLine("Columbia", "always", c(
    -123.216, 46.171,  -123.253, 46.160,  -123.317, 46.160,  -123.394, 46.205,  -123.457, 46.261,
    -123.549, 46.268,  -123.598, 46.261,  -123.692, 46.279,  -123.828, 46.211,  -124.044, 46.249)),
  # Natural Earth's Niagara covers only the gorge below the Falls. This adds the
  # upper river from Lake Erie at Buffalo, down the main (Canadian) channel.
  riverLine("Niagara", "always", c(
    -78.905, 42.900,  -78.925, 42.960,  -78.980, 43.000,  -79.050, 43.060,  -79.075, 43.080,  -79.070, 43.110)),
  # Natural Earth's St. Lawrence ends upstream of Montreal; this carries it past
  # Montreal, Trois-Rivieres, and Quebec City to where the estuary is open water.
  riverLine("St. Lawrence", "always", c(
    -74.73, 45.01,  -74.45, 45.10,  -74.30, 45.17,  -74.13, 45.26,  -73.90, 45.33,
    -73.67, 45.42,  -73.54, 45.52,  -73.49, 45.60,  -73.44, 45.67,  -73.36, 45.76,
    -73.25, 45.86,  -73.12, 46.04,
    -72.98, 46.13,  -72.80, 46.20,  -72.52, 46.33,  -72.35, 46.40,  -72.24, 46.50,
    -71.93, 46.64,  -71.62, 46.68,  -71.29, 46.75,  -71.19, 46.81,  -71.02, 46.85,
    -70.85, 46.93,  -70.58, 47.00,  -70.30, 47.25)),
  # Natural Earth's main river layer has no Atchafalaya; this is its course from
  # Natural Earth's North America rivers layer (simplified to about 0.7 km).
  # The first point is Old River, where the Atchafalaya leaves the Mississippi,
  # so walkers can't slip between the two rivers.
  riverLine("Atchafalaya", "fallback", c(
    -91.640, 31.070,  -91.747, 31.032,  -91.801, 30.977,  -91.803, 30.876,  -91.819, 30.853,  -91.798, 30.840,
    -91.807, 30.818,  -91.768, 30.782,  -91.736, 30.701,  -91.756, 30.574,  -91.734, 30.516,
    -91.754, 30.497,  -91.750, 30.479,  -91.696, 30.407,  -91.641, 30.367,  -91.624, 30.315,
    -91.538, 30.201,  -91.527, 30.159,  -91.483, 30.102,  -91.479, 30.079,  -91.495, 30.034,
    -91.459, 29.969,  -91.467, 29.925,  -91.394, 29.828,  -91.379, 29.785,  -91.347, 29.761,
    -91.254, 29.753,  -91.224, 29.736,  -91.219, 29.691,  -91.253, 29.613)),
  # The Mobile River: Natural Earth's "Alabama" line below the Tombigbee confluence
  riverLine("Mobile", "fallback", c(
    -87.885, 31.156,  -87.945, 31.147,  -87.961, 31.092,  -87.941, 31.076,  -87.961, 31.075,
    -87.934, 31.073,  -87.960, 31.029,  -87.968, 30.980,  -87.941, 30.950,  -87.984, 30.880,
    -87.954, 30.844,  -87.957, 30.830,  -88.001, 30.812,  -88.023, 30.785,  -87.989, 30.761,
    -88.002, 30.747,  -87.995, 30.685)),
  # The Saguenay fjord from Chicoutimi to the St. Lawrence at Tadoussac, snapped to
  # Natural Earth's shoreline. Above Chicoutimi it is bridged at several towns.
  riverLine("Saguenay", "always", c(
    -71.070, 48.445,  -71.003, 48.452,  -70.945, 48.444,  -70.891, 48.448,  -70.827, 48.432,
    -70.743, 48.425,  -70.709, 48.401,  -70.670, 48.392,  -70.634, 48.397,  -70.495, 48.375,
    -70.415, 48.340,  -70.353, 48.327,  -70.323, 48.300,  -70.272, 48.298,  -70.225, 48.268,
    -70.126, 48.238,  -70.093, 48.235,  -70.060, 48.244,  -69.956, 48.232,  -69.917, 48.216,
    -69.883, 48.175,  -69.727, 48.141)),
  # Natural Earth has no Detroit River line; drawn by hand along the channel
  riverLine("Detroit", "fallback", c(
    -82.93, 42.34,  -83.04, 42.32,  -83.07, 42.31,  -83.13, 42.20,  -83.14, 42.03))
)

# Bridge cities, by river, upstream to downstream. Each is snapped to the
# nearest point on its own river, and a gap is opened in the barrier there.
bridgeRows <- function(river, ...) {
  pts <- list(...)
  data.frame(name = names(pts), river = river,
             lon = vapply(pts, `[`, numeric(1), 1), lat = vapply(pts, `[`, numeric(1), 2),
             row.names = NULL)
}
bridges <- rbind(
  bridgeRows("Mississippi",
    "Minneapolis" = c(-93.26, 44.98),
    "La Crosse" = c(-91.24, 43.80), "Dubuque" = c(-90.66, 42.50), "Davenport" = c(-90.58, 41.52),
    "Burlington" = c(-91.10, 40.81), "Quincy" = c(-91.41, 39.93), "St. Louis" = c(-90.18, 38.63),
    "Cape Girardeau" = c(-89.52, 37.31), "Memphis" = c(-90.05, 35.15), "Greenville" = c(-91.06, 33.41),
    "Vicksburg" = c(-90.88, 32.35), "Natchez" = c(-91.40, 31.56), "Baton Rouge" = c(-91.19, 30.45),
    "New Orleans" = c(-90.07, 29.95)),
  bridgeRows("Missouri",
    "Great Falls" = c(-111.30, 47.50), "Williston" = c(-103.62, 48.15), "Bismarck" = c(-100.78, 46.81),
    "Pierre" = c(-100.35, 44.37), "Yankton" = c(-97.40, 42.87), "Sioux City" = c(-96.40, 42.50),
    "Omaha" = c(-95.94, 41.26), "St. Joseph" = c(-94.85, 39.77), "Kansas City" = c(-94.58, 39.10),
    "Jefferson City" = c(-92.17, 38.58), "St. Charles" = c(-90.48, 38.79)),
  bridgeRows("Ohio",
    "Pittsburgh" = c(-80.00, 40.44), "Wheeling" = c(-80.72, 40.06), "Parkersburg" = c(-81.56, 39.27),
    "Huntington" = c(-82.45, 38.42), "Portsmouth" = c(-83.00, 38.73), "Cincinnati" = c(-84.51, 39.10),
    "Louisville" = c(-85.76, 38.25), "Evansville" = c(-87.57, 37.97), "Paducah" = c(-88.60, 37.08),
    "Cairo" = c(-89.18, 37.00)),
  bridgeRows("St. Lawrence",
    "Thousand Islands" = c(-75.98, 44.35), "Ogdensburg" = c(-75.49, 44.70), "Cornwall" = c(-74.73, 45.02),
    "Montreal" = c(-73.57, 45.50), "Trois-Rivieres" = c(-72.55, 46.34), "Quebec City" = c(-71.27, 46.75)),
  # New barrier rivers. Coordinates are approximate city crossings; check on the map.
  bridgeRows("Columbia",
    "Golden" = c(-116.97, 51.30), "Revelstoke" = c(-118.20, 50.99), "Castlegar" = c(-117.66, 49.32),
    "Trail" = c(-117.71, 49.10), "Kettle Falls" = c(-118.10, 48.63), "Grand Coulee" = c(-118.98, 47.95),
    "Wenatchee" = c(-120.31, 47.42), "Vantage" = c(-119.98, 46.94), "Tri-Cities" = c(-119.11, 46.22),
    "Umatilla" = c(-119.33, 45.92), "The Dalles" = c(-121.18, 45.61), "Hood River" = c(-121.51, 45.71),
    "Portland-Vancouver" = c(-122.67, 45.62), "Longview" = c(-122.96, 46.10), "Astoria" = c(-123.85, 46.20)),
  bridgeRows("Fraser",
    "Prince George" = c(-122.74, 53.92), "Quesnel" = c(-122.50, 52.98), "Lillooet" = c(-121.93, 50.69),
    "Lytton" = c(-121.58, 50.23), "Hope" = c(-121.44, 49.38), "Mission" = c(-122.30, 49.13),
    "New Westminster" = c(-122.90, 49.21)),
  bridgeRows("Atchafalaya",
    "Simmesport" = c(-91.80, 30.98), "Krotz Springs" = c(-91.75, 30.54),
    "Butte La Rose (I-10)" = c(-91.69, 30.28), "Morgan City" = c(-91.21, 29.70)),
  bridgeRows("Niagara",
    "Buffalo" = c(-78.90, 42.91), "Niagara Falls" = c(-79.07, 43.09), "Lewiston" = c(-79.04, 43.15)),
  bridgeRows("Detroit", "Detroit" = c(-83.07, 42.31)),
  bridgeRows("St. Clair", "Port Huron" = c(-82.42, 43.00)),
  bridgeRows("Tennessee",
    "Knoxville" = c(-83.92, 35.96), "Loudon" = c(-84.33, 35.73), "Chattanooga" = c(-85.31, 35.06),
    "South Pittsburg" = c(-85.70, 35.01), "Guntersville" = c(-86.30, 34.36), "Decatur" = c(-86.98, 34.62),
    "Florence" = c(-87.67, 34.79), "Savannah" = c(-88.24, 35.22), "I-40 crossing" = c(-88.02, 35.96),
    "Paris Landing" = c(-88.08, 36.44), "Kentucky Dam" = c(-88.27, 37.01)),
  bridgeRows("Mobile", "I-65 (Mount Vernon)" = c(-87.97, 30.98), "Mobile" = c(-88.03, 30.69)),
  bridgeRows("Red",
    "Fulton (I-30)" = c(-93.82, 33.61), "Shreveport" = c(-93.74, 32.51), "Coushatta" = c(-93.34, 32.01),
    "Grand Ecore" = c(-93.06, 31.79), "Alexandria" = c(-92.45, 31.31)),
  bridgeRows("Snake",
    "Brownlee Dam" = c(-116.90, 44.84), "Lewiston" = c(-117.03, 46.42), "Central Ferry" = c(-117.79, 46.65),
    "Lyons Ferry" = c(-118.22, 46.59), "Pasco (mouth)" = c(-119.03, 46.21)),
  bridgeRows("Ottawa",
    "Temiscaming" = c(-79.10, 46.72), "Mattawa" = c(-78.70, 46.32), "Rapides-des-Joachims" = c(-77.70, 46.20),
    "Pembroke" = c(-77.11, 45.83), "Portage-du-Fort" = c(-76.67, 45.59), "Ottawa-Gatineau" = c(-75.70, 45.42),
    "Hawkesbury" = c(-74.61, 45.62), "Carillon" = c(-74.38, 45.57)),
  bridgeRows("Saguenay", "Chicoutimi" = c(-71.06, 48.43)),
  # Remote northern rivers: only the few road crossings (often at dams) exist
  bridgeRows("Nelson", "Jenpeg" = c(-98.03, 54.53), "Kettle (Gillam)" = c(-94.69, 56.39)),
  bridgeRows("Peace",
    "Hudson's Hope" = c(-121.91, 56.03), "Taylor" = c(-120.68, 56.15), "Dunvegan" = c(-118.60, 55.92),
    "Peace River" = c(-117.29, 56.24), "Fort Vermilion" = c(-116.00, 58.39)),
  bridgeRows("Liard", "Liard River (Alaska Hwy)" = c(-126.10, 59.42)),
  bridgeRows("La Grande", "Radisson (LG-2)" = c(-77.50, 53.78)),
  bridgeRows("Churchill", "Churchill Falls" = c(-64.00, 53.53), "Goose Bay" = c(-60.77, 53.25)),
  bridgeRows("Usumacinta", "Boca del Cerro" = c(-91.43, 17.43), "Emiliano Zapata" = c(-91.77, 17.74))
  # The Slave and Koksoak have no bridges.
)

# ---- River helpers -------------------------------------------------------------
# Every barrier river as lines in WGS84, with a `river` column naming it.
# Stops if any barrier river ends up with no line at all.
loadBarrierRivers <- function() {
  raw <- vect(ne_download(scale = 10, type = "rivers_lake_centerlines",
                          category = "physical", returnclass = "sf"))
  matches <- sapply(barrierPatterns, function(pattern) grepl(pattern, raw$name, ignore.case = TRUE))
  found <- colSums(matches)
  handOnly <- setdiff(vapply(handDrawnRivers, `[[`, "", "river"), names(found))
  found[handOnly] <- 0
  isBarrier <- rowSums(matches) > 0
  rivers <- raw[isBarrier]
  rivers$river <- names(barrierPatterns)[apply(matches[isBarrier, , drop = FALSE], 1, which.max)]
  rivers <- rivers[, "river"]
  whollyInside <- function(i, b) {
    e <- as.vector(ext(rivers[i]))
    e[["xmin"]] >= b[1] && e[["xmax"]] <= b[2] && e[["ymin"]] >= b[3] && e[["ymax"]] <= b[4]
  }
  for (rl in riverRelabel) {
    hit <- which(rivers$river == rl$from)
    hit <- hit[vapply(hit, whollyInside, logical(1), b = rl$box)]
    if (length(hit)) {
      rivers$river[hit] <- rl$to
      found[[rl$from]] <- found[[rl$from]] - length(hit)
      found[[rl$to]] <- found[[rl$to]] + length(hit)
      message(sprintf("Renamed %d segment(s) labelled %s to %s", length(hit), rl$from, rl$to))
    }
  }
  for (r in names(riverBounds)) {
    b <- riverBounds[[r]]
    named <- which(rivers$river == r)
    inside <- vapply(named, whollyInside, logical(1), b = b)
    if (any(!inside)) {
      message(sprintf("Skipped %d segment(s) named %s outside its expected area", sum(!inside), r))
      rivers <- rivers[-named[!inside]]
      found[[r]] <- found[[r]] - sum(!inside)
    }
  }
  for (r in names(riverClip)) {
    b <- riverClip[[r]]
    kept <- crop(rivers[rivers$river == r], ext(b[1], b[2], b[3], b[4]))
    rivers <- rbind(rivers[rivers$river != r], kept)
    found[[r]] <- nrow(kept)
    message(sprintf("Kept the %s only between %.1f and %.1f W", r, -b[2], -b[1]))
  }
  for (h in handDrawnRivers) {
    if (h$use == "fallback" && found[[h$river]] > 0) next
    line <- vect(h$coords, type = "lines", crs = "EPSG:4326")
    line$river <- h$river
    rivers <- rbind(rivers, line)
    message(sprintf("Added hand-drawn line for the %s (%s)", h$river,
                    if (found[[h$river]] > 0) "extends Natural Earth" else "not in Natural Earth"))
    found[[h$river]] <- found[[h$river]] + 1
  }
  message("Barrier river segments: ", paste(names(found), found, sep = " = ", collapse = ", "))
  if (any(found == 0)) {
    stop("No line found for: ", paste(names(found)[found == 0], collapse = ", "),
         ". Check unique(raw$name) for its spelling, or add a hand-drawn line.")
  }
  rivers
}

# Cut the barrier rivers into a cost grid, opening a gap at each bridge city.
#   paceCoarse: the coarse pace raster (hours per meter), already in `crs`
#   riversWgs:  output of loadBarrierRivers()
#   clipWgs:    the area to keep, in WGS84 (a SpatExtent or a SpatVector polygon)
# Returns the pace raster with rivers as NA, plus the rivers and crossings used.
applyRiverBarriers <- function(paceCoarse, riversWgs, clipWgs, crs) {
  # Cropping in WGS84 keeps a loose box, so crop again to this run's grid
  rivers <- crop(project(crop(riversWgs, clipWgs), crs), ext(paceCoarse))
  present <- sort(unique(rivers$river))
  message("Barrier rivers in this area: ", paste(present, collapse = ", "))
  b <- bridges[bridges$river %in% present, ]
  bridgesV <- project(vect(b, geom = c("lon", "lat"), crs = "EPSG:4326"), crs)
  inGrid <- relate(bridgesV, ext(paceCoarse) + maxBridgeSnapKm * 1000, "intersects")[, 1]
  b <- b[inGrid, ]
  bridgesSf <- st_as_sf(bridgesV[inGrid])
  riversSf <- st_as_sf(rivers)
  snapXY <- matrix(NA_real_, nrow(b), 2)
  snapDistKm <- rep(NA_real_, nrow(b))
  for (i in seq_len(nrow(b))) {
    # Snap to the bridge's own river (so St. Charles snaps to the Missouri, not
    # the nearby Mississippi)
    ownRiver <- riversSf[riversSf$river == b$river[i], ]
    connector <- st_nearest_points(bridgesSf[i, ], st_union(ownRiver))
    snapXY[i, ] <- st_coordinates(connector)[2, 1:2]
    snapDistKm[i] <- as.numeric(st_length(connector)) / 1000
  }
  far <- snapDistKm > maxBridgeSnapKm
  if (any(far)) {
    message("Dropped bridges more than ", maxBridgeSnapKm, " km from their river: ",
            paste(sprintf("%s (%.0f km)", b$name[far], snapDistKm[far]), collapse = ", "))
  }
  crossings <- vect(snapXY[!far, , drop = FALSE], crs = crs)
  crossings$name <- b$name[!far]
  crossings$river <- b$river[!far]
  barrier <- rasterize(buffer(rivers, riverBufferM), paceCoarse, touches = TRUE)
  openings <- rasterize(buffer(crossings, crossingRadiusKm * 1000), paceCoarse, touches = TRUE)
  barrier <- mask(barrier, openings, inverse = TRUE)  # open the bridges
  list(pace = mask(paceCoarse, barrier, inverse = TRUE),  # rivers become NA barriers
       rivers = rivers, crossings = crossings)
}
