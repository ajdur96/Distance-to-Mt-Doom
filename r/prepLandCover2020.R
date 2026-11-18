# prepLandCover2020.R
# One-time prep: reduce the NALCMS 2020 land cover (30 m, about 24 billion
# cells) to 240 m by keeping each block's most common class. The walking model
# works at 250 m, so this keeps the newer, sharper map without making every run
# read the full 30 m file.
#
# Why "most common class" and not nearest neighbor: nearest neighbor keeps one
# 30 m pixel out of every 64 and throws the rest away, so a single road or pond
# pixel can label a whole 240 m cell. The mode keeps what most of the ground is.
#
# Before running:
#   1. Download "North American Land Cover, 2020 (Landsat, 30m)" from
#      https://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/
#   2. Unzip it and set landCoverRawPath in modelParams.R to the .tif
# Expect this to take a long while (an hour or more) and a few GB of temp space.
# Run it once; frodoWalkingIsochrones.R, continentExport.R, and validateModel.R
# then read the result through loadLandCover().

library(terra)
source(Find(file.exists, c("modelParams.R", file.path("r", "modelParams.R"))))

if (!file.exists(landCoverRawPath)) {
  stop("Set landCoverRawPath in modelParams.R to the 2020 30 m .tif first.")
}
dir.create(dirname(landCoverPath), recursive = TRUE, showWarnings = FALSE)
terraOptions(progress = 1, memfrac = 0.6)

raw <- rast(landCoverRawPath)
cat(sprintf("Input: %d x %d cells at %.0f m, CRS %s\n",
            ncol(raw), nrow(raw), res(raw)[1], crs(raw, describe = TRUE)$name))
if (abs(res(raw)[1] - 30) > 1) stop("Expected a 30 m raster; check landCoverRawPath.")

# Without a declared no-data value, the blank border would count as a class
if (is.na(NAflag(raw))) NAflag(raw) <- 0

aggFactor <- 8  # 30 m x 8 = 240 m, just finer than the model's 250 m grid
reduced <- aggregate(raw, fact = aggFactor, fun = "modal", na.rm = TRUE)

# Anything that isn't one of the 19 classes (a different no-data code) becomes NA
reduced <- classify(reduced, cbind(1:19, 1:19), others = NA)
writeRaster(reduced, landCoverPath, datatype = "INT1U", overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))

# Quick check: class shares in the result
shares <- freq(rast(landCoverPath))
shares$percent <- round(100 * shares$count / sum(shares$count), 2)
shares$label <- landCoverFriction$label[match(shares$value, landCoverFriction$class)]
print(shares[, c("value", "label", "percent")], row.names = FALSE)
cat("Wrote", landCoverPath, "\n")
