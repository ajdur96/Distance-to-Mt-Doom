# placesExport.R
# Towns and cities for the Your Journey search box, from GeoNames (CC BY 4.0).
# Writes places.json into the same continent web folder. Takes a few minutes.

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite', type = 'binary')")

continentBox <- c(xmin = -130, ymin = 14, xmax = -52, ymax = 60)  # same extent as continentExport.R
minPopulation <- 1000
webDir <- file.path("frodoOutputs", "continent", "web")
gnDir <- file.path("frodoOutputs", "continent", "geonames")
dir.create(gnDir, recursive = TRUE, showWarnings = FALSE)

fetchOnce <- function(url, dest) {
  if (file.exists(dest)) return(TRUE)
  tryCatch({
    withCallingHandlers(download.file(url, dest, mode = "wb", method = "wininet", quiet = TRUE),
                        warning = function(w) invokeRestart("muffleWarning"))
    TRUE
  }, error = function(e) FALSE)
}
citiesZip <- file.path(gnDir, "cities1000.zip")
admin1File <- file.path(gnDir, "admin1CodesASCII.txt")
countryFile <- file.path(gnDir, "countryInfo.txt")
if (!fetchOnce("https://download.geonames.org/export/dump/cities1000.zip", citiesZip) ||
    !fetchOnce("https://download.geonames.org/export/dump/admin1CodesASCII.txt", admin1File) ||
    !fetchOnce("https://download.geonames.org/export/dump/countryInfo.txt", countryFile)) {
  stop("Could not download from download.geonames.org; try another network.")
}

# GeoNames columns: 2 name, 5 latitude, 6 longitude, 9 country code, 11 admin1 code, 15 population
cities <- read.delim(unz(citiesZip, "cities1000.txt"), header = FALSE, quote = "", comment.char = "",
                     encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
cities <- data.frame(name = cities$V2, lat = as.numeric(cities$V5), lon = as.numeric(cities$V6),
                     country = cities$V9, admin1 = cities$V11, pop = as.numeric(cities$V15))
cities <- cities[cities$lon >= continentBox[["xmin"]] & cities$lon <= continentBox[["xmax"]] &
                 cities$lat >= continentBox[["ymin"]] & cities$lat <= continentBox[["ymax"]] &
                 cities$pop >= minPopulation, ]

admin1 <- read.delim(admin1File, header = FALSE, quote = "", encoding = "UTF-8", stringsAsFactors = FALSE)
admin1Name <- setNames(admin1$V2, admin1$V1)
countries <- read.delim(countryFile, header = FALSE, quote = "", comment.char = "#", encoding = "UTF-8", stringsAsFactors = FALSE)
countryName <- setNames(countries$V5, countries$V1)

# "Springfield" needs its state to be useful; outside the US, add the country too
region <- unname(admin1Name[paste(cities$country, cities$admin1, sep = ".")])
region[is.na(region)] <- ""
outsideUs <- cities$country != "US"
region[outsideUs] <- trimws(paste(region[outsideUs], countryName[cities$country[outsideUs]], sep = ", "), whitespace = "[ ,]")

places <- data.frame(n = cities$name, r = region, lon = round(cities$lon, 3), lat = round(cities$lat, 3), p = cities$pop)
places <- places[order(-places$p), ]
jsonlite::write_json(places, file.path(webDir, "places.json"), dataframe = "columns")
message(sprintf("%d places written to places.json (%.1f MB)", nrow(places),
                file.size(file.path(webDir, "places.json")) / 1e6))
