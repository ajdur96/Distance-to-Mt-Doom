# fetchPhotos.R
# Photos for the ring landmarks of both roads on the Frodo's Journey tab. For each one it
# takes the lead image of its English Wikipedia article, but only if that image
# is freely licensed, then saves a 640 px copy plus the author and license
# from Wikimedia Commons, which the page needs to credit each photo.
#
# To hand pick a different photo, find it on Wikimedia Commons and put its
# file name in photoOverrides below, then run the script again.

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first: install.packages('jsonlite', type = 'binary')")
options(HTTPUserAgent = "frodoWalkingIsochrones/1.0 (personal mapping project)")

photoDir <- file.path("frodoOutputs", "photos")
dir.create(photoDir, recursive = TRUE, showWarnings = FALSE)

# Wikipedia article for each ring landmark, in order (step 1 to 11), for both roads
roads <- list(
  arlington = c(
    "Battle of Harpers Ferry", "Clarks Knob", "Laurel Hill (Pennsylvania)", "Hocking Hills State Park",
    "Linville Falls", "Kuwohi", "Lake Blackshear", "Okefenokee Swamp",
    "Manatee Springs State Park", "Lower Suwannee National Wildlife Refuge", "Ocala National Forest"
  ),
  hobbit = c(
    "Tahkenitch Lake", "Charles and Anna Drain House", "Rogue River Ranch", "Klamath National Forest",
    "Vya, Nevada", "Black Rock Desert", "Yosemite Falls", "New Idria, California",
    "Los Padres National Forest", "Santa Ynez Valley", "Hollywood Sign"
  )
)
# Hand-picked Wikimedia Commons photos, by road and step, file name only
# (credits are fetched automatically), e.g. "hobbit_7" = "Yosemite Falls from trail.jpg"
photoOverrides <- c(
)
# Hand-picked photos from other sources, by road and step. Only use photos you
# have the right to use (public domain, a free license, or written permission),
# and type the credit exactly as the source asks.
photoDirect <- list(
  "arlington_6" = list(url = "https://www.nps.gov/common/uploads/cropped_image/primary/7F24F209-DA90-053E-45656F09B7CFD9BF.jpg?width=1600&quality=90&mode=crop",
                       credit = "National Park Service", license = "Public domain",
                       sourcePage = "https://www.nps.gov/grsm/")
)

# Downloads use libcurl, since the WinINet method failed for Wikimedia Commons
fetchJson <- function(url) {
  tmp <- tempfile(fileext = ".json")
  for (wait in c(0, 10, 30)) {
    if (wait > 0) Sys.sleep(wait)
    ok <- tryCatch({
      download.file(url, tmp, mode = "wb", method = "libcurl", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL))
  }
  NULL
}
fetchFile <- function(url, dest) {
  tryCatch({
    download.file(url, dest, mode = "wb", method = "libcurl", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
}
stripHtml <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", x)))

credits <- list()
for (roadName in names(roads)) {
  articles <- roads[[roadName]]
  for (step in seq_along(articles)) {
    key <- paste0(roadName, "_", step)
    saveAs <- sprintf("%s_step%02d.jpg", roadName, step)
    direct <- photoDirect[[key]]
    if (!is.null(direct)) {
      if (!fetchFile(direct$url, file.path(photoDir, saveAs))) { message(key, ": direct download failed; rerun"); next }
      credits[[length(credits) + 1]] <- list(road = roadName, step = step, article = articles[step], file = saveAs,
        commonsFile = NA, commonsPage = direct$sourcePage, author = direct$credit,
        license = direct$license, licenseUrl = direct$sourcePage)
      message(key, ". ", articles[step], ": ", direct$credit)
      next
    }
    fileName <- photoOverrides[key]
    if (is.null(fileName) || is.na(fileName)) {
      res <- fetchJson(paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageimages&piprop=name",
                              "&pilicense=free&redirects=1&format=json&formatversion=2&titles=",
                              URLencode(articles[step], reserved = TRUE)))
      fileName <- tryCatch(res$query$pages[[1]]$pageimage, error = function(e) NULL)
      if (is.null(fileName)) { message(key, ". ", articles[step], ": no free lead image; consider an override"); next }
    }
    info <- fetchJson(paste0("https://commons.wikimedia.org/w/api.php?action=query&prop=imageinfo",
                             "&iiprop=url%7Cextmetadata&iiurlwidth=640&format=json&formatversion=2&titles=",
                             URLencode(paste0("File:", sub("^File:", "", fileName)), reserved = TRUE)))
    ii <- tryCatch(info$query$pages[[1]]$imageinfo[[1]], error = function(e) NULL)
    if (is.null(ii)) { message(key, ". ", articles[step], ": image is not on Commons; consider an override"); next }
    meta <- ii$extmetadata
    if (!fetchFile(ii$thumburl, file.path(photoDir, saveAs))) { message(key, ": download failed; rerun"); next }
    credits[[length(credits) + 1]] <- list(
      road = roadName, step = step, article = articles[step], file = saveAs, commonsFile = sub("^File:", "", fileName),
      commonsPage = ii$descriptionurl,
      author = if (!is.null(meta$Artist$value)) stripHtml(meta$Artist$value) else "Unknown",
      license = if (!is.null(meta$LicenseShortName$value)) meta$LicenseShortName$value else "See Commons page",
      licenseUrl = if (!is.null(meta$LicenseUrl$value)) meta$LicenseUrl$value else ii$descriptionurl
    )
    message(key, ". ", articles[step], ": ", sub("^File:", "", fileName))
    Sys.sleep(1)
  }
}

jsonlite::write_json(credits, file.path(photoDir, "photos.json"), auto_unbox = TRUE, pretty = TRUE)
message(length(credits), " of 22 photos saved in ", normalizePath(photoDir),
        ". Look through them, set any overrides, and zip the folder to upload.")
