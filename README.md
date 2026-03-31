# static_gear_vms

Queries VMS (Vessel Monitoring System) fishing effort data for static gear types from the ICES VMS database, aggregates by year, vessel length category and c-square, and produces interactive Leaflet maps as standalone HTML files.

## Data Source

Fishing effort data is retrieved via the [icesVMS](https://github.com/ices-tools-prod/icesVMS) R package, which queries the ICES VMS/logbook database. Records are spatially referenced using [c-squares](https://en.wikipedia.org/wiki/C-squares) at 0.05° resolution and converted to polygon geometries using [vmstools](https://github.com/nielshintzen/vmstools).

Map tiles are served by [MapTiler Ocean](https://www.maptiler.com/maps/#ocean) — you will need your own API key.

## Installation

### R packages (CRAN)
```r
install.packages(c("sf", "dplyr", "leaflet", "htmltools", "htmlwidgets", "jsonlite"))
```

### icesVMS (r-universe)
```r
install.packages("icesVMS", repos = "https://ices-tools-prod.r-universe.dev")
```

### vmstools (GitHub release)
```r
url <- "https://github.com/nielshintzen/vmstools/releases/download/0.77/vmstools_0.77.tar.gz"
download.file(url, destfile = "vmstools_0.77.tar.gz", mode = "wb")
install.packages("vmstools_0.77.tar.gz", repos = NULL, type = "source")
unlink("vmstools_0.77.tar.gz")
```

## Usage

1. Set your MapTiler API key and define the years and gear codes to query:
```r
api_key      <- "your_maptiler_api_key"
years        <- 2012:2024
static_lines <- c("LLS", "FPO", "GNS", "GTR")
```

2. Query the database and build the spatial data frame:
```r
results %
  filter(!is.na(cSquare)) %>%
  group_by(year, vesselLengthCategory, cSquare) %>%
  summarise(fishingHours = sum(fishingHours, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    summarise(., fishingHours = sum(fishingHours, na.rm = TRUE),
              .by = c(year, cSquare)) %>%
      mutate(vesselLengthCategory = "All Vessels")
  ) %>%
  mutate(gearCode = "Static Lines",
    geom = with(CSquare2LonLat(cSquare, 0.05), wkt_csquare(lat = SI_LATI, lon = SI_LONG))) %>%
  st_as_sf(wkt = "geom", crs = 4326)
```

3. Generate one interactive HTML map per year:
```r
source("build_maps.R")
```

This produces files named `static_vms_YYYY.html` in the working directory. Each map includes a dropdown to filter by vessel length category and hover tooltips showing gear type and fishing hours.