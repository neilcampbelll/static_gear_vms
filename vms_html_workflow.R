library(pacman)

pacman::p_load(icesVMS, sf, vmstools, dplyr, tidyverse, marmap, leaflet,
               raster, viridis, rnaturalearth, rnaturalearthdata,
               htmltools, htmlwidgets, jsonlite)
## remember with sf and dplyr loaded we need to specify which when using the select function

years <- c(2009:2024)

static_lines <- c("LX", "LLS")

entangling_nets <- c("GTR", "GNS", "GN", "GTN")

traps_and_pots <- c("FPO", "FPN")



## Do it all for static lines
results <- list()

for (i in years) {
  for (j in static_lines) {
    results[[length(results) + 1]] <- get_vms(year = i, gear_code = j)
  }
}

static_vms <- bind_rows(results) %>%
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

static_vms <- st_set_geometry(
  static_vms,
  st_as_sfc(st_as_binary(st_geometry(static_vms), precision = 10000))
)

st_crs(static_vms) <- 4326

pal_cols <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
api_key  <- readLines("api_key.txt")
years    <- sort(unique(static_vms$year))

for (yr in years) {
  
  vms_yr <- static_vms[static_vms$year == yr, ]
  
  vlc_cats  <- c("All Vessels", sort(unique(vms_yr$vesselLengthCategory[vms_yr$vesselLengthCategory != "All Vessels"])))
  max_hours <- max(vms_yr$fishingHours, na.rm = TRUE)
  log_max   <- log1p(max_hours)
  
  st_write(vms_yr, "temp.geojson", delete_dsn = TRUE, quiet = TRUE)
  geojson_str <- readLines("temp.geojson") |> paste(collapse = "\n")
  
  html_page <- tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"),
      tags$style(HTML("
        body { margin: 0; display: flex; flex-direction: column; height: 100vh; font-family: sans-serif; background: #f4f4f4; }
        #controls { background: white; padding: 15px; display: flex; gap: 30px; align-items: center; border-bottom: 1px solid #ccc; z-index: 1001; flex-wrap: wrap; }
        .ctrl-group { display: flex; flex-direction: column; gap: 4px; }
        .heading { font-size: 0.7rem; font-weight: bold; color: #666; text-transform: uppercase; }
        #vlc-select { padding: 5px 10px; font-size: 1rem; border-radius: 4px; border: 1px solid #ccc; }
        #year-title { font-size: 1.2rem; font-weight: bold; color: #2c3e50; }
        #map { flex: 1; background: #001524; }
        #legend {
          position: absolute; bottom: 30px; right: 20px; z-index: 1000;
          background: rgba(255,255,255,0.9); padding: 12px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.2);
        }
        #legend-bar { width: 150px; height: 15px; background: linear-gradient(to right, #ffffb2, #fecc5c, #fd8d3c, #f03b20, #bd0026); }
      "))
    ),
    tags$body(
      tags$div(id = "controls",
               tags$div(class = "ctrl-group",
                        tags$span(id = "year-title", paste("Static Lines —", yr))
               ),
               tags$div(class = "ctrl-group",
                        tags$label(class = "heading", "Vessel Length Category"),
                        tags$select(id = "vlc-select",
                                    lapply(vlc_cats, function(v) tags$option(value = v, v))
                        )
               )
      ),
      tags$div(id = "map"),
      tags$div(id = "legend",
               tags$b("Fishing Hours (Log Scale)"),
               tags$div(id = "legend-bar"),
               tags$div(
                 style = "display:flex; justify-content:space-between; font-size:0.8rem; margin-top:3px;",
                 tags$span("Low"), tags$span(format(round(max_hours), big.mark = ","))
               )
      ),
      tags$script(src = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"),
      tags$script(HTML(sprintf("
        const data = %s;
        const logMax = %f;
        const colors = %s;
        const API_KEY = '%s';

        const map = L.map('map', { preferCanvas: true }).setView([55, 5], 5);

        L.tileLayer('https://api.maptiler.com/maps/ocean/{z}/{x}/{y}.png?key=' + API_KEY, {
          attribution: '&copy; MapTiler &copy; OpenStreetMap',
          tileSize: 512,
          zoomOffset: -1,
          crossOrigin: true
        }).addTo(map);

        let activeVlc = 'All Vessels';
        let geoLayer = null;

        function getCol(h) {
          if (!h || h <= 0.05) return 'transparent';
          const val = Math.log1p(h) / logMax;
          const idx = Math.round(val * (colors.length - 1));
          return colors[Math.max(0, Math.min(idx, colors.length - 1))];
        }

        function render() {
          if (geoLayer) map.removeLayer(geoLayer);

          const filtered = data.features.filter(f => f.properties.vesselLengthCategory === activeVlc);

          geoLayer = L.geoJSON({ type: 'FeatureCollection', features: filtered }, {
            style: (f) => ({
              fillColor: getCol(f.properties.fishingHours),
              fillOpacity: 0.85,
              stroke: false
            }),
            onEachFeature: (f, layer) => {
              layer.bindTooltip(
                '<b>' + f.properties.vesselLengthCategory + '</b><br>' +
                'Gear: ' + f.properties.gearCode + '<br>' +
                'Hours: ' + f.properties.fishingHours.toLocaleString(undefined, {maximumFractionDigits: 1})
              );
            }
          }).addTo(map);
        }

        document.getElementById('vlc-select').addEventListener('change', (e) => {
          activeVlc = e.target.value;
          render();
        });

        const bounds = L.geoJSON(data).getBounds();
        if (bounds.isValid()) map.fitBounds(bounds.pad(0.1));

        render();
      ", geojson_str, log_max, toJSON(pal_cols), api_key)))
    )
  )
  
  save_html(html_page, sprintf("static_lines_vms_%d.html", yr))
  cat("Saved", yr, "\n")
}

unlink("temp.geojson")

######################################################################################




## Do it all for gillnets
results <- list()

for (i in years) {
  for (j in entangling_nets) {
    results[[length(results) + 1]] <- get_vms(year = i, gear_code = j)
  }
}

static_vms <- bind_rows(results) %>%
  filter(!is.na(cSquare)) %>%
  group_by(year, vesselLengthCategory, cSquare) %>%
  summarise(fishingHours = sum(fishingHours, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    summarise(., fishingHours = sum(fishingHours, na.rm = TRUE),
              .by = c(year, cSquare)) %>%
      mutate(vesselLengthCategory = "All Vessels")
  ) %>%
  mutate(gearCode = "Gillnets",
         geom = with(CSquare2LonLat(cSquare, 0.05), wkt_csquare(lat = SI_LATI, lon = SI_LONG))) %>%
  st_as_sf(wkt = "geom", crs = 4326)

static_vms <- st_set_geometry(
  static_vms,
  st_as_sfc(st_as_binary(st_geometry(static_vms), precision = 10000))
)

st_crs(static_vms) <- 4326

pal_cols <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
api_key  <- readLines("api_key.txt")
years    <- sort(unique(static_vms$year))

for (yr in years) {
  
  vms_yr <- static_vms[static_vms$year == yr, ]
  
  vlc_cats  <- c("All Vessels", sort(unique(vms_yr$vesselLengthCategory[vms_yr$vesselLengthCategory != "All Vessels"])))
  max_hours <- max(vms_yr$fishingHours, na.rm = TRUE)
  log_max   <- log1p(max_hours)
  
  st_write(vms_yr, "temp.geojson", delete_dsn = TRUE, quiet = TRUE)
  geojson_str <- readLines("temp.geojson") |> paste(collapse = "\n")
  
  html_page <- tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"),
      tags$style(HTML("
        body { margin: 0; display: flex; flex-direction: column; height: 100vh; font-family: sans-serif; background: #f4f4f4; }
        #controls { background: white; padding: 15px; display: flex; gap: 30px; align-items: center; border-bottom: 1px solid #ccc; z-index: 1001; flex-wrap: wrap; }
        .ctrl-group { display: flex; flex-direction: column; gap: 4px; }
        .heading { font-size: 0.7rem; font-weight: bold; color: #666; text-transform: uppercase; }
        #vlc-select { padding: 5px 10px; font-size: 1rem; border-radius: 4px; border: 1px solid #ccc; }
        #year-title { font-size: 1.2rem; font-weight: bold; color: #2c3e50; }
        #map { flex: 1; background: #001524; }
        #legend {
          position: absolute; bottom: 30px; right: 20px; z-index: 1000;
          background: rgba(255,255,255,0.9); padding: 12px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.2);
        }
        #legend-bar { width: 150px; height: 15px; background: linear-gradient(to right, #ffffb2, #fecc5c, #fd8d3c, #f03b20, #bd0026); }
      "))
    ),
    tags$body(
      tags$div(id = "controls",
               tags$div(class = "ctrl-group",
                        tags$span(id = "year-title", paste("Static Lines —", yr))
               ),
               tags$div(class = "ctrl-group",
                        tags$label(class = "heading", "Vessel Length Category"),
                        tags$select(id = "vlc-select",
                                    lapply(vlc_cats, function(v) tags$option(value = v, v))
                        )
               )
      ),
      tags$div(id = "map"),
      tags$div(id = "legend",
               tags$b("Fishing Hours (Log Scale)"),
               tags$div(id = "legend-bar"),
               tags$div(
                 style = "display:flex; justify-content:space-between; font-size:0.8rem; margin-top:3px;",
                 tags$span("Low"), tags$span(format(round(max_hours), big.mark = ","))
               )
      ),
      tags$script(src = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"),
      tags$script(HTML(sprintf("
        const data = %s;
        const logMax = %f;
        const colors = %s;
        const API_KEY = '%s';

        const map = L.map('map', { preferCanvas: true }).setView([55, 5], 5);

        L.tileLayer('https://api.maptiler.com/maps/ocean/{z}/{x}/{y}.png?key=' + API_KEY, {
          attribution: '&copy; MapTiler &copy; OpenStreetMap',
          tileSize: 512,
          zoomOffset: -1,
          crossOrigin: true
        }).addTo(map);

        let activeVlc = 'All Vessels';
        let geoLayer = null;

        function getCol(h) {
          if (!h || h <= 0.05) return 'transparent';
          const val = Math.log1p(h) / logMax;
          const idx = Math.round(val * (colors.length - 1));
          return colors[Math.max(0, Math.min(idx, colors.length - 1))];
        }

        function render() {
          if (geoLayer) map.removeLayer(geoLayer);

          const filtered = data.features.filter(f => f.properties.vesselLengthCategory === activeVlc);

          geoLayer = L.geoJSON({ type: 'FeatureCollection', features: filtered }, {
            style: (f) => ({
              fillColor: getCol(f.properties.fishingHours),
              fillOpacity: 0.85,
              stroke: false
            }),
            onEachFeature: (f, layer) => {
              layer.bindTooltip(
                '<b>' + f.properties.vesselLengthCategory + '</b><br>' +
                'Gear: ' + f.properties.gearCode + '<br>' +
                'Hours: ' + f.properties.fishingHours.toLocaleString(undefined, {maximumFractionDigits: 1})
              );
            }
          }).addTo(map);
        }

        document.getElementById('vlc-select').addEventListener('change', (e) => {
          activeVlc = e.target.value;
          render();
        });

        const bounds = L.geoJSON(data).getBounds();
        if (bounds.isValid()) map.fitBounds(bounds.pad(0.1));

        render();
      ", geojson_str, log_max, toJSON(pal_cols), api_key)))
    )
  )
  
  save_html(html_page, sprintf("Entangling_nets_vms_%d.html", yr))
  cat("Saved", yr, "\n")
}

unlink("temp.geojson")

###############################################################################################################




## Do it all for pots and traps
results <- list()

for (i in years) {
  for (j in traps_and_pots) {
    results[[length(results) + 1]] <- get_vms(year = i, gear_code = j)
  }
}

static_vms <- bind_rows(results) %>%
  filter(!is.na(cSquare)) %>%
  group_by(year, vesselLengthCategory, cSquare) %>%
  summarise(fishingHours = sum(fishingHours, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    summarise(., fishingHours = sum(fishingHours, na.rm = TRUE),
              .by = c(year, cSquare)) %>%
      mutate(vesselLengthCategory = "All Vessels")
  ) %>%
  mutate(gearCode = "Pots & Traps",
         geom = with(CSquare2LonLat(cSquare, 0.05), wkt_csquare(lat = SI_LATI, lon = SI_LONG))) %>%
  st_as_sf(wkt = "geom", crs = 4326)

static_vms <- st_set_geometry(
  static_vms,
  st_as_sfc(st_as_binary(st_geometry(static_vms), precision = 10000))
)

st_crs(static_vms) <- 4326

pal_cols <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
api_key  <- readLines("api_key.txt")
years    <- sort(unique(static_vms$year))

for (yr in years) {
  
  vms_yr <- static_vms[static_vms$year == yr, ]
  
  vlc_cats  <- c("All Vessels", sort(unique(vms_yr$vesselLengthCategory[vms_yr$vesselLengthCategory != "All Vessels"])))
  max_hours <- max(vms_yr$fishingHours, na.rm = TRUE)
  log_max   <- log1p(max_hours)
  
  st_write(vms_yr, "temp.geojson", delete_dsn = TRUE, quiet = TRUE)
  geojson_str <- readLines("temp.geojson") |> paste(collapse = "\n")
  
  html_page <- tagList(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"),
      tags$style(HTML("
        body { margin: 0; display: flex; flex-direction: column; height: 100vh; font-family: sans-serif; background: #f4f4f4; }
        #controls { background: white; padding: 15px; display: flex; gap: 30px; align-items: center; border-bottom: 1px solid #ccc; z-index: 1001; flex-wrap: wrap; }
        .ctrl-group { display: flex; flex-direction: column; gap: 4px; }
        .heading { font-size: 0.7rem; font-weight: bold; color: #666; text-transform: uppercase; }
        #vlc-select { padding: 5px 10px; font-size: 1rem; border-radius: 4px; border: 1px solid #ccc; }
        #year-title { font-size: 1.2rem; font-weight: bold; color: #2c3e50; }
        #map { flex: 1; background: #001524; }
        #legend {
          position: absolute; bottom: 30px; right: 20px; z-index: 1000;
          background: rgba(255,255,255,0.9); padding: 12px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.2);
        }
        #legend-bar { width: 150px; height: 15px; background: linear-gradient(to right, #ffffb2, #fecc5c, #fd8d3c, #f03b20, #bd0026); }
      "))
    ),
    tags$body(
      tags$div(id = "controls",
               tags$div(class = "ctrl-group",
                        tags$span(id = "year-title", paste("Static Lines —", yr))
               ),
               tags$div(class = "ctrl-group",
                        tags$label(class = "heading", "Vessel Length Category"),
                        tags$select(id = "vlc-select",
                                    lapply(vlc_cats, function(v) tags$option(value = v, v))
                        )
               )
      ),
      tags$div(id = "map"),
      tags$div(id = "legend",
               tags$b("Fishing Hours (Log Scale)"),
               tags$div(id = "legend-bar"),
               tags$div(
                 style = "display:flex; justify-content:space-between; font-size:0.8rem; margin-top:3px;",
                 tags$span("Low"), tags$span(format(round(max_hours), big.mark = ","))
               )
      ),
      tags$script(src = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"),
      tags$script(HTML(sprintf("
        const data = %s;
        const logMax = %f;
        const colors = %s;
        const API_KEY = '%s';

        const map = L.map('map', { preferCanvas: true }).setView([55, 5], 5);

        L.tileLayer('https://api.maptiler.com/maps/ocean/{z}/{x}/{y}.png?key=' + API_KEY, {
          attribution: '&copy; MapTiler &copy; OpenStreetMap',
          tileSize: 512,
          zoomOffset: -1,
          crossOrigin: true
        }).addTo(map);

        let activeVlc = 'All Vessels';
        let geoLayer = null;

        function getCol(h) {
          if (!h || h <= 0.05) return 'transparent';
          const val = Math.log1p(h) / logMax;
          const idx = Math.round(val * (colors.length - 1));
          return colors[Math.max(0, Math.min(idx, colors.length - 1))];
        }

        function render() {
          if (geoLayer) map.removeLayer(geoLayer);

          const filtered = data.features.filter(f => f.properties.vesselLengthCategory === activeVlc);

          geoLayer = L.geoJSON({ type: 'FeatureCollection', features: filtered }, {
            style: (f) => ({
              fillColor: getCol(f.properties.fishingHours),
              fillOpacity: 0.85,
              stroke: false
            }),
            onEachFeature: (f, layer) => {
              layer.bindTooltip(
                '<b>' + f.properties.vesselLengthCategory + '</b><br>' +
                'Gear: ' + f.properties.gearCode + '<br>' +
                'Hours: ' + f.properties.fishingHours.toLocaleString(undefined, {maximumFractionDigits: 1})
              );
            }
          }).addTo(map);
        }

        document.getElementById('vlc-select').addEventListener('change', (e) => {
          activeVlc = e.target.value;
          render();
        });

        const bounds = L.geoJSON(data).getBounds();
        if (bounds.isValid()) map.fitBounds(bounds.pad(0.1));

        render();
      ", geojson_str, log_max, toJSON(pal_cols), api_key)))
    )
  )
  
  save_html(html_page, sprintf("pots_and_traps_vms_%d.html", yr))
  cat("Saved", yr, "\n")
}

unlink("temp.geojson")
