#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  # In ambienti "bloccati" (es. sandbox) la libreria utente di R può non essere scrivibile.
  # Installiamo quindi i pacchetti in una libreria locale al progetto.
  local_lib <- file.path(getwd(), ".r_libs")
  dir.create(local_lib, showWarnings = FALSE, recursive = TRUE)
  .libPaths(c(local_lib, .libPaths()))

  req <- c("sf", "dplyr", "readr", "ggplot2", "viridis", "scales")
  to_install <- req[!vapply(req, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(to_install) > 0) {
    install.packages(to_install, repos = "https://cloud.r-project.org", lib = local_lib)
  }

  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(viridis)
  library(scales)
})

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

sf::sf_use_s2(FALSE)

restaurants_url <- "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv"
communes_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"

restaurants_path <- file.path("data", "data_on_french_states.csv")
communes_path <- file.path("data", "communes.geojson")

download_with_retry <- function(url, dest, tries = 5) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("OK: trovato in locale: ", dest)
    return(invisible(dest))
  }

  message("Download: ", url)
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      if (requireNamespace("curl", quietly = TRUE)) {
        curl::curl_download(url, destfile = dest, quiet = TRUE)
      } else {
        download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
      }
      file.exists(dest) && file.info(dest)$size > 0
    }, error = function(e) FALSE)

    if (isTRUE(ok)) return(invisible(dest))

    wait <- min(10, 2^(i - 1))
    message("  tentativo ", i, "/", tries, " fallito; retry tra ", wait, "s")
    Sys.sleep(wait)
  }
  stop("Impossibile scaricare: ", url, "\nDest: ", dest)
}

download_with_retry(restaurants_url, restaurants_path)
download_with_retry(communes_url, communes_path)

# 1) CSV ristoranti (per comune / depcom)
restaurants <- readr::read_delim(
  restaurants_path,
  delim = ";",
  col_types = cols(.default = col_guess()),
  locale = locale(encoding = "UTF-8")
)

if (!all(c("depcom", "nb_equip") %in% names(restaurants))) {
  stop(
    "Il CSV non ha le colonne attese. Trovate: ",
    paste(names(restaurants), collapse = ", ")
  )
}

restaurants <- restaurants %>%
  transmute(
    depcom = as.character(depcom),
    nb_equip = readr::parse_number(nb_equip, locale = locale(decimal_mark = ","))
  )

# 2) GeoJSON (comuni francesi)
communes <- suppressWarnings(sf::read_sf(communes_path, quiet = TRUE))
communes <- tryCatch(sf::st_make_valid(communes), error = function(e) communes)

# Se nel GeoJSON esiste un campo regione, ricaviamo anche i confini regionali
region_candidates <- c(
  "region", "nom_region", "region_name", "lib_reg", "reg_name",
  "code_region", "region_code", "reg_code"
)
region_lower <- tolower(names(communes))
region_idx <- match(region_candidates, region_lower)
region_idx <- region_idx[!is.na(region_idx)][1]
region_col <- if (!is.na(region_idx)) names(communes)[region_idx] else NA_character_

# Deduzione chiave comune (INSEE) per join con depcom
candidate_keys <- c(
  "code", "insee", "insee_com", "code_insee", "codgeo", "codgeo", "id",
  "code_commune", "code_commune_insee", "com", "depcom"
)
key_lower <- tolower(names(communes))
key_idx <- match(candidate_keys, key_lower)
key_idx <- key_idx[!is.na(key_idx)][1]
if (is.na(key_idx)) {
  stop(
    "Non riesco a trovare una colonna chiave nel GeoJSON per fare join con depcom.\n",
    "Colonne disponibili: ", paste(names(communes), collapse = ", ")
  )
}
join_key <- names(communes)[key_idx]
message("Join su: communes$", join_key, "  <-> restaurants$depcom")

communes <- communes %>%
  mutate(
    depcom = as.character(.data[[join_key]])
  ) %>%
  left_join(restaurants, by = "depcom")

# 3) Sud della Francia: crop via bounding box (metropolitana)
#    (approccio semplice e riproducibile: latitudine ~42–46.8 e longitudine ~-6.5–8.5)
south_bbox <- sf::st_bbox(
  c(xmin = -6.5, ymin = 42.0, xmax = 8.5, ymax = 46.8),
  crs = sf::st_crs(4326)
)

communes_south <- communes %>%
  sf::st_transform(4326) %>%
  sf::st_crop(south_bbox) %>%
  filter(!sf::st_is_empty(geometry)) %>%
  mutate(nb_equip = dplyr::coalesce(nb_equip, 0))

regions_south <- NULL
if (!is.na(region_col)) {
  regions_south <- communes %>%
    sf::st_transform(4326) %>%
    filter(!is.na(.data[[region_col]])) %>%
    group_by(region = .data[[region_col]]) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    suppressWarnings(sf::st_make_valid()) %>%
    sf::st_crop(south_bbox) %>%
    filter(!sf::st_is_empty(geometry))
}

if (nrow(communes_south) == 0) {
  stop("Il crop del Sud ha prodotto 0 geometrie. Controlla CRS o bbox.")
}

# 4) Mappa di “densità” (numero di ristoranti per comune)
#    Uso scala log per gestire coda lunga.
plot_title <- "Concentrazione ristoranti — Sud della Francia"
plot_subtitle <- "Numero di ristoranti per comune (dati R Graph Gallery) su confini comunali (France-GeoJSON)"

p <- ggplot(communes_south) +
  geom_sf(aes(fill = nb_equip), color = "#ffffff", linewidth = 0.05, alpha = 1) +
  { if (!is.null(regions_south) && nrow(regions_south) > 0)
    geom_sf(data = regions_south, fill = NA, color = "#2b2b2b", linewidth = 0.35, alpha = 0.9)
    else NULL } +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    trans = scales::log1p_trans(),
    breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500),
    labels = scales::label_number(),
    name = "Ristoranti"
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = "Fonti: holtzy/R-graph-gallery (data_on_french_states.csv) • gregoiredavid/france-geojson (communes.geojson)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, color = "#2b2b2b", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#4a4a4a", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#6a6a6a", margin = margin(t = 10)),
    legend.position = "bottom",
    legend.key.width = grid::unit(18, "mm"),
    legend.key.height = grid::unit(3.5, "mm")
  )

out_png <- file.path("output", "densita_ristoranti_sud_francia.png")
ggsave(out_png, p, width = 11, height = 7, dpi = 200, bg = "white")

message("Creato: ", out_png)

