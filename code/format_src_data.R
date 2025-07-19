
rm(list = ls())
source("code/set_library.R")

# read data ---------------------------------------------------------------

## NC DEQ data ####
df_fish0 <- list.files("data/src",
                       pattern = "NCIBI",
                       full.names = TRUE) %>% 
  lapply(FUN = function(x) suppressMessages(read_csv(x))) %>% 
  bind_rows() %>% 
  rename_with(.fn = function(x) {
    x %>% 
      str_to_lower() %>% 
      str_replace_all("\\s", "_")
  }) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>% 
  mutate(across(.cols = where(is.character),
                .fns = str_to_lower)) %>% 
  mutate(scientific_name = str_to_sentence(scientific_name) %>% 
           str_replace("\\ssp\\.\\s[cC]\\.?f\\.?\\s", " ") %>% 
           str_replace("nigrum/olmstedi", "nigrum")) %>% 
  filter(stationid != "qf120") # qf57 & qf120 has the same coordinate; retain qf120


df_fish <- df_fish0 %>% 
  distinct(stationid,
           date,
           longitude,
           latitude) %>% 
  group_by(stationid) %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  left_join(df_fish0,
            by = c("stationid",
                   "date",
                   "longitude",
                   "latitude")) %>% 
  mutate(year = format(date, "%Y") %>% 
           as.numeric(),
         common_name = str_to_sentence(common_name)
  ) %>% 
  dplyr::select(site_id = stationid,
                year,
                date,
                lon = longitude,
                lat = latitude,
                scientific_name,
                common_name,
                count = no_coll)

write_csv(df_fish,
          "data/data_fish_nc.csv")

## finsync data ####

# df_fin <- finsyncR::getFishData(taxonLevel = "Species",
#                                 agency = c("USGS","EPA"))
# 
# saveRDS(df_fin, file = "data/src/data_finsync_src.rds")

df_finsync0 <- readRDS("data/src/data_finsync_src.rds") %>% 
  pivot_longer(cols = Luxilus.cornutus:Salvelinus.namaycush, 
               names_to = "latin",
               values_to = "presence") %>% 
  rename_with(.fn = function(x) {
    x %>% 
      str_replace_all("(?<=[a-z])([A-Z])", "_\\1") %>% 
      str_to_lower()
  }) %>% 
  mutate(across(.cols = where(is.character),
                .fns = str_to_lower)) %>% # confirmed this operation does not change # of unique elements in each column
  filter(presence > 0) %>% 
  group_by(site_number) %>% 
  filter(collection_date == max(collection_date)) %>% 
  ungroup() %>% 
  group_by(site_number,
           collection_date,
           latin) %>% 
  slice(1) %>% # remove duplicated records of presence (captured by different methods)
  ungroup()

## standardize geospatial datum
df_site <- df_finsync0 %>% 
  distinct(site_number,
           collection_date,
           coordinate_datum,
           longitude_dd,
           latitude_dd)

v_crs <- sort(unique(df_site$coordinate_datum))
epsg <- c(4267, 4269, 4135) # NAD27, NAD83, Old Hawaiian
names(epsg) <- v_crs

sf_site <- foreach(i = v_crs,
                   .combine = bind_rows) %do% {
                     
                     df_i <- df_site %>%
                       filter(coordinate_datum == i) %>% 
                       st_as_sf(crs = epsg[which(names(epsg) == i)],
                                coords = c("longitude_dd",
                                           "latitude_dd")) %>% 
                       st_transform(crs = 4326) %>% 
                       dplyr::select(-coordinate_datum)
                     
                   }

df_xy <- sf_site %>% 
  mutate(lon = st_coordinates(.)[,"X"],
         lat = st_coordinates(.)[,"Y"]) %>% 
  as_tibble() %>% 
  dplyr::select(site_number, lon, lat)

## append standardize lat lon in WGS84
df_finsync <- df_finsync0 %>% 
  left_join(df_xy) %>% 
  dplyr::select(site_id = site_number,
                year = collection_year,
                date = collection_date,
                lon,
                lat,
                latin,
                presence) %>% 
  mutate(latin = str_replace_all(latin, "\\.", " ") %>% 
           str_to_sentence(),
         count = NA,
         source = "finsyncR",
         site_id = paste0("finsync_", site_id),
         continent = "na")

saveRDS(df_finsync, "data/data_finsync.rds")

## subset finsync data by NC polygons
sf_nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>% 
  dplyr::select(NULL) %>% 
  st_transform(crs = 4326) %>% 
  mutate(fid = row_number())

sf_finsync <- df_finsync %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

v_site <- sf_finsync %>% 
  st_join(sf_nc) %>% 
  drop_na(fid) %>% 
  pull(site_id) %>% 
  unique()

df_finsync_nc <- df_finsync %>% 
  filter(site_id %in% v_site) %>% 
  dplyr::select(-count, -source, -continent)

sf_site_nc <- sf_finsync %>% 
  filter(site_id %in% v_site) %>% 
  group_by(site_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(site_id)

write_csv(df_finsync_nc,
          "data/data_finsync_nc.csv")

saveRDS(sf_site_nc,
        "data/sf_finsync_nc.rds")


# sf example --------------------------------------------------------------

# Get roads in a specific NC county
sf_str <- tigris::linear_water(state = "NC",
                               county = "Guilford",
                               year = 2021) %>% 
  dplyr::select(NULL) %>% 
  st_transform(crs = 4326) %>% 
  mutate(fid = row_number() %>% 
           str_pad(width = 6, pad = "0") %>% 
           paste0("fid", .))

saveRDS(sf_str, "data/sf_stream.rds")

# Get roads in a specific NC county
sf_nc_county <- st_read(system.file("shape/nc.shp", package = "sf"),
                        quiet = TRUE) %>% 
  rename_with(str_to_lower) %>% 
  dplyr::select(county = name) %>% 
  mutate(county = str_to_lower(county)) %>% 
  st_transform(crs = 4326)

st_write(sf_nc_county, 
         dsn = "data/sf_nc_county.shp", 
         append = FALSE)

saveRDS(sf_nc_county, "data/sf_nc_county.rds")


# terra example -----------------------------------------------------------

library(wrapbox)

## upstream watershed area
spr_upa <- list.files("E://gis//merithydro",
                      full.names = TRUE,
                      pattern = get_key(sf_nc_county)) %>% 
  {.[str_detect(., "_upa")]} %>% 
  lapply(rast) %>% 
  terra::sprc() %>% 
  terra::merge() %>% 
  terra::crop(st_bbox(sf_nc_county))

varnames(spr_upa) <- names(spr_upa) <- "upa"

writeRaster(spr_upa,
            "data/spr_upa_nc.tif",
            overwrite = TRUE)

## elevation
spr_dem <- list.files("E://gis//merithydro",
                      full.names = TRUE,
                      pattern = get_key(sf_nc_county)) %>% 
  {.[str_detect(., "_dem")]} %>% 
  lapply(rast) %>% 
  terra::sprc() %>% 
  terra::merge() %>% 
  terra::crop(st_bbox(sf_nc_county))

varnames(spr_dem) <- names(spr_dem) <- "dem"

writeRaster(spr_dem,
            "data/spr_dem_nc.tif",
            overwrite = TRUE)

## climate data
spr_chelsa_us <- list.files("data/src",
                            full.names = TRUE,
                            pattern = "CHELSA") %>% 
  lapply(FUN = function(x) {
    
    y <- rast(x)
    names(y) <- ifelse(str_detect(names(y), "_bio1_"),
                       "temperature",
                       "precipitation")
    
    return(y)
    
  }) %>% 
  rast() %>% 
  terra::crop(c(-125, -66.9, 24.5, 49.5))

spr_tmp_us <- spr_chelsa_us$temperature
spr_prec_us <- spr_chelsa_us$precipitation

writeRaster(spr_tmp_us,
            "data/spr_tmp_us.tif",
            overwrite = TRUE)

writeRaster(spr_prec_us,
            "data/spr_prec_us.tif",
            overwrite = TRUE)
