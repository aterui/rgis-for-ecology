
rm(list = ls())
source("code/set_library.R")

# read data ---------------------------------------------------------------

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
