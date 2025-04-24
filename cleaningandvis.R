library(tidyverse)
library(terra)
library(dplyr)
library(sf)

#forb data to do the ML 
r = rast(c("/Users/chippymarx/Downloads/sl_forb_current.tiff",
           '/Users/chippymarx/Downloads/sl_forb_mid_585.tiff',
           'data/sl_forb_late_585.tiff'))
#gives metadata
extract(r, pts)

#use extract for long and lat 

Invasive_data <- read_csv("data/CO_INVASIVE_SUBPLOT_SPP.csv")
Location_data <- read_csv("data/CO_PLOTGEOM.csv")
Invasive_withLoc <- inner_join(Invasive_data, Location_data, by = c("PLT_CN" = 'CN'))
colnames(Invasive_withLoc)

r = rast("/Users/chippymarx/Downloads/sl_forb_current.tiff") 

TMP <- Invasive_withLoc %>%
  select(PLT_CN, LAT, INVYR.x, LON, VEG_FLDSPCD, COVER_PCT) %>%
  filter(INVYR.x == 2007) %>%                
  drop_na()      

pts = TMP %>% 
  st_as_sf(coords = c("LON", 'LAT'), remove = FALSE, crs = 4326) %>% 
  distinct()

colnames(Invasive_withLoc)
mapview::mapview(pts)


pts$sl_forb_current = extract(r, pts)

bb = st_bbox(pts) %>% 
  sf::st_as_sfc() %>% 
  st_as_sf() %>% 
  st_transform(crs(r))

cc = crop(r, bb)

plot(cc)
plot(project(vect(pts), crs(r)), add = TRUE, col = "red", pch = 16)

plot(r)

mapview::mapview(r)


plot(pts$sl_forb_current)


#sites we will select 
site_ids = c(12224885010690, 12221198010690, 12223803010690, 12206792010690, 12222071010690, 1223416010690,	12225851010690,	12232600010690, 12212814010690, 	12214073010690  ) 



r = rast(c("/Users/chippymarx/Downloads/sl_forb_current.tiff",
           '/Users/chippymarx/Downloads/sl_forb_mid_585.tiff',
           'data/sl_forb_late_585.tiff'))

ex_pts = pts %>% 
  filter(PLT_CN %in% site_ids) %>% 
  #distinct() %>% 
  st_transform(crs(r))


v <-  extract(r, ex_pts)
ex_Pts_both = bind_cols(ex_pts, v)

df_long <- ex_Pts_both %>%
  select(ID, COVER_PCT, sl_forb_current...10, sl_forb_mid_585, sl_forb_late_585) %>%
  pivot_longer(
    cols = c(COVER_PCT, sl_forb_current...10, sl_forb_mid_585, sl_forb_late_585),
    names_to = "variable",
    values_to = "value"
  )
print(df_long)
ggplot(df_long, aes(x = factor(ID), y = value, color = variable, group = variable)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  geom_line(aes(group = variable), position = position_jitter(width = 0.2), alpha = 0.3) +
  labs(
    x = "Site ID",
    y = "Value",
    title = "Invasive Cover by Scenario and Site",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(tidyverse)

# Reshape the data
df_long <- ex_Pts_both %>%
  select(ID, COVER_PCT, `sl_forb_current...10`, sl_forb_mid_585, sl_forb_late_585) %>%
  pivot_longer(
    cols = c(COVER_PCT, `sl_forb_current...10`, sl_forb_mid_585, sl_forb_late_585),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
                           "sl_forb_current...10" = "sl_forb_current"  # optional: clean up name
  ))

# Create faceted bar plot
ggplot(df_long, aes(x = factor(ID), y = value, fill = variable)) +
  geom_col() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Site ID",
    y = "Value",
    title = "Invasive Cover by Site and Scenario"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # remove legend since it's in facets
  )
#specify tunable parameters 
xgb_mod_tuned <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

#create new workflow 
xgb_wf_tuned <- workflow() %>%
  add_model(xgb_mod_tuned) %>%
  add_recipe(forb_recipe)

#define tunable grid
xgb_grid <- grid_regular(
  trees(range = c(100, 1000)),
  learn_rate(range = c(0.01, 0.3)),
  tree_depth(range = c(2, 10)),
  levels = 5
)
#tune model with cross-validation
xgb_tuned_results <- tune_grid(
  xgb_wf_tuned,
  resamples = ex_both_cv,
  grid = xgb_grid,
  metrics = my_metrics,
  control = control_grid(save_pred = TRUE)
)


