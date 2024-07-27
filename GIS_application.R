#### Load Necessary Libraries ####

library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(exactextractr)
library(tidyr)
library(raster)
library(xml2)
library(scales)
library(RColorBrewer)
library(leaflet)
library(terra)
library(tmap)
library(hrbrthemes)
library(gridExtra)


# Load the catchment data (gpkg)
catchments <- st_read("catchments_germany.gpkg")
class(catchments)

# Load the nutrient data (rds)
nutrient_data <- readRDS("waterbase_prepared.rds")
class(nutrient_data)


# Load the cattle raster data

dem <- raster("cattle_data.tif")

# load the geometry data for germany
germany <- st_read("gadm36_DEU_3_pk.gpkg")
class(germany)


library(geodata)

# Filter 
Lower_Saxony <- filter(germany, NAME_1 == "Niedersachsen")
class(Lower_Saxony)
class(dem)
Lower_Saxony_dem <- mask(dem, Lower_Saxony)
Lower_Saxony_dem <- crop(Lower_Saxony_dem, Lower_Saxony)


tm_shape(Lower_Saxony_dem) +
  tm_raster() 

## Variability of nitrate explained by cattle 

# Extract the nitrate value from nutrient data

nitrate <- nutrient_data %>%
  filter(compound == "Nitrate") %>%
  dplyr::select(value, longitude, latitude)

# rename value Nitrate conc.

nitrate <- nitrate %>%
  rename("Nitrate conc." = value)

## remove NA values from the nitrate conc. 
nitrate <- dplyr::filter(nitrate, !is.na(`Nitrate conc.`))

## Remove missing values in coordinates

nitrate <- dplyr::filter(nitrate, !is.na(longitude))

# turn nitrate data into sf object

nitrate_sf <- st_as_sf(nitrate, coords = c("longitude", "latitude"), crs = 4326)
Lower_Saxony
class(nitrate_sf)
## Aggregate the nitrate_sf using the Lower_Saxony geometry with the function mean
library(mapview)
nitrate_sf_agg <- aggregate(nitrate_sf, by = Lower_Saxony, FUN = mean)
mapview(nitrate_sf_agg,  zcol = "Nitrate conc.")


## Creating base map for the tmap
library(maptiles)

Lower_Saxony_dem <- rast(Lower_Saxony_dem)
base_map <- get_tiles(x = Lower_Saxony_dem,
                      provider = "Esri.WorldImagery",
                      zoom = 10,
                      crop = TRUE)


## Variability of phosphate explained by cattle density

# Extract the phosphate value from nutrient data

phosphate <- nutrient_data %>%
  filter(compound == "Phosphate") %>%
  dplyr::select(value, longitude, latitude)

# rename value Phosphate conc.

phosphate <- phosphate %>%
  rename("Phosphate conc." = value)

## remove NA values from the phosphate conc.
phosphate <- dplyr::filter(phosphate, !is.na(`Phosphate conc.`))

## Remove missing values in coordinates

phosphate <- dplyr::filter(phosphate, !is.na(longitude))

# turn phosphate data into sf object

phosphate_sf <- st_as_sf(phosphate, coords = c("longitude", "latitude"), crs = 4326)

## Aggregate the phosphate_sf using the Lower_Saxony geometry with the function mean

phosphate_sf_agg <- aggregate(phosphate_sf, by = Lower_Saxony, FUN = mean)
mapview(phosphate_sf_agg,  zcol = "Phosphate conc.")

class(Lower_Saxony_dem)
plot(Lower_Saxony_dem)
class(catchments)

Lower_Saxony_dem.ext <- ext(Lower_Saxony_dem) # get the extent of the raster

# convert extend to bounding box usable by sf

Lower_Saxony_dem.bbox <- st_bbox(Lower_Saxony_dem.ext) |> st_as_sfc() |> st_as_sf(crs = 4326)

# Transform the catchments to same bb as the Lower_Saxony_dem

catchments <- st_transform(catchments, crs = 4326)

# Subset catchments to only those that intersect with the Lower_Saxony_dem

catchment_filtered <- st_intersection(catchments, Lower_Saxony_dem.bbox)

## extract the raster values from the cells within the different catchment polygons with the extract() function.
Lower_Saxony_dem.ext <- terra::extract(Lower_Saxony_dem, catchment_filtered)
head(Lower_Saxony_dem.ext)

## Calculating mean cattle density per id
Lower_Saxony_dem.ext1 <- Lower_Saxony_dem.ext |> 
  group_by(ID) |> 
  summarize(mean_cattle = mean(cattle_data))

catchment_filtered$mean_cattle_id <- Lower_Saxony_dem.ext1$mean_cattle

## Remove coordinates with mean_cattle_id NA

catchment_filtered <- dplyr::filter(catchment_filtered, !is.na(mean_cattle_id))
class(catchment_filtered)
class(nitrate_sf_agg)
tmap_mode("plot")

lower_saxony_basic <- tm_shape(nitrate_sf_agg) +
  tm_fill()

tmap_save(lower_saxony_basic, "lower_saxony_basic.png")

# Variability of nitrate explained by average cattle density per catchment in lower Saxony

mean_nitrate <- tm_shape(base_map) +
  tm_rgb() +
  tm_shape(catchment_filtered) +
  tm_fill(col = "mean_cattle_id", title = "Mean cattle density per catchment", n = 4, style = "quantile", palette="-RdBu") +
  tm_facets(by = "namebasin", ncol = 2) +
  tm_shape(nitrate_sf_agg) + 
  tm_dots(size = "Nitrate conc.", col = "Nitrate conc.", title = "Average Nitrate Concentration [mg/L]", palette=c(B='yellow', H='blue',L= "#E69F00", N='green'), n = 4) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE, 
            main.title = "Mean variability of nitrate concentration explained by mean of cattle per catchment area in Lower Saxony",
            main.title.size = 1) 

mean_nitrate
tmap_save(mean_nitrate, "mean_nitrate.png")


# Variability of phosphate explained by average cattle density per catchment in lower Saxony
tmap_mode("view")
mean_phosphate <- tm_shape(base_map) +
  tm_rgb()+ 
  tm_shape(catchment_filtered) +
  tm_fill(col = "mean_cattle_id", title = "Mean cattle density per catchment", n = 4, style = "quantile", palette="-RdBu", alpha = 0.5) +
  tm_facets(by = "namebasin", ncol = 2) +
  tm_shape(phosphate_sf_agg) + 
  tm_dots(size = "Phosphate conc.", col = "Phosphate conc.", title = "Average Phosphate Concentration [mg/L]", palette=c(B='yellow', H='blue',L= "#E69F00", N='green'), n = 4) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE, 
            main.title = "Mean variability of phosphate concentration explained by mean of cattle per catchment area in Lower Saxony",
            main.title.size = 1) 

mean_phosphate

tmap_save(mean_phosphate, "mean_phosphate.png")

class(catchment_filtered)

################################################################################
## Detrminig the agriculture and urban area cells in the lower saxony catchment
################################################################################
library(pacman)
p_load(sf, spatstat, mapview, dplyr, ggplot2, magrittr, terra, raster, maptools, data.table)

germany_rast <- rast("germany.tif")
germany_xml <- xml2::read_xml("germany.tif.aux.xml")
landcover_not_csv <- read_csv("clc_vocabulary.csv")
class(germany_rast)
class(germany_xml)
class(landcover_not_csv)

head(landcover_not_csv)

class(germany_rast)
class(catchment_filtered)
class(Lower_Saxony)

# Extract mapping information from the xml file
library(xml2)

value_nodes <- xml_find_all(germany_xml, "//Row/F[1]")
notation_nodes <- xml_find_all(germany_xml, "//Row/F[6]")
r_nodes <- xml_find_all(germany_xml, "//Row/F[3]")
g_nodes <- xml_find_all(germany_xml, "//Row/F[4]")
b_nodes <- xml_find_all(germany_xml, "//Row/F[5]")

# Extract values and notations
values <- as.integer(xml_text(value_nodes))
notations <- as.integer(xml_text(notation_nodes))
r_values <- as.numeric(xml_text(r_nodes))
g_values <- as.numeric(xml_text(g_nodes))
b_values <- as.numeric(xml_text(b_nodes))

# Create a mapping table
mapping_table <- data.frame(
  value = values,
  Notation = notations
)

mapping_table <- mapping_table %>%
  mutate(
    r = r_values,
    g = g_values,
    b = b_values
  )

# Merge with landcover notation
mapping_table <- mapping_table %>%
  left_join(landcover_not_csv, by = "Notation")

mapping_table <- mapping_table %>%
  mutate(color = rgb(r, g, b, maxColorValue = 1))

class(mapping_table)
class(landcover_not_csv)
catchments

# Ensure the reclassification matrix is numeric
landcover_dataframe <- mapping_table %>%
  dplyr::select(value, Notation) %>%
  as.matrix()


# Perform the reclassification using terra
landcover_mapped <- classify(germany_rast, landcover_dataframe)
class(landcover_mapped)

## Assign the crs to the catchment_filtered
catchment_filtered <- st_transform(catchment_filtered, crs = 4326)

# Ensuring that the catchment_filtered is in the same extent as the germany raster data

catchment_filtered <- st_transform(catchment_filtered, crs = crs(landcover_mapped))

## Filtering the germany raster data which is in the same extent as the catchment_filtered in the Lower Saxony
germany_rast1 <- mask(landcover_mapped, catchment_filtered)
germany_rast1 <- crop(germany_rast1, catchment_filtered)
class(germany_rast1)
plot(germany_rast1)

germany_rast_df <- as.data.frame(germany_rast1, xy = TRUE)
class(germany_rast_df)

## Rename LABEL3 column to Notation
germany_rast_df <- germany_rast_df %>%
  rename(Notation = LABEL3)

## Merge germany_rast_df with the mapping table by Notation and keep only the Notation which is available in the germany_rast_df
mapping_table <- mapping_table %>%
  filter(Notation %in% germany_rast_df$Notation)

# Now merge the germany_rast_df with the mapping table by notation

germany_rast_df_merge <- merge(germany_rast_df, mapping_table, by = "Notation", all.x = TRUE)

head(germany_rast_df_merge)


###########
# Ensure the reclassification matrix is numeric
landcover_reclass_matrix <- mapping_table %>%
  dplyr::select(value, Notation) %>%
  as.matrix()

# Perform the reclassification using terra
landcover_mapped <- classify(germany_rast, landcover_reclass_matrix)

# Verify the reclassified raster
print(landcover_mapped)

if (!identical(st_crs(catchment_filtered), crs(landcover_mapped))) {
  catchments2 <- st_transform(catchment_filtered, crs = crs(landcover_mapped))
}

# Extract the dominant landcover type for each catchment
landcover_extract <- exact_extract(landcover_mapped, catchments2, fun = "mode", append_cols = c("h1_id","h2_id", "h3_id", "h4_id", "h5_id", "h6_id", "h7_id"))

# Convert the extracted data to a data frame
landcover_extract_df <- as.data.frame(landcover_extract)
colnames(landcover_extract_df) <- c("h1_id","h2_id", "h3_id", "h4_id", "h5_id", "h6_id", "h7_id", "Notation")

# Merge the extracted landcover data with the notation file
landcover_extract_df <- merge(landcover_extract_df, mapping_table, by.x = "Notation", by.y = "Notation")

# Convert catchments data to a data frame
catchments_df <- as.data.frame(catchment_filtered)

# Merge the landcover data with the catchments data
catchments_landcover <- merge(catchments_df, landcover_extract_df, by = c("h1_id", "h2_id", "h3_id", "h4_id", "h5_id", "h6_id", "h7_id"), all.x = TRUE)


#####################################
## Filter Nutrient Data for Germany ##
#####################################

# Check for missing values in coordinates
missing_coords <- is.na(nutrient_data$longitude) | is.na(nutrient_data$latitude)

# Filter out rows with missing coordinates
nutrient_data <- nutrient_data[!missing_coords, ]

# Convert nutrient data to spatial points
nutrient_data_sf <- st_as_sf(nutrient_data, coords = c("longitude", "latitude"), crs = 4326)

# Filter nutrient data by country code "DE"
nutrient_data <- nutrient_data_sf %>%
  filter(country == "DE")

## load the gadm data for germany
admin_regions <- readRDS("gadm_ger.rds")

# Filter the admin_regions data for lower saxony

lower_saxony <- admin_regions %>%
  filter(VARNAME_1 == "Lower Saxony")

# Match the CRS system of catchments with Lower Saxony
if (!identical(st_crs(catchments), st_crs(lower_saxony))) {
  lower_saxony <- st_transform(lower_saxony, crs = st_crs(catchments))
}

# Catchments within Lower Saxony
catchments_lower_saxony <- st_intersection(catchments, lower_saxony)

## Spatial Join:  Perform Spatial Join of nutrient data with Catchments ##

if (!identical(st_crs(nutrient_data), st_crs(catchments_lower_saxony))) {
  # Reproject nutrient_data_germany to match catchments CRS
  nutrient_data <- st_transform(nutrient_data, crs = st_crs(catchments_lower_saxony))
}

# Perform spatial join between nutrient data points and catchments
nutrient_catchment <- st_join(nutrient_data, catchments_lower_saxony, join = st_intersects)


## Separate the Nutrient Data into Nitrate and Phosphate Datasets ##

nitrate_data <- nutrient_catchment %>% filter(compound == "Nitrate")
phosphate_data <- nutrient_catchment %>% filter(compound == "Phosphate")


## Filter Nitrate & Phosphate Data for Only Lower Saxony ##

# Ensure the CRS of Lower Saxony matches that of nutrient data
if (!identical(st_crs(nitrate_data), st_crs(lower_saxony))) {
  lower_saxony <- st_transform(lower_saxony, crs = st_crs(nitrate_data))
}

# Filter nitrate and phosphate data for points within Lower Saxony
nitrate_lower_saxony <- nitrate_data[st_within(nitrate_data, lower_saxony, sparse = FALSE), ]
phosphate_lower_saxony <- phosphate_data[st_within(phosphate_data, lower_saxony, sparse = FALSE), ]
class(nitrate_lower_saxony)

# Convert nitrate and phosphate data to data frames
nitrate_df <- as.data.frame(nitrate_lower_saxony)
phosphate_df <- as.data.frame(phosphate_lower_saxony)


# Merge nitrate and phosphate data with catchments and landcover data
nitrate_landcover <- merge(nitrate_df, catchments_landcover, by = c("h1_id", "h2_id", "h3_id", "h4_id", "h5_id", "h6_id", "h7_id"), all.x = TRUE)
phosphate_landcover <- merge(phosphate_df, catchments_landcover, by = c("h1_id", "h2_id", "h3_id", "h4_id", "h5_id", "h6_id", "h7_id"), all.x = TRUE)

# Calculate mean nitrate value for each landcover category
nitrate_landcover_summary <- nitrate_landcover %>%
  group_by(h1_id, h2_id, h3_id, h4_id, h5_id, h6_id, h7_id,level1category, level2category,mean_cattle_id, Label, geometry) %>%
  summarise(mean_nitrate = mean(value.x, na.rm = TRUE))

# Calculate mean phosphate value for each landcover category
phosphate_landcover_summary <- phosphate_landcover %>%
  group_by(h1_id, h2_id, h3_id, h4_id, h5_id, h6_id, h7_id, level1category, level2category, mean_cattle_id, Label, geometry) %>%
  summarise(mean_phosphate = mean(value.x, na.rm = TRUE))

# Combine nitrate and phosphate summaries
nutrient_landcover_summary <- merge(nitrate_landcover_summary, phosphate_landcover_summary, by = c("h1_id", "h2_id", "h3_id", "h4_id", "h5_id", "h6_id", "h7_id", "level1category", "level2category", "mean_cattle_id", "geometry"))

# Print the summary
print(nutrient_landcover_summary)

# Convert to sf objects
nitrate_landcover_summary_sf <- st_as_sf(nitrate_landcover_summary)

nitrate_landcover_summary_sf <- nitrate_landcover_summary_sf %>%
  left_join(mapping_table)  

phosphate_landcover_summary_sf <- st_as_sf(phosphate_landcover_summary)

phosphate_landcover_summary_sf <- phosphate_landcover_summary_sf %>%
  left_join(mapping_table)  
class(nitrate_landcover_summary_sf)

# convert nitrate_landcover_summary_sf to sf object
nitrate_landcover_summary_sf <- st_as_sf(nitrate_landcover_summary)
class(nitrate_landcover_summary_sf)

## Removing missing coordinates from the nitrate landcover summary

nitrate_landcover_summary_sf <- dplyr::filter(nitrate_landcover_summary_sf, !is.na(mean_nitrate))
phosphate_landcover_summary_sf <- dplyr::filter(phosphate_landcover_summary_sf, !is.na(mean_phosphate))


# removing NA values in the label1category column in the nitrate_landcover_summary_sf

nitrate_landcover_summary_sf <- dplyr::filter(nitrate_landcover_summary_sf, !is.na(level1category))

# removing NA values in the label1category column in the phosphate_landcover_summary_sf

phosphate_landcover_summary_sf <- dplyr::filter(phosphate_landcover_summary_sf, !is.na(level1category))
  
################

## Mean variability of cattle and phosphate explained by landcover in Lower Saxony
tmap_mode("plot")
mean_cat_phos_land <- tm_shape(base_map) +
  tm_rgb() +
  tm_shape(catchment_filtered) +
  tm_fill(col = "mean_cattle_id", title = "Mean cattle density per catchment", n = 4, style = "quantile", palette="-RdBu") +
  tm_shape(phosphate_landcover_summary_sf) +
  tm_dots(size = "mean_phosphate", col = "mean_phosphate", title = "Mean Phosphate Concentration [mg/L]", palette=c(B='yellow', H='blue',L= "#E69F00", N='green'), n = 4) +
  tm_facets(by = "level1category", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Mean variability of cattle and phosphate explained by landcover in Lower Saxony",
            main.title.size = 1)
mean_cat_phos_land

tmap_save(mean_cat_phos_land, "mean_cat_phos_land.png")

# Mean variability of cattle and nitrate explained by landcover in Lower Saxony

mean_cat_nit_land <- tm_shape(base_map) +
  tm_rgb() +
  tm_shape(catchment_filtered) +
  tm_fill(col = "mean_cattle_id", title = "Mean cattle density per catchment", n = 4, style = "quantile", palette="-RdBu") +
  tm_shape(nitrate_landcover_summary_sf) +
  tm_dots(size = "mean_nitrate", col = "mean_nitrate", title = "Mean Nitrate Concentration [mg/L]", palette=c(B='yellow', H='blue',L= "#E69F00", N='green'), n = 4) +
  tm_facets(by = "level1category", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Mean variability of cattle and nitrate explained by landcover in Lower Saxony",
            main.title.size = 1)
mean_cat_nit_land

tmap_save(mean_cat_nit_land, "mean_cat_nit_land.png")

MyPalette <- c("#cbc9e2", "#9e9ac8", "#6a51a3", "red")

# Mean variability of cattle and nitrate explained by landcover and namebasin in Lower Saxony
cat_nit_lan_bas <- tm_shape(catchment_filtered) +
  tm_fill(col = "namebasin") +
  tm_shape(catchment_filtered) +
  tm_fill(col = "mean_cattle_id", title = "Mean cattle density per catchment", n = 4, style = "quantile", palette= MyPalette, alpha = 0.2) +
  tm_shape(nitrate_landcover_summary_sf) +
  tm_dots(size = "mean_nitrate", col = "mean_nitrate", title = "Mean Nitrate Concentration [mg/L]", palette = "-viridis", n = 4, style = "quantile",
          popup.vars =  "cattle_per_catchment") +
  tm_facets(by = "level1category", ncol = 2) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Mean variability of cattle and nitrate explained by landcover and basin in Lower Saxony",
            main.title.size = 1)

cat_nit_lan_bas

tmap_save(cat_nit_lan_bas, "cat_nit_lan_bas.png")

# Mean variability of cattle and phosphate explained by landcover and namebasin in Lower Saxony

cat_phos_lan_bas <- tm_shape(catchment_filtered) +
  tm_fill(col = "namebasin") +
  tm_shape(catchment_filtered) +
  tm_fill(col = "mean_cattle_id", title = "Mean cattle density per catchment", n = 4, style = "quantile", palette= MyPalette, alpha = 0.2) +
  tm_shape(phosphate_landcover_summary_sf) +
  tm_dots(size = "mean_phosphate", col = "mean_phosphate", title = "Mean Phosphate Concentration [mg/L]", palette = "-viridis", n = 4, style = "quantile",
          popup.vars =  "cattle_per_catchment") +
  tm_facets(by = "level1category", ncol = 2) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Mean variability of cattle and phosphate explained by landcover and basin in Lower Saxony",
            main.title.size = 1)
cat_phos_lan_bas

tmap_save(cat_phos_lan_bas, "cat_phos_lan_bas.png")

############################################################################################################

# Linear regression for changing nitrate concentration with cattle density
library(sf)
library(tidyverse)
library(tidycensus)
library(corrr)
library(tmap)
library(spdep)
library(tigris)
library(rmapshaper)
library(flextable)
library(car)
library(spatialreg)
library(stargazer)

class(nitrate_landcover_summary_sf)

# changing column names Nitrate conc. to mean_nitrate in the nitrate_sf_agg

nitrate_sf_agg <- nitrate_sf_agg %>%
  rename(mean_nitrate = `Nitrate conc.`)

# Drop geometry column from the nitrate_landcover_summary_sf
class(nitrate_landcover_summary_sf)
nitrate_landcover_summary_sf_lm <- nitrate_landcover_summary_sf %>%
  dplyr::select(-geometry) %>%
  as.data.frame()
class(nitrate_landcover_summary_sf)

nitrate_sf_agg_lm <- as.data.frame(nitrate_sf_agg)
class(nitrate_sf_agg_lm)
## Merging the nitrate_sf_agg with the nitrate_landcover_summary_sf by st_joins

nitrate_landcover_summary_sf_lm <- merge(nitrate_sf_agg_lm, nitrate_landcover_summary_sf_lm, by = "mean_nitrate", all = TRUE)

## Convert nitrate_landcover_summary_sf_lm to sf object

nitrate_landcover_summary_sf_lm <- st_as_sf(nitrate_landcover_summary_sf_lm)

## Removing rows containig NA values in the nitrate_landcover_summary_sf_lm

catchments_nitrate1 <- st_join(catchments_landcover1, nitrate_lower_saxony, join = st_intersects)


## Omitting the NA values in the catchments_nitrate1

catchments_nitrate1 <- dplyr::filter(catchments_nitrate1, !is.na(mean_cattle_id))
catchments_nitrate1 <- dplyr::filter(catchments_nitrate1, !is.na(value.y))
catchments_nitrate1 <- dplyr::filter(catchments_nitrate1, !is.na(level1category))


nitrate_lm1 <- lm(value.y ~ mean_cattle_id*level1category, data = catchments_nitrate1)
summary(nitrate_lm1) ## nitrate concentration decreases with cattle density


## predict the nitrate concentration

catchments_nitrate1$nitrate_pred <- predict(nitrate_lm1)


ggplot() + 
  geom_histogram(mapping = aes(x=resid(nitrate_lm1))) +
  xlab("nitrate model residual")
qqPlot(nitrate_lm1)
plot(nitrate_lm1, which = 1)

## Plot the predicted nitrate concentration vs. mean cattle density

nitrate.lm1 <- ggplot(catchments_nitrate1, aes(x = mean_cattle_id, y = nitrate_pred, color = level1category)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Predicted nitrate concentration vs. mean cattle density",
       x = "Mean cattle density per catchment",
       y = "Predicted nitrate concentration [mg/L]") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

nitrate.lm1

ggsave("nitrate.lm1final.png", nitrate.lm1, width = 10, height = 6, units = "in", dpi = 300)




# Linear regression for changing phosphate concentration with cattle density

# Omitting the NA values in the catchments_phosphate1

catchments_phosphate1 <- dplyr::filter(catchments_landcover1, !is.na(mean_cattle_id))
catchments_phosphate1 <- dplyr::filter(catchments_phosphate1, !is.na(value))
catchments_phosphate1 <- dplyr::filter(catchments_phosphate1, !is.na(level1category))

phosphate_lm1 <- lm(value ~ mean_cattle_id*level1category, data = catchments_phosphate1)
summary(phosphate_lm1) ## phosphate concentration increases with cattle density

ggplot() + 
  geom_histogram(mapping = aes(x=resid(phosphate_lm1))) +
  xlab("phosphate model residual")

qqPlot(phosphate_lm1)

plot(phosphate_lm1, which = 1)

## predict the phosphate concentration

catchments_phosphate1$phosphate_pred <- predict(phosphate_lm1)

## Plot the predicted phosphate concentration vs. mean cattle density

phosphate.lm1 <- ggplot(catchments_phosphate1, aes(x = mean_cattle_id, y = phosphate_pred, color = level1category)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Predicted phosphate concentration vs. mean cattle density",
       x = "Mean cattle density per catchment",
       y = "Predicted phosphate concentration [mg/L]") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

phosphate.lm1


ggsave("phosphate.lm1final.png", phosphate.lm1, width = 10, height = 6, units = "in", dpi = 300)

########################################################################################
### Final MAPS
########################################################################################

map1 <- tm_shape(Lower_Saxony) +
  tm_polygons()

map1

tmap_save(map1, "map1.png")

## Proximity polygon for phosphate 
phosphate_df <- st_as_sf(phosphate_df)
class(phosphate_df)

mapview(phosphate_df)

mapview(Lower_Saxony)
voroni_ph <- st_voronoi(phosphate_df)

phosphate_df  %<>% st_transform(3035)
Lower_Saxony %<>% st_transform(3035)

voroni_ph <- 
  phosphate_df |> 
  st_union() |> 
  st_voronoi() |> 
  st_collection_extract() |> 
  st_intersection(y= Lower_Saxony) |> 
  st_as_sf()

id <- st_nearest_feature(voroni_ph, phosphate_df)
voroni_ph$value <- phosphate_df$value[id]

mapview(voroni_ph, zcol = "value") + mapview(phosphate_df, zcol = "value", legend = F)

library(tmaptools)

map3 <- tm_shape(voroni_ph) +
  tm_fill(col = "value", title = "Phosphate Concentration [mg/L]",palette="RdYlGn",  n = 4) +
  tm_shape(phosphate_df) +
  tm_dots(size = "value") +
  tm_layout(legend.outside = TRUE) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Variability of phosphate conc. in Lower Saxony",
            main.title.size = 1)
map3

tmap_save(map3, "map3.png")

### Trend surface analysis for nitrate and phosphate concentration in Lower Saxony ###
class(nitrate_df)
nitrate_df <- st_as_sf(nitrate_df)
class(nitrate_df)

mapview(nitrate_df)

mapview(Lower_Saxony)
voroni <- st_voronoi(nitrate_df)

nitrate_df  %<>% st_transform(3035)
Lower_Saxony %<>% st_transform(3035)

voroni <- 
  nitrate_df |> 
  st_union() |> 
  st_voronoi() |> 
  st_collection_extract() |> 
  st_intersection(y= Lower_Saxony) |> 
  st_as_sf()

id <- st_nearest_feature(voroni, nitrate_df)
voroni$nitrate <- nitrate_df$value[id]

mapview(voroni, zcol = "nitrate") + mapview(nitrate_df, zcol = "value", legend = F)


map2 <- tm_shape(voroni) +
  tm_fill(col = "nitrate", title = "Nitrate Concentration [mg/L]",palette="RdYlGn",  n = 4) +
  tm_shape(nitrate_df) +
  tm_dots(size = "value.x") +
  tm_layout(legend.outside = TRUE) + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Variability of nitrate conc. in Lower Saxony",
            main.title.size = 1)

map2
tmap_save(map2, "map2.png")


## Map for mean_cattle_id in Lower Saxony explained by land cover type by using catchments_landcover

map4 <- tm_shape(catchments_landcover1) +
  tm_fill(col = "mean_cattle_id", title = "Average cattle per catchment", n = 4, style = "quantile", palette="Dark2") +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Average cattle per catchment in Lower Saxony",
            main.title.size = 1)

map4

tmap_save(map4, "map4.png")

########################################################################################
####

### Phosphate concentration in Lower Saxony explained by land cover type

class(catchments_landcover)
catchments_landcover1 <- st_as_sf(catchments_landcover)

## Landcover type in Lower Saxony
MyPalette <- c("darkgreen", "red", "#6a51a3", "blue", "green")
map5 <- tm_shape(catchments_landcover1) +
  tm_fill(col = "level1category", title = "Land cover types", legend.show = TRUE, palette= MyPalette) + 
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Land cover types in Lower Saxony",
            main.title.size = 1)
map5

tmap_save(map5, "map5.png")

## Landcover faceted by level1category

map6 <- tm_shape(catchments_landcover1) +
  tm_fill(col = "level1category", title = "Land cover types", legend.show = TRUE, palette= MyPalette) + 
  tm_facets(by = "level1category", ncol = 2) +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Land cover types in Lower Saxony (Faceted by land cover types)",
            main.title.size = 1)
map6

tmap_save(map6, "map6.png")

## Ensuring that the catchment_landcover1 and phosphate_lower_saxony should allign in the same CRS
class(phosphate_lower_saxony)

if (!identical(st_crs(catchments_landcover1), st_crs(phosphate_lower_saxony))) {
  catchments_landcover1 <- st_transform(catchments_landcover1, crs = st_crs(phosphate_lower_saxony))
}

## st_join the catchments_landcover1 and phosphate_lower_saxony

catchments_phosphate1 <- st_join(catchments_landcover1, phosphate_lower_saxony, join = st_intersects)

# Map for Phosphate concentration in Lower Saxony explained by landcover type

map8 <- tm_shape(catchments_phosphate1) +
  tm_fill(col = "mean_cattle_id", title = "Average cattle per catchment", n = 4, style = "quantile", palette="Dark2") +
  tm_facets(by = "level1category", ncol = 2) +
  tm_dots(size = "value.y") +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Variability of cattle density and phosphate concentration [mg/L] explained by land cover types in Lower Saxony",
            main.title.size = 0.8)

map8

tmap_save(map8, "map8.png")

### Nitrate concentration in Lower Saxony explained by land cover type

class(catchments_landcover)
catchments_landcover1 <- st_as_sf(catchments_landcover)

## Ensuring that the catchment_landcover1 and nitrate_lower_saxony should allign in the same CRS

if (!identical(st_crs(catchments_landcover1), st_crs(nitrate_lower_saxony))) {
  catchments_landcover1 <- st_transform(catchments_landcover1, crs = st_crs(nitrate_lower_saxony))
}

## st_join the catchments_landcover1 and nitrate_lower_saxony

catchments_nitrate1 <- st_join(catchments_landcover1, nitrate_lower_saxony, join = st_intersects)

# Map for Nitrate concentration in Lower Saxony explained by landcover type

map7 <- tm_shape(catchments_nitrate1) +
  tm_fill(col = "mean_cattle_id", title = "Average cattle per catchment", n = 4, style = "quantile", palette="Dark2") +
  tm_facets(by = "level1category", ncol = 2) +
  tm_dots(size = "value.y") +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Variability of cattle density and nitrate concentration [mg/L] explained by land cover types in Lower Saxony",
            main.title.size = 0.8)

map7

tmap_save(map7, "map7.png")

# Map for namebasin in Lower Saxony 

map9 <- tm_shape(catchments_nitrate1) +
  tm_fill(col = "namebasin.x", title = "Basin types", legend.show = TRUE, palette="Dark2") + 
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Basin types in Lower Saxony",
            main.title.size = 1)
map9

tmap_save(map9, "map9.png")

# Variability of nitrate and mean cattle density explained by basin in Lower Saxony

map10 <- tm_shape(catchments_nitrate1) +
  tm_fill(col = "mean_cattle_id", title = "Average cattle per catchment", n = 4, style = "quantile", palette="Dark2") +
  tm_facets(by = "namebasin.x", nrow = 2) +
  tm_dots(size = "value.y") +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Variability of cattle density and nitrate concentration [mg/L] explained by basin types in Lower Saxony",
            main.title.size = 0.88)
map10

tmap_save(map10, "map10.png")


# Variability of phosphate and mean cattle density explained by basin in Lower Saxony

map11 <- tm_shape(catchments_phosphate1) +
  tm_fill(col = "mean_cattle_id", title = "Average cattle per catchment", n = 4, style = "quantile", palette="Dark2") +
  tm_facets(by = "namebasin.x", nrow = 2) +
  tm_dots(size = "value.y") +
  tm_layout(legend.outside = TRUE, legend.show = TRUE) +
  tm_compass() + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE,
            legend.show = TRUE,
            main.title = "Variability of cattle density and phosphate concentration [mg/L] explained by basin types in Lower Saxony",
            main.title.size = 0.84)
map11

tmap_save(map11, "map11.png")




