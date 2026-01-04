
################################################################################
## Food Web Construction Work Flow 
## If you want to change the spatial and temporal scale of the fish survey grabs change the variables: 
## lat_min, lat_max, long_min, long_max, grid_m, grid_radius, year_min, year_max and month
##If you want to change the spatial scale of the stomach contents then change long_min_stom, long_max_stom, lat_min_stom and lat_max_stom
##If you include month in the fish survey you need to change the coords expand grid to include month options this is on line 122 to 145
##Make sure to update all setwd()
################################################################################

## ---- Load Packages ----
pkgs <- c(
  "plyr", "stringr", "mapplots",
  "dplyr", "tidyr", "reshape2",
  "cheddar", "fuzzyjoin", "splitstackshape",
  "igraph", "gridExtra", "ggplot2",
  "caret", "ISLR", "raster", "zebu",
  "MLmetrics", "ggpubr", "nlme", "ggpmisc",
  "mgcv", "ggplotify", "pdp", "xlsx",
  "effects", "tidyverse", "biscale", "maps",
  "RColorBrewer", "PNWColors", "gganimate",
  "fluxweb", "bartMachine", "vegan", "writexl",
  "purrr", "sf", "data.table", "units", "geosphere"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  stop(
    "Missing packages: ", paste(to_install, collapse = ", "),
    "\nInstall them first, e.g. install.packages(c(...))"
  )
}
invisible(lapply(pkgs, library, character.only = TRUE))

## -- Load in Main Data Sets --
setwd() ## set your own working directory

fish_survey<-read.csv("Fish_survey_with_new_guilds.csv")  ##The fish survey with all fish predators
NewIntdfAll<-read.csv("Stomach_contents_food_web.csv") ##Stomach contents dataset

FishInfo<-read.csv("Master Unique Pred.csv")   ##Predator feeding information
PredStomachInfo.all<-read.csv("PredStomachInfo.all.csv") ##Predator feeedin information
IntSpInfo<-as.data.frame(read.csv("Prey_Sp_Feeding_Info.csv"))  ##Feeding Information of Prey Species from the stomach contents

Fishweight<-read.csv("Fishweight.csv") ##Dataset with Fish weight based on fish survey data
intDfmean<-read.csv("Stomach_Contents_Parameters.csv") ##Weight and abundance of stomach contents species
SST<-readRDS("COP_SST_Data.rdata") ##Sea surface Temperature DATA from Copernicus

# Change spatial and temporal variables of fish survey here

lat_min<-51.25 ## Personalize spatial coordinates here
lat_max<-61.75

long_min<- -3.5
long_max<- 8.5

grid_m<-50000 ##change the distance between each grid central point here it is 50 000 m = 50 km 
grid_radius<-100000 ##change the radius around each grid coordinate , here we used 100000m = 100 km

year_min<- 1982 ## personalize year here
year_max<- 2016
year_range<-year_min:year_max

##change spatial scale of stomach contents here , NOTE : You do not necessarily want to use the same spatial subset throughout as
##this may not leave enough data for adequate completeness of a predators diet, here we used -Inf to keep the 

lat_min_stom<- -Inf ## Personalize spatial coordinates here
lat_max_stom<- Inf

long_min_stom<- -Inf
long_max_stom<- Inf

year_min_stom<- -Inf
year_max_stom<- Inf

subset_by_coords <- function(df,
                             lat = c(lat_min_stom, lat_max_stom),
                             lon = c(long_min_stom, long_max_stom),
                             year = c(year_min_stom, year_max_stom),
                             lat_col = latitude,
                             lon_col = longitude,
                             year_col = Year) {
  df %>%
    dplyr::filter(
      (
        is.na({{lat_col}}) |
          ({{lat_col}} >= lat[1] & {{lat_col}} <= lat[2])
      ) &
        (
          is.na({{lon_col}}) |
            ({{lon_col}} >= lon[1] & {{lon_col}} <= lon[2])
        ) &
        (
          is.na({{year_col}}) |
            ({{year_col}} >= year[1] & {{year_col}} <= year[2])
        )
    )
}
##This function also retuns NA longitude and latitude if you dont want that delete

NewIntdfAll<- subset_by_coords(NewIntdfAll)

##North sea grid squares 

bbox <- st_bbox(c(xmin = long_min, ymin = lat_min, xmax = long_max, ymax = lat_max), 
                crs = st_crs(4326))  # WGS84
bbox_sf <- st_as_sfc(bbox)
# Project to UTM (zone depends on your region – adjust if needed)
bbox_utm <- st_transform(bbox_sf, 32611)
# Make 50 km grid
grid <- st_make_grid(bbox_utm, cellsize = c(grid_m, grid_m), square = TRUE)
centroids <- st_centroid(grid)
centroids_ll <- st_transform(centroids, 4326)
land <- ne_countries(scale = "medium", returnclass = "sf")
ocean_centroids <- centroids_ll[!lengths(st_intersects(centroids_ll, land)), ]
ocean_centroids <- st_intersection(ocean_centroids, bbox_sf)

coords <- st_coordinates(ocean_centroids)
coords_df <- as.data.frame(coords) %>%
  rename(Longitude = X, Latitude = Y) %>%
  tidyr::expand_grid(Year = year_range)
coords_summary <- coords_df %>%
  group_by(Longitude, Latitude) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop")

#need to expand grid to include month if you choose to include month in the subset

# ##North sea grid squares including month
#
# month_range <- 1:12
# bbox <- st_bbox(c(xmin = long_min, ymin = lat_min, xmax = long_max, ymax = lat_max), 
#                 crs = st_crs(4326))  # WGS84
# bbox_sf <- st_as_sfc(bbox)
# # Project to UTM (zone depends on your region – adjust if needed)
# bbox_utm <- st_transform(bbox_sf, 32611)
# # Make 50 km grid
# grid <- st_make_grid(bbox_utm, cellsize = c(grid_m, grid_m), square = TRUE)
# centroids <- st_centroid(grid)
# centroids_ll <- st_transform(centroids, 4326)
# land <- ne_countries(scale = "medium", returnclass = "sf")
# ocean_centroids <- centroids_ll[!lengths(st_intersects(centroids_ll, land)), ]
# ocean_centroids <- st_intersection(ocean_centroids, bbox_sf)
# 
# coords <- st_coordinates(ocean_centroids)
# coords_df <- as.data.frame(coords) %>%
#   rename(Longitude = X, Latitude = Y) %>%
#   tidyr::expand_grid(Year = year_range, Month = month_range)
# 
# #need to expand grid to include month if you choose to include month in the subset
# 
# coords_summary <- coords_df %>%
#   group_by(Longitude, Latitude) %>%
#   summarise(n_years = n_distinct(Year), n_months = n_distinct(Month), .groups = "drop")


## subset fish survey to the spatial and temporal scale used 
fish_survey_sub <- fish_survey %>%
  filter(
    longitude >= long_min, longitude <= long_max,
    latitude  >= lat_min,  latitude  <= lat_max, 
    year >= year_min, year <= year_max) %>%
  mutate(
    Species = trimws(paste(taxa, f_guild, sep = "_"), whitespace = "_NA")
  )

FishEnv <- fish_survey_sub %>%
  dplyr::select(
    HaulID,
    Species,
    FishLength_cm,
    Biomass   = DensBiom_kg_Sqkm,
    Abundance = DensAbund_N_Sqkm,
    Year      = year,
    Gear,
    Longitude = longitude,
    Latitude  = latitude,
    Genus     = genus,
    ICES      = ices,
    Ind_weight_g = ind_weight_g,
    F.Guild   = f_guild
  ) %>%
  as.data.frame()

## Get the fish survey observations with 100 km radius of the grid coordinates
results <- vector("list", length(sample))
df2_within_100km <- vector("list",length(sample))

for (i in 1:nrow(coords_df)) {
  
  df <- coords_df[i, ]
  year <- df$Year
  lon  <- as.numeric(df$Longitude)
  lat  <- as.numeric(df$Latitude)
  
  # centroid
  pts1_coords <- cbind(df$Longitude, df$Latitude)
  
  # subset by lon/lat/year
  fish_survey_sub_sub <- FishEnv%>%
    filter(
      between(Longitude, lon - 2, lon + 2),
      between(Latitude,  lat - 2, lat + 2),
      Year == year
    )
  
  if (nrow(fish_survey_sub_sub) > 0) {
    
    fish_coords <- as.matrix(fish_survey_sub_sub[, c("Longitude", "Latitude")])
    dist_matrix <- distm(fish_coords, pts1_coords, fun = distHaversine)
    
    within_100km <- apply(dist_matrix, 1, function(x) any(x <= grid_radius))
    df2_within_100km <- fish_survey_sub_sub[within_100km, ]
    
    if (nrow(df2_within_100km) > 0) {
      
      centroid_coords <- c(lon, lat)
      
      df2_within_100km <- df2_within_100km %>%
        mutate(distance_m = mapply(
          function(lon, lat) distHaversine(c(lon, lat), centroid_coords),
          Longitude, Latitude
        ))
      
      hauls_sorted <- df2_within_100km %>% arrange(distance_m)
      unique_dists <- unique(hauls_sorted$distance_m)
      
      # only proceed if we have at least 5 distances, and takes a maxiumum of 10 starting from the closest
      if (length(unique_dists) >= 5) {
        
        if (length(unique_dists) >= 10) {
          cutoff <- sort(unique_dists)[10]
        } else {
          cutoff <- max(unique_dists)
        }
        
        closest_10plus <- hauls_sorted %>%
          filter(distance_m <= cutoff)
        
        results[[i]] <- closest_10plus %>%
          group_by(Species, Genus, F.Guild) %>%
          summarise(
            Biomass = mean(Biomass, na.rm = TRUE),
            Abundance = mean(Abundance, na.rm = TRUE),
            FishLength_cm = mean(FishLength_cm, na.rm = TRUE),
            Ind_weight_g = mean(Ind_weight_g, na.rm = TRUE),
            Year = first(Year),
            ICES = first(ICES),
            .groups = "drop"
          ) %>%
          as.data.frame()
        
        results[[i]]$Latitude  <- lat
        results[[i]]$Longitude <- lon
      }
    }
  }
}


Fish_survey_gridded <- Filter(function(x) !is.null(x) && nrow(x) > 0, results)

#saveRDS(Fish_survey_gridded, "df grab Nov 2025.rdata") #option to save

##Food web construction

##create FW

df<-Fish_survey_gridded
#df<-readRDS("df grab Nov 2025.rdata") #if you saved the grab data

FWW <- vector('list', length(df))
for (i in 1:length(df)) {
  FWW[[i]]<- as.data.frame(df[[i]])
}

Long <- vector('list', length(df))
for (i in 1:length(df)) {Long[[i]]<- FWW[[i]][1,11]}

Lat <- vector('list', length(df))
for (i in 1:length(df)) {Lat[[i]]<- FWW[[i]][1,10]}

Year <- vector('list', length(df))
for (i in 1:length(df)) {Year[[i]]<- FWW[[i]][1,8]}

names <- vector('list', length(df))
for (i in 1:length(df)) {names[[i]]<- paste0("FW_", Long[[i]], "_", Lat[[i]], "_", Year[[i]])}

# --- prepare lists ---
TL<-vector("list", length(FWW))
Year <- vector('list', length(FWW))
FinFW<- vector('list', length(FWW))

for (i in 1:length(FWW)){
  x<-i
  tryCatch({
    FW<-FWW[[i]]
    
    Year_i<-unique(FW$Year)
    
    Lat<-unique(FW$Latitude)
    Lon<-unique(FW$Longitude)
    
    FW_N<-length(unique(FW$Species)) #getting the number of fish species for use later
    
    # add feeding guild to fish survey species
    FW$New_Species <- FW$Species
    
    # Filter stomach contents data spatially for a 100 km radius
    
    NewIntdfAll <- NewIntdfAll[!is.na(NewIntdfAll$longitude), ]
    NewIntdfAll <- NewIntdfAll[!is.na(NewIntdfAll$latitude), ]
    
    # Convert to sf objects (WGS84)
    pts1 <- st_as_sf(FW, coords = c("Longitude", "Latitude"), crs = 4326)
    pts2 <- st_as_sf(NewIntdfAll, coords = c("longitude", "latitude"), crs = 4326)
    
    pts1_m <- st_transform(pts1, 3857)
    pts2_m <- st_transform(pts2, 3857)
    
    dist_matrix <- st_distance(pts1_m, pts2_m) #/ 1000  # km
    cols_within_200 <- apply(dist_matrix, 2, function(x) any(x <= 100000))
    NewIntdf <- NewIntdfAll[cols_within_200, ]
    
    #NewIntdf <-subset(NewIntdf, year = Year_i)  ##If you want the stomach contents to subsetted by time as well as space
    
    rm(pts2_m, pts2, dist_matrix)
    
    ##link poorly named predators and prey (less specific identification)
    All_species_one_word <- unique(subset(NewIntdf, !grepl("_", New_Species) & !grepl(" ", New_Species)))
    base_species<-unique(as.vector(as.character(All_species_one_word$New_Species)))
    pattern <- paste(base_species, collapse = "|")
    
    filtered_df <- NewIntdf %>%
      filter(!is.na(New_Species)) %>%
      filter(str_detect(New_Species, regex(pattern, ignore_case = FALSE)))
    filtered_df<-as.data.frame(filtered_df$New_Species)
    colnames(filtered_df)<-c("Species")
    filtered_df<-filter(filtered_df, !Species %in% base_species)
    filtered_df<-as.vector(filtered_df$Species)
    
    replacements <- filtered_df %>%
      split(str_extract(., "^[^ ]+")) %>%
      keep(names(.) %in% base_species) 
    
    to_replace <-  NewIntdf$New_Species%in% names(replacements)
    df_keep <- unique(NewIntdf[!to_replace, ])
    df_replace <- unique(NewIntdf[to_replace, ])
    
    tryCatch({
      df_expanded <- df_replace %>%
        rowwise() %>%
        mutate(New_Species= list(replacements[[New_Species]])) %>%
        unnest(New_Species)
    }, error=function(e){
      message(paste("failed for Iteration", x))})
    
    tryCatch({
      NewIntdf <- unique(NewIntdf)
    }, error=function(e){
      message(paste("failed for Iteration", x))})
    
    tryCatch({
      NewIntdf <- unique(rbind(NewIntdf, df_keep))
    }, error=function(e){
      message(paste("failed for Iteration", x))})
    
    tryCatch({
      NewIntdf <- unique(rbindlist(list(NewIntdf, df_expanded), use.names = TRUE, fill = TRUE))
    }, error = function(e) {
      message("failed for Iteration ", x)
    })
    
    All_species_one_word <- unique(subset(NewIntdf, !grepl("_", prey_taxa) & !grepl(" ", prey_taxa)))
    base_species<-unique(as.vector(as.character(All_species_one_word$prey_taxa)))
    pattern <- paste(base_species, collapse = "|")
    
    filtered_df <- NewIntdf %>%
      filter(!is.na(prey_taxa)) %>%
      filter(str_detect(prey_taxa, regex(pattern, ignore_case = FALSE)))
    filtered_df<-as.data.frame(filtered_df$prey_taxa)
    colnames(filtered_df)<-c("Species")
    filtered_df<-filter(filtered_df, !Species %in% base_species)
    filtered_df<-as.vector(filtered_df$Species)
    
    replacements <- filtered_df %>%
      split(str_extract(., "^[^ ]+")) %>%
      keep(names(.) %in% base_species) 
    
    to_replace <-  NewIntdf$prey_taxa%in% names(replacements)
    df_keep <- unique(NewIntdf[!to_replace, ])
    df_replace <- unique(NewIntdf[to_replace, ])
    
    tryCatch({
      df_expanded <- df_replace %>%
        rowwise() %>%
        mutate(prey_taxa= list(replacements[[prey_taxa]])) %>%
        unnest(prey_taxa)
    }, error=function(e){
      message(paste("failed for Iteration", x))})
    
    tryCatch({
      NewIntdf <- unique(NewIntdf)
    }, error=function(e){
      message(paste("failed for Iteration", x))})
    
    tryCatch({
      NewIntdf <- unique(rbind(NewIntdf, df_keep))
    }, error=function(e){
      message(paste("failed for Iteration", x))})
    
    tryCatch({
      NewIntdf <- unique(rbindlist(list(NewIntdf, df_expanded), use.names = TRUE, fill = TRUE))
    }, error = function(e) {
      message("failed for Iteration ", x)
    })
    
    
    if(nrow(NewIntdf) != 0) {
      
      # Filter out non-fish records and keep relevant columns to use later
      ZooIntdf <- NewIntdfAll %>%
        filter(fish == 0) %>%
        dplyr::select(Species = 2, feeding.group = 69, diet = 71)
      FW_Species<-FW$New_Species
      
      # Create master species lists
      Predators <- unique(NewIntdf$New_Species)
      Prey <- unique(na.omit(NewIntdf$prey_taxa))
      All_species <- unique(c(Predators, Prey, FW_Species)) %>%
        as.data.frame() %>%
        setNames("Species")
      
      # Remove uninformative taxa
      unrefined <- c("Decapoda", "Chordata", "Mollusca", "Arthropoda")
      All_species <- All_species %>%
        filter(!Species %in% unrefined)
      
      # Identify species not included in NewIntdf (subsetted stomach contents)
      NtInc <- All_species %>%
        filter(!Species %in% NewIntdf$Species)
      
      # Fuzzy match missing species with potential spelling errors
      new_matches <- NtInc %>%
        regex_inner_join(NewIntdf, by = "Species") %>% distinct()
      
      newSp <- unique(new_matches$New_Species) %>%
        as.data.frame() %>%
        setNames("Species")
      
      # Get additional NewIntdf records for matched species
      New1 <- NewIntdf %>%
        filter(New_Species %in% newSp$Species) %>%
        dplyr::select(consumer = 71, resource = 2, Prey_lng = 15,
                      prey_funcgrp = 54, prey_count = 12, stomach_id = 1)
      
      New2 <- NewIntdf %>%
        filter(Species %in% newSp$Species) %>%
        dplyr::select(consumer = 71, resource = 2, Prey_lng = 15,
                      prey_funcgrp = 54, prey_count = 12, stomach_id = 1)
      
      New <- rbind(New1, New2) %>%
        distinct()
      
      # Fuzzy match ZooIntdf for species not included
      NtIncZoo <- All_species %>%
        filter(!Species %in% ZooIntdf$Species,
               !Species %in% new_matches$Species.y,
               !Species %in% new_matches$Species.x)
      
      rm(new_matches)
      
      NewZoo <- NtIncZoo %>%
        regex_inner_join(ZooIntdf, by = "Species") #%>%

      NewZoosp <- NewZoo %>%
        dplyr::select(Species = Species.x) %>%
        distinct()
      
      # Combine all species
      All_species <- rbind(All_species, newSp, NewZoosp) %>%
        distinct()
      
      # Clean up redundant or misformatted entries
      FG <- All_species %>%
        filter(grepl("_", Species))
      
      Clean <- regex_inner_join(FG, All_species, by = "Species")
      
      reSp <- Clean$Species.y[!grepl("_", Clean$Species.y)]
      
      All_species <- All_species %>%
        filter(!Species %in% reSp)
      
      ### creating a food web based on the fish present from the fish survey and the interactions established by the stomach contents data
      
      ## 1.1. Species at grid level
      
      #merge fish survey with subseted stomach contents
      FWDiet <- FW %>%
        inner_join(NewIntdf, by = "New_Species", multiple = "all") %>%
        distinct()
      
      # Extract relevant diet data
      ZooFishDietInt <- FWDiet %>%
        transmute(
          consumer = New_Species,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        )
      
      # Combine with fuzzy match interactions
      ZooFishDietInt <- rbind(ZooFishDietInt, New) %>%
        distinct()
      
      # Standardize resource names using reverse-predator mapping
      newprey <- NewIntdf %>%
        filter(rev_pred_taxa %in% ZooFishDietInt$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      ZooFishDietInt <- ZooFishDietInt %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Filter by valid species list
      ZooFishDietInt <- ZooFishDietInt %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      ZooFishDietInt <- ZooFishDietInt[!is.na(ZooFishDietInt$resource), ]
      
      # Handle NA prey_count and prepare grouped data
      pred_grouped_data <- ZooFishDietInt %>%
        mutate(prey_count = as.numeric(prey_count),
               prey_count = replace_na(prey_count, 1)) %>%
        group_by(consumer)
      
      pred_grouped_data$prey_count[is.na(pred_grouped_data$prey_count)] <- 1
      
      # Split into list by predator
      df_list <- pred_grouped_data %>% group_split()
      pred<-as.vector(unlist(sapply(df_list, function(x) x[1, 1])))
      
      sp<-vector("list", length(df_list))
      best_model_fit_fsp<-vector("list", length(df_list))
      per_com<-vector("list", length(df_list))
      
      # creating and predicting yield effort curves for each predator in the food web
      for (i in 1:length(df_list)){
        tryCatch({
          z<-i
          df<-df_list[[i]]
          if(length(unique(df_list[[i]]$stomach_id)) >= 5&& length(unique(df_list[[i]]$resource)) > 1) {
            df$prey_count<-as.numeric(df$prey_count)
            grouped_data <- df %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix <- dcast(
              grouped_data,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix) <- community_matrix$stomach_id
            community_matrix <- community_matrix[, -1]  # remove stomach_id column
            
            sp[[i]] <- specaccum(community_matrix, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            fits <- list()
            aic_values <- numeric(length(models))
            names(aic_values) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit <- try(fitspecaccum(sp[[i]], model = m), silent = TRUE)
              
              if (inherits(fit, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits[[m]] <- NULL
                aic_values[m] <- NA
              } else {
                fits[[m]] <- fit
                aic_values[m] <- AIC(fit)
              }
            }
            
            # Filter out failed fits
            successful_fits <- Filter(Negate(is.null), fits)
            
            # Show AICs of successful models
            aic_values <- na.omit(aic_values)
            
            best_model_name <- names(which.min(aic_values))
            best_model_fit_fsp[[i]] <- fits[[best_model_name]]
            sone<-2 * max(sp[[i]]$sites)
            stwo<-50
            new_x <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness <- predict(best_model_fit_fsp[[i]], newdata = data.frame(sites = new_x))
            last_richness_sp <- sp[[i]]$richness[length(sp[[i]]$richness)]
            last_richness_fsp <- pred_richness[length(pred_richness)]
            per_com[[i]]<-(last_richness_sp/last_richness_fsp)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com[[i]] <- 0
          }}, error=function(e){
            message(paste("Iteration", z))})
      }
      
      per_com <- unlist(per_com)
      per_com[is.na(per_com) | sapply(per_com, is.null)] <- 0
      per_com <- as.numeric(per_com)
      n_pred <- as.character(unlist(pred))
      
      per_com_df <- data.frame(
        pred = as.character(n_pred),
        per_com = per_com,
        stringsAsFactors = FALSE
      )
      per_com_df<-as.data.frame(per_com_df)
      per_com_df<-filter(per_com_df, pred %in% FW$consumer)
      
      x_under_75 <- filter(per_com_df, per_com < 75) ## the threshold for diet completeness was chosen to be 75%
      pred_under_75_spec <- x_under_75$pred
      
      x_over_75 <- filter(per_com_df, per_com >= 75)
      
      n_per_com <- nrow(x_over_75)
      pred_per_com <- (n_per_com / FW_N) * 100
      pred_per_com<-as.numeric(pred_per_com)
      
      ## 1.2 establishing interactions based on genus name at grid 
      
      # Identify fish species with no observed interactions and under-sampled species
      #FW$New_Species <- with(FW, trimws(paste(Species, F.Guild, sep = "_"), whitespace = "_NA"))
      names(FW)[names(FW) == 'New_Species']<- "consumer"
      FWDietNtPres <- filter(FW,!consumer %in% ZooFishDietInt$consumer) %>% distinct()
      FWNotComSpe <- FW %>% filter(consumer %in% pred_under_75_spec)
      FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
      
      # Prepare genus-level match info
      GenusNtPres <- FWNotComSpef %>%
        dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
        distinct()
      
      # Match by genus in diet data
      AllGenusv1 <- NewIntdf %>% filter(pred_genus %in% GenusNtPres$Genus)
      
      Genusmerge <- GenusNtPres %>%
        inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
        mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
        distinct()
      
      Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
      
      # Select most similar species by weight within genus
      Genusmergemin <- Genusmerge %>%
        group_by(New_Species.x) %>%
        slice_min(order_by = Dist, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      # Extract matches and merge with diet info
      newAllGenus <- Genusmergemin %>%
        dplyr::select(Species = New_Species.y, Genus) %>%
        distinct()
      
      DietGenus <- inner_join(newAllGenus, NewIntdf, by = c("Species" = "New_Species"), multiple = "all")
      
      FishDietGenus <- DietGenus %>%
        transmute(
          consumer = Species,
          Genus,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        distinct()
      
      # Restore original species name and finalize
      FWGenusFin <- inner_join(FishDietGenus, GenusNtPres, by = "Genus") %>%
        transmute(
          consumer = New_Species,
          resource,
          Prey_lng,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        na.omit() %>%
        distinct()
      
      # Combine diet data from species-level and genus-level matches
      FishInfoNtPresGen <- rbind(ZooFishDietInt, FWGenusFin) %>%
        distinct() %>%
        rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
      
      # Resolve potential taxonomic synonyms or misspellings
      newprey <- NewIntdf %>%
        filter(rev_pred_taxa %in% FishInfoNtPresGen$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      # Replace general prey labels with specific species if available
      FishInfoNtPresGen <- FishInfoNtPresGen %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Keep only species that are present in the master list
      FishInfoNtPresGen <- FishInfoNtPresGen %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      FishInfoNtPresGen$prey_count[is.na(FishInfoNtPresGen$prey_count)] <- 1
      FishInfoNtPresGen <- FishInfoNtPresGen[!is.na(FishInfoNtPresGen$resource), ]
      
      # Handle missing prey count values, then group and split by consumer
      df_list_gen <- FishInfoNtPresGen %>%
        mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
        group_by(consumer) %>%
        group_split()
      
      pred_gen<-as.vector(unlist(sapply(df_list_gen, function(x) x[1, 1])))
      
      sp_gen<-vector("list", length(df_list_gen))
      best_model_fit_fsp_gen<-vector("list", length(df_list_gen))
      per_com_gen<-vector("list", length(df_list_gen))
      
      # creating and predicting yield effort curves for each predator in the food web
      for (i in 1:length(df_list_gen)){
        tryCatch({
          z<-i
          df_gen<-df_list_gen[[i]]
          
          if(length(unique(df_list_gen[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen[[i]]$resource)) > 1) {
            df_gen$prey_count<-as.numeric(df_gen$prey_count)
            grouped_data_gen <- df_gen %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              ) %>% as.data.table()
            
            community_matrix_gen <- dcast(
              grouped_data_gen,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
            community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
            
            sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits_gen <- list()
            aic_values_gen <- numeric(length(models))
            names(aic_values_gen) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_gen, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_gen[[m]] <- NULL
                aic_values_gen[m] <- NA
              } else {
                fits_gen[[m]] <- fit_gen
                aic_values_gen[m] <- AIC(fit_gen)
              }
            }
            # Filter out failed fits
            successful_fits_gen <- Filter(Negate(is.null), fits_gen)
            
            # Show AICs of successful models
            aic_values_gen <- na.omit(aic_values_gen)
            
            best_model_name_gen <- names(which.min(aic_values_gen))
            best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
            sone<-2 * max(sp_gen[[i]]$sites)
            stwo<-50
            new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
            last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
            last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
            per_com_gen[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
          } else {
            per_com_gen[[i]] <- 0
          } }, error=function(e){
            message(paste("Iteration", z))})
      }
      
      per_com_gen <- unlist(per_com_gen)
      per_com_gen[is.na(per_com_gen) | sapply(per_com_gen, is.null)] <- 0
      per_com_gen <- as.numeric(per_com_gen)
      n_pred_gen <- unique(as.character(unlist(pred_gen)))
      
      per_com_df_gen <- data.frame(
        pred = as.character(n_pred_gen),
        per_com = per_com_gen,
        stringsAsFactors = FALSE
      )
      per_com_df_gen<-as.data.frame(per_com_df_gen)
      per_com_df_gen<-filter(per_com_df_gen, pred %in% FW$consumer)
      
      # filter by threshold
      x_under_75_gen <- subset(per_com_df_gen, per_com < 75)
      x_over_75_gen  <- subset(per_com_df_gen, per_com >= 75)
      
      # Get list of under-sampled species
      pred_under_75_spec_gen <- x_under_75_gen$pred
      
      # Calculate percent well-sampled predators
      pred_per_com_gen <- (nrow(x_over_75_gen) / FW_N) * 100
      pred_per_com_gen<-as.numeric(pred_per_com_gen)
      
      ## 1.3 establishing interactions based on genus name at grid that is not more than double in size
      
      # Identify fish species with no observed interactions and under-sampled species
      #FW$New_Species <- with(FW, trimws(paste(Species, F.Guild, sep = "_"), whitespace = "_NA"))
      #names(FW)[names(FW) == 'New_Species']<- "consumer"
      FWDietNtPres <- filter(FW,!consumer %in% FishInfoNtPresGen$consumer) %>% distinct()
      FWNotComSpe <- FW %>% filter(consumer %in% pred_under_75_spec_gen)
      FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
      
      # Prepare genus-level match info
      GenusNtPres <- FWNotComSpef %>%
        dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
        distinct()
      
      # Match by genus in diet data
      AllGenusv1 <- NewIntdf %>% filter(pred_genus %in% GenusNtPres$Genus)
      
      Genusmerge <- GenusNtPres %>%
        inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
        mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
        distinct()
      
      Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
      
      # Select most similar species by weight within genus
      Genusmergemin <- Genusmerge %>%
        group_by(New_Species.x) %>%
        filter(Dist < Ind.wgt.g) %>%
        ungroup()
      
      # Extract matches and merge with diet info
      newAllGenus <- Genusmergemin %>%
        dplyr::select(Species = New_Species.y, Genus) %>%
        distinct()
      
      DietGenus <- inner_join(newAllGenus, NewIntdf, by = c("Species" = "New_Species"), multiple = "all")
      
      FishDietGenus <- DietGenus %>%
        transmute(
          consumer = Species,
          Genus,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        distinct()
      
      # Restore original species name and finalize
      FWGenusFin <- inner_join(FishDietGenus, GenusNtPres, by = "Genus") %>%
        transmute(
          consumer = New_Species,
          resource,
          Prey_lng,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        na.omit() %>%
        distinct()
      
      # Combine diet data from species-level and genus-level matches
      FishInfoNtPresGen.two <- rbind(FishInfoNtPresGen, FWGenusFin) %>%
        distinct() %>%
        rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
      
      # Resolve potential taxonomic synonyms or misspellings
      newprey <- NewIntdf %>%
        filter(rev_pred_taxa %in% FishInfoNtPresGen.two$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      # Replace general prey labels with specific species if available
      FishInfoNtPresGen.two <- FishInfoNtPresGen.two %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Keep only species that are present in the master list
      FishInfoNtPresGen.two <- FishInfoNtPresGen.two %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      FishInfoNtPresGen.two$prey_count[is.na(FishInfoNtPresGen.two$prey_count)] <- 1
      FishInfoNtPresGen.two <- FishInfoNtPresGen.two[!is.na(FishInfoNtPresGen.two$resource), ]
      
      # Handle missing prey count values, then group and split by consumer
      df_list_gen.two <- FishInfoNtPresGen.two %>%
        mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
        group_by(consumer) %>%
        group_split()
      
      pred_gen<-as.vector(unlist(sapply(df_list_gen.two, function(x) x[1, 1])))
      
      sp_gen<-vector("list", length(df_list_gen.two))
      best_model_fit_fsp_gen<-vector("list", length(df_list_gen.two))
      per_com_gen.two<-vector("list", length(df_list_gen.two))
      
      # creating and predicting yield effort curves for each predator in the food web
      for (i in 1:length(df_list_gen.two)){
        tryCatch({
          z<-i
          #pred_gen[[i]]<-(df_list_gen.two[[i]]$consumer)[1]
          df_gen<-df_list_gen.two[[i]]
          
          if(length(unique(df_list_gen.two[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen.two[[i]]$resource)) > 1) {
            df_gen$prey_count<-as.numeric(df_gen$prey_count)
            grouped_data_gen <- df_gen %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_gen <- dcast(
              grouped_data_gen,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
            community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
            
            sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits_gen <- list()
            aic_values_gen <- numeric(length(models))
            names(aic_values_gen) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_gen, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_gen[[m]] <- NULL
                aic_values_gen[m] <- NA
              } else {
                fits_gen[[m]] <- fit_gen
                aic_values_gen[m] <- AIC(fit_gen)
              }
            }
            # Filter out failed fits
            successful_fits_gen <- Filter(Negate(is.null), fits_gen)
            
            # Show AICs of successful models
            aic_values_gen <- na.omit(aic_values_gen)
            
            best_model_name_gen <- names(which.min(aic_values_gen))
            best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
            sone<-2 * max(sp_gen[[i]]$sites)
            stwo<-50
            new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
            last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
            last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
            per_com_gen.two[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
          } else {
            per_com_gen.two[[i]] <- 0
          } }, error=function(e){
            message(paste("Iteration", z))})
      }
      
      per_com_gen.two <- unlist(per_com_gen.two)
      per_com_gen.two[is.na(per_com_gen.two) | sapply(per_com_gen.two, is.null)] <- 0
      per_com_gen.two <- as.numeric(per_com_gen.two)
      n_pred_gen <- as.character(unlist(pred_gen))
      
      per_com_df_gen.two <- data.frame(
        pred = as.character(n_pred_gen),
        per_com = per_com_gen.two,
        stringsAsFactors = FALSE
      )
      per_com_df_gen.two<-as.data.frame(per_com_df_gen.two)
      per_com_df_gen.two<-filter(per_com_df_gen.two, pred %in% FW$consumer)
      
      # filter by threshold
      x_under_75_gen.two <- subset(per_com_df_gen.two, per_com < 75)
      x_over_75_gen.two  <- subset(per_com_df_gen.two, per_com >= 75)
      
      # Get list of under-sampled species
      pred_under_75_spec_gen.two <- x_under_75_gen.two$pred
      
      # Calculate percent well-sampled predators
      pred_per_com_gen.two <- (nrow(x_over_75_gen.two) / FW_N) * 100
      pred_per_com_gen.two<-as.numeric(pred_per_com_gen.two)
      
      
      ## 1.4. FEEDING GUILD THAT IS CLOSEST IN SIZE AT GRID
      
      # Identify consumers under-sampled at genus level and species with no diet information
      FWNotComGe <- FW %>% 
        filter(consumer %in% pred_under_75_spec_gen.two)
      FguildNtPres  <- filter(FW,!consumer %in% FishInfoNtPresGen.two$consumer) %>% distinct()
      FWNotComGef <- rbind(FWNotComGe, FguildNtPres) %>% 
        distinct()
      
      # Extract potential replacements from same feeding guild
      AllFguildv1 <- PredStomachInfo.all %>% 
        filter(F.guild %in% FWNotComGef$F.Guild)
      
      # Remove species already present
      AllFguild <- AllFguildv1 %>%
        mutate(F.Guild = F.guild) %>%
        dplyr::select(Species, Genus, Ind.wgt.g, F.Guild) %>%
        distinct()
      
      # Match replacement species from same guild amd the most similar weight
      Fguildmerge <- merge(FWNotComGef, AllFguild, by = "F.Guild") %>%
        mutate(
          Ind_weight_g = as.numeric(Ind_weight_g),
          Ind_wgt_match = as.numeric(Ind.wgt.g),
          Dist = abs(Ind_weight_g - Ind_wgt_match)
        )
      Fguildmerge <- Fguildmerge[Fguildmerge$consumer != Fguildmerge$Species.y, ]
      
      Fguildmergemin <- Fguildmerge %>%
        group_by(consumer) %>%
        filter(Dist == min(Dist, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct()
      
      # Map new substitute species
      newAllFguild <- Fguildmergemin %>%
        transmute(Species = Species.y, F.guild = F.Guild, Old_Species = consumer)%>% distinct ()
      
      # Join with dietary data
      DietFguild <- inner_join(newAllFguild, NewIntdf, by = c("Species" = "New_Species"))
      
      # Extract relevant diet fields
      FishDietFguild <- DietFguild %>%
        transmute(
          consumer = Old_Species,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        distinct()
      
      # Final unified dataset
      ZooFishDiet <- rbind(FishInfoNtPresGen.two, FishDietFguild) %>%
        distinct()
      
      rm(FWNotComGef, DietFguild, FishDietFguild, AllFguild)
      
      # Map prey taxon names to updated species names
      newprey <- NewIntdf %>%
        filter(rev_pred_taxa %in% ZooFishDiet$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      # Update prey names in ZooFishDiet, then clean and filter
      ZooFishDiet <- ZooFishDiet %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa")) %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct() %>%
        filter(
          consumer %in% All_species$Species,
          resource %in% All_species$Species,
          !resource %in% c("Chordata", "Mollusca", "Arthropoda")
        )
      ZooFishDiet <- ZooFishDiet[!is.na(ZooFishDiet$resource), ]
      
      # Replace NA prey counts with 1 and split by consumer
      pred_grouped_data <- ZooFishDiet %>% 
        group_by(consumer) %>%
        mutate()
      pred_grouped_data$prey_count[is.na(pred_grouped_data$prey_count)] <- 1
      df_list_fg <- pred_grouped_data %>% group_split()
      
      pred_fg<-as.vector(unlist(sapply(df_list_fg, function(x) x[1, 1])))
      # creating and predicting yield effort curves for each predator in the food web
      
      sp_fg<-vector("list", length(df_list_fg))
      best_model_fit_fsp_fg<-vector("list", length(df_list_fg))
      per_com_fg<-vector("list", length(df_list_fg))
      
      for (i in 1:length(df_list_fg)){
        tryCatch({
          z<-i
          #pred_fg[[i]]<-(df_list_fg[[i]]$consumer)[1]
          df_fg<-df_list_fg[[i]]
          if(length(unique(df_list_fg[[i]]$stomach_id)) >= 5&& length(unique(df_list_fg[[i]]$resource)) > 1) {
            df_fg$prey_count<-as.numeric(df_fg$prey_count)
            grouped_data_fg <- df_fg %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_fg <- dcast(
              grouped_data_fg,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
            community_matrix_fg <- community_matrix_fg[, -1]  # remove stomach_id column
            
            sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            fits_fg <- list()
            aic_values_fg <- numeric(length(models))
            names(aic_values_fg) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_fg, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_fg[[m]] <- NULL
                aic_values_fg[m] <- NA
              } else {
                fits_fg[[m]] <- fit_fg
                aic_values_fg[m] <- AIC(fit_fg)
              }
            }
            
            # Filter out failed fits
            successful_fits_fg <- Filter(Negate(is.null), fits_fg)
            
            # Show AICs of successful models
            aic_values_fg <- na.omit(aic_values_fg)
            
            best_model_name_fg <- names(which.min(aic_values_fg))
            best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
            sone<-2 * max(sp_fg[[i]]$sites)
            stwo<-50
            new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
            last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
            last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
            per_com_fg[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com_fg[[i]] <- 0
          } }, error=function(e){
            message(paste("Iteration", z))})
      }
      
      # Replace NA with 0 and flatten to numeric vector
      per_com_fg <- unlist(per_com_fg)
      per_com_fg[is.na(per_com_fg) | sapply(per_com_fg, is.null)] <- 0
      per_com_fg <- as.numeric(per_com_fg)
      
      n_pred_fg <- unlist(pred_fg) %>% as.character()
      
      per_com_df_fg <- data.frame(
        pred = as.character(n_pred_fg),
        per_com = per_com_fg,
        stringsAsFactors = FALSE
      )
      per_com_df_fg<-as.data.frame(per_com_df_fg)
      per_com_df_fg<-filter(per_com_df_fg, pred %in% FW$consumer)
      
      # Filter based on thresholds
      x_under_75_fg <- per_com_df_fg %>% filter(per_com < 75)
      pred_under_75_spec_fg <- x_under_75_fg$pred
      
      x_over_75_fg <- per_com_df_fg %>% filter(per_com >= 75)
      n_per_com_fg <- nrow(x_over_75_fg)
      
      pred_per_com_fg <- (n_per_com_fg / FW_N) * 100
      pred_per_com_fg<-as.numeric(pred_per_com_fg)
      
      ## 1.5 ALL FEEDING GUILDS THAT ARE NOT MORE THAN DOUBLE IN SIZE
      
      # Filter underrepresented consumers and that a re not represent 
      FWNotComGe <- FW %>% 
        filter(consumer %in% pred_under_75_spec_fg)
      
      FguildNtPres <- filter(FW,!consumer %in% ZooFishDiet$consumer) %>%
        distinct()
      
      FWNotComGef <- rbind(FWNotComGe, FguildNtPres)%>%
        distinct()
      
      # Get all entries where the feeding guild matches 
      AllFguildv1 <- PredStomachInfo.all %>% 
        filter(F.guild %in% FWNotComGef$F.Guild) %>%
        mutate(Species = as.character(Species))
      
      FWNotComGef <- FWNotComGef %>% 
        mutate(Species = as.character(Species))
      
      # Remove overlapping records and resolve guild assignment
      AllFguild <- AllFguildv1 %>%
        mutate(F.Guild = F.guild, 
               Ind_weight_g = Ind.wgt.g) %>%
        dplyr::select(Species, F.Guild, Ind_weight_g, Ind.wgt.g)
      
      # Merge based on F.Guild and compute weight distance
      Fguildmerge <- merge(FWNotComGef, AllFguild, by = "F.Guild") %>%
        mutate(
          Ind_weight_g.x = as.numeric(Ind_weight_g.x),
          Ind_weight_g.y = as.numeric(Ind.wgt.g),
          Dist = abs(Ind_weight_g.x - Ind_weight_g.y)
        )
      
      Fguildmerge <- Fguildmerge[Fguildmerge$consumer != Fguildmerge$Species.y, ]
      
      # Select matches where species are not more then double in size
      Fguildmergemin <- Fguildmerge %>%
        group_by(consumer) %>%
        filter(Dist < Ind_weight_g.x) %>%
        ungroup()
      
      # Construct new guild assignment table
      newAllFguild <- Fguildmergemin %>%
        transmute(
          Species = Species.y,
          F.guild = F.Guild,
          Old_Species = consumer
        )
      
      # Join new guild information with interaction data
      newAllFguild <- newAllFguild %>% distinct(Species, .keep_all = TRUE)
      
      #NewIntdf     <- NewIntdf %>% distinct(New_Species, .keep_all = TRUE)
      DietFguild <- inner_join(newAllFguild, NewIntdf, by = c("Species" = "New_Species"), multiple = "all")
      
      FWFguildFin.two <- DietFguild %>%
        transmute(
          consumer     = Old_Species,
          resource     = prey_taxa,
          Prey_lng     = .[[19]],
          prey_funcgrp = .[[61]],
          prey_count   = .[[16]],
          stomach_id   = stomach_id
        ) %>%
        distinct()
      
      rm(FWNotComGef, DietFguild, AllFguild)
      
      # Combine with previous diet data
      ZooFishDiet.two <- rbind(ZooFishDiet, FWFguildFin.two) %>%
        distinct()
      
      # Update resource names based on revised predator taxa
      newprey <- NewIntdf %>%
        filter(rev_pred_taxa %in% ZooFishDiet.two$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      ZooFishDiet.two <- ZooFishDiet.two %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Filter from master species list
      ZooFishDiet.two <- ZooFishDiet.two %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      
      ZooFishDiet.two <- ZooFishDiet.two[!is.na(ZooFishDiet.two$resource), ]
      pred_grouped_data <- ZooFishDiet.two %>% 
        group_by(consumer) %>%
        mutate()
      pred_grouped_data$prey_count[is.na(pred_grouped_data$prey_count)] <- 1
      df_list_fg.two <- pred_grouped_data %>% group_split()
      
      # creating and predicting yield effort curves for each predator in the food web
      pred_fg<-as.vector(unlist(sapply(df_list_fg.two, function(x) x[1, 1])))
      
      sp_fg<-vector("list", length(df_list_fg.two))
      best_model_fit_fsp_fg<-vector("list", length(df_list_fg.two))
      per_com_fg.two<-vector("list", length(df_list_fg.two))
      
      for (i in 1:length(df_list_fg.two)){
        tryCatch({
          z<-i
          df_fg<-df_list_fg.two[[i]]
          if(length(unique(df_list_fg.two[[i]]$stomach_id)) >= 5 && length(unique(df_list_fg.two[[i]]$resource)) > 1) {
            df_fg$prey_count<-as.numeric(df_fg$prey_count)
            grouped_data_fg <- df_fg %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(
                prey_count = sum(prey_count),
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_fg <- dcast(
              grouped_data_fg,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
            community_matrix_fg <- community_matrix_fg[, -1]  # remove stomach_id column
            
            sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits_fg <- list()
            aic_values_fg <- numeric(length(models))
            names(aic_values_fg) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_fg, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_fg[[m]] <- NULL
                aic_values_fg[m] <- NA
              } else {
                fits_fg[[m]] <- fit_fg
                aic_values_fg[m] <- AIC(fit_fg)
              }
            }
            
            # Filter out failed fits
            successful_fits_fg <- Filter(Negate(is.null), fits_fg)
            
            # Show AICs of successful models
            aic_values_fg <- na.omit(aic_values_fg)
            
            best_model_name_fg <- names(which.min(aic_values_fg))
            best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
            sone<-2 * max(sp_fg[[i]]$sites)
            stwo<-50
            new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
            last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
            last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
            per_com_fg.two[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com_fg.two[[i]] <- 0
          }
        }, error=function(e){
          message(paste("Iteration", z))})
      }
      
      per_com_fg.two <- unlist(per_com_fg.two)
      per_com_fg.two[is.na(per_com_fg.two) | sapply(per_com_fg.two, is.null)] <- 0
      per_com_fg.two <- as.numeric(per_com_fg.two)
      
      n_pred_fg <- unlist(pred_fg)
      
      per_com_df_fg.two <- data.frame(
        pred = as.character(n_pred_fg),
        per_com = per_com_fg.two,
        stringsAsFactors = FALSE
      )
      per_com_df_fg.two<-as.data.frame(per_com_df_fg.two)
      per_com_df_fg.two<-filter(per_com_df_fg.two, pred %in% FW$consumer)
      
      # filter by threshold
      x_under_75 <- per_com_df_fg.two %>% filter(per_com< 75)
      pred_under_75_spec_fg.two <- x_under_75$pred
      
      x_over_75_fg.two <- per_com_df_fg.two %>% filter(per_com >= 75)
      
      n_per_com_fg <- nrow(x_over_75_fg.two)
      
      pred_per_com_fg.two <- (n_per_com_fg / FW_N) * 100
      pred_per_com_fg.two<-as.numeric(pred_per_com_fg.two)
      
      ## 2.1 SPECIES AT THE STOMACH LEVEL
      
      # subset the stomach contents to 400 km radius
      # Filter undersampled predators and those that are not represented
      FWNotCom <- FW %>% 
        filter(consumer %in% pred_under_75_spec_fg.two)
      
      NtPres <- filter(FW,!consumer %in% ZooFishDiet.two$consumer) %>%
        distinct()
      
      FWNotComf <- rbind(FWNotCom, NtPres)%>%
        distinct()%>%
        group_by(consumer, Genus, F.Guild) %>%
        dplyr::summarise(          
          Ind_weight_g = mean(Ind_weight_g)
        ) %>% as.data.frame()
      
      # Merge species with the non-subsetted stomach contents data 
      FWDiet <- FWNotComf %>%
        inner_join(NewIntdfAll, by = c("consumer" = "New_Species"), multiple = "all") %>%
        distinct()
      
      # Extract relevant diet data
      ZooFishDietInt.all <- FWDiet %>%
        transmute(consumer = consumer,
                  resource = prey_taxa,
                  Prey_lng = prey_length_cm,
                  prey_funcgrp,
                  prey_count,
                  stomach_id
        )
      
      # Combine with previously inferred interactions
      ZooFishDietInt.all <- rbind(ZooFishDietInt.all, ZooFishDiet.two) %>%
        distinct()
      
      # Standardize resource names using reverse-predator mapping
      newprey <- NewIntdfAll %>%
        filter(rev_pred_taxa %in% ZooFishDietInt.all$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      ZooFishDietInt.all <- ZooFishDietInt.all %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Filter by valid species master list
      ZooFishDietInt.all <- ZooFishDietInt.all %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      ZooFishDietInt.all<- ZooFishDietInt.all[!is.na(ZooFishDietInt.all$resource), ]
      
      # Handle NA prey_count and prepare grouped data
      pred_grouped_data.all <- ZooFishDietInt.all %>%
        mutate(prey_count = as.numeric(prey_count), 
               prey_count = replace_na(prey_count, 1)) %>%
        group_by(consumer)
      pred_grouped_data.all$prey_count[is.na(pred_grouped_data.all$prey_count)] <- 1
      
      df_list.all <- pred_grouped_data.all %>% group_split()
      pred<-as.vector(unlist(sapply(df_list.all, function(x) x[1, 1])))
      
      # creating and predicting yield effort curves for each predator in the food web
      sp<-vector("list", length(df_list.all))
      best_model_fit_fsp<-vector("list", length(df_list.all))
      per_com.all<-vector("list", length(df_list.all))
      
      for (i in 1:length(df_list.all)){
        tryCatch({
          z<-i
          df<-df_list.all[[i]]
          if(length(unique(df_list.all[[i]]$stomach_id)) >= 5&& length(unique(df_list.all[[i]]$resource)) > 1) {
            df$prey_count<-as.numeric(df$prey_count)
            grouped_data <- df %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix <- dcast(
              grouped_data,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix) <- community_matrix$stomach_id
            community_matrix <- community_matrix[, -1]  # remove stomach_id column
            
            sp[[i]] <- specaccum(community_matrix, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits <- list()
            aic_values <- numeric(length(models))
            names(aic_values) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit <- try(fitspecaccum(sp[[i]], model = m), silent = TRUE)
              
              if (inherits(fit, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits[[m]] <- NULL
                aic_values[m] <- NA
              } else {
                fits[[m]] <- fit
                aic_values[m] <- AIC(fit)
              }
            }
            
            # Filter out failed fits
            successful_fits <- Filter(Negate(is.null), fits)
            
            # Show AICs of successful models
            aic_values <- na.omit(aic_values)
            
            best_model_name <- names(which.min(aic_values))
            best_model_fit_fsp[[i]] <- fits[[best_model_name]]
            sone<-2 * max(sp[[i]]$sites)
            stwo<-50
            new_x <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness <- predict(best_model_fit_fsp[[i]], newdata = data.frame(sites = new_x))
            last_richness_sp <- sp[[i]]$richness[length(sp[[i]]$richness)]
            last_richness_fsp <- pred_richness[length(pred_richness)]
            per_com.all[[i]]<-(last_richness_sp/last_richness_fsp)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com.all[[i]] <- 0
          }}, error=function(e){
            message(paste("Iteration", z))})
      }
      
      # Replace NAs and NULLs with 0, then flatten to numeric vector
      per_com.all <- unlist(per_com.all)
      per_com.all[is.na(per_com.all) | sapply(per_com.all, is.null)] <- 0
      per_com.all <- as.numeric(per_com.all)
      n_pred <- as.character(unlist(pred))
      
      # Combine with predator names into a data frame
      per_com_df.all <- data.frame(
        pred = as.character(n_pred),
        per_com = per_com.all,
        stringsAsFactors = FALSE
      )
      per_com_df.all<-as.data.frame(per_com_df.all)
      per_com_df.all<-filter(per_com_df.all, pred %in% FW$consumer)
      
      # Filter by threshold
      x_under_75.all <- filter(per_com_df.all, per_com < 75)
      pred_under_75_spec.all <- x_under_75.all$pred
      
      x_over_75.all <- filter(per_com_df.all, per_com >= 75)
      
      n_per_com.all <- nrow(x_over_75.all)
      pred_per_com.all <- (n_per_com.all / FW_N) * 100
      pred_per_com.all<-as.numeric(pred_per_com.all)
      
      ## 2.2. GENUS AT STOMACH CONTENTS LEVEL
      
      # Identify fish species with no observed interactions and under-sampled species
      FWDietNtPres <- anti_join(FW, ZooFishDietInt.all, by = "consumer") %>% distinct()
      FWNotComSpe <- FW %>% filter(consumer %in% pred_under_75_spec.all)
      FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
      
      # Prepare genus-level match info
      GenusNtPres.all <- FWNotComSpef %>%
        dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
        distinct()
      
      # Match by genus in diet data
      AllGenusv1 <- NewIntdfAll %>% filter(pred_genus %in% GenusNtPres.all$Genus)
      
      Genusmerge <- GenusNtPres.all %>%
        inner_join(AllGenusv1, by = c("Genus" = "pred_genus")) %>%
        mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
        distinct()
      
      Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
      
      # Select most similar species by weight within genus
      Genusmergemin <- Genusmerge %>%
        group_by(New_Species.x) %>%
        slice_min(order_by = Dist, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      # Extract species-level matches and merge with diet info
      newAllGenus <- Genusmergemin %>%
        dplyr::select(Species = New_Species.y, Genus) %>%
        distinct()
      
      DietGenus.all <- inner_join(newAllGenus, NewIntdfAll, by = c("Species" = "New_Species"), multiple = "all")
      
      FishDietGenus.all <- DietGenus.all %>%
        transmute(
          consumer = Species,
          Genus,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        distinct()
      
      # Restore original species name and finalize
      FWGenusFin.all <- inner_join(FishDietGenus.all, GenusNtPres.all, by = "Genus") %>%
        transmute(
          consumer = New_Species,
          resource,
          Prey_lng,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        distinct()
      
      # Combine diet data from species-level and genus-level matches
      FishInfoNtPresGen.all <- rbind(ZooFishDietInt.all, FWGenusFin.all) %>%
        distinct() %>%
        rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
      
      # Resolve potential taxonomic synonyms or misspellings
      newprey <- NewIntdfAll %>%
        filter(rev_pred_taxa %in% FishInfoNtPresGen.all$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      # Replace general prey labels with specific species if available
      FishInfoNtPresGen.all <- FishInfoNtPresGen.all %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Keep only species that are present in the master list
      FishInfoNtPresGen.all <- FishInfoNtPresGen.all %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      FishInfoNtPresGen.all$prey_count[is.na(FishInfoNtPresGen.all$prey_count)] <- 1
      FishInfoNtPresGen.all<- FishInfoNtPresGen.all[!is.na(FishInfoNtPresGen.all$resource), ]
      
      # Handle missing prey count values, then group and split by consumer
      df_list_gen.all <- FishInfoNtPresGen.all %>%
        mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
        group_by(consumer) %>%
        group_split()
      
      pred_gen<-as.vector(unlist(sapply(df_list_gen.all, function(x) x[1, 1])))
      # creating and predicting yield effort curves for each predator in the food web
      
      sp_gen<-vector("list", length(df_list_gen.all))
      best_model_fit_fsp_gen<-vector("list", length(df_list_gen.all))
      per_com_gen.all<-vector("list", length(df_list_gen.all))
      
      for (i in 1:length(df_list_gen.all)){
        tryCatch({
          z<-i
          df_gen<-df_list_gen.all[[i]]
          
          if(length(unique(df_list_gen.all[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen.all[[i]]$resource)) > 1) {
            df_gen$prey_count<-as.numeric(df_gen$prey_count)
            grouped_data_gen <- df_gen %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_gen <- dcast(
              grouped_data_gen,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
            community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
            
            sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits_gen <- list()
            aic_values_gen <- numeric(length(models))
            names(aic_values_gen) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_gen, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_gen[[m]] <- NULL
                aic_values_gen[m] <- NA
              } else {
                fits_gen[[m]] <- fit_gen
                aic_values_gen[m] <- AIC(fit_gen)
              }
            }
            # Filter out failed fits
            successful_fits_gen <- Filter(Negate(is.null), fits_gen)
            
            # Show AICs of successful models
            aic_values_gen <- na.omit(aic_values_gen)
            
            best_model_name_gen <- names(which.min(aic_values_gen))
            best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
            sone<-2 * max(sp_gen[[i]]$sites)
            stwo<-50
            new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
            last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
            last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
            per_com_gen.all[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com_gen.all[[i]] <- 0
          } }, error=function(e){
            message(paste("Iteration", z))})
      }
      
      # Replace NAs and NULLs in per_com_gen
      per_com_gen.all <- lapply(per_com_gen.all, function(x) if (is.null(x) || is.na(x)) 0 else x)
      per_com_gen.all <- as.numeric(unlist(per_com_gen.all))  # Flatten and convert to numeric
      
      n_pred_gen <- as.character(unlist(pred_gen))
      
      per_com_df_gen.all <- data.frame(
        pred = as.character(n_pred_gen),
        per_com = per_com_gen.all,
        stringsAsFactors = FALSE
      )
      per_com_df_gen.all<-as.data.frame(per_com_df_gen.all)
      per_com_df_gen.all<-filter(per_com_df_gen.all, pred %in% FW$consumer)
      
      # Split into groups based on threshold
      x_under_75_gen.all <- subset(per_com_df_gen.all, per_com < 75)
      x_over_75_gen.all  <- subset(per_com_df_gen.all, per_com >= 75)
      
      pred_under_75_spec_gen.all <- x_under_75_gen.all$pred
      
      pred_per_com_gen.all <- (nrow(x_over_75_gen.all) / FW_N) * 100
      pred_per_com_gen.all<-as.numeric(pred_per_com_gen.all)
      
      ## 2.3 establishing interactions based on genus name at stomach contents level that is not more than double in size
      
      # Identify fish species with no observed interactions and under-sampled species
      #FW$New_Species <- with(FW, trimws(paste(Species, F.Guild, sep = "_"), whitespace = "_NA"))
      #names(FW)[names(FW) == 'New_Species']<- "consumer"
      
      FWDietNtPres <- filter(FW,!consumer %in% FishInfoNtPresGen.all$consumer) %>% distinct()
      FWNotComSpe <- FW %>% filter(consumer %in% pred_under_75_spec_gen.all)
      FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
      
      # Prepare genus-level match info
      GenusNtPres <- FWNotComSpef %>%
        dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
        distinct()
      
      # Match by genus in diet data
      AllGenusv1 <- NewIntdfAll %>% filter(pred_genus %in% GenusNtPres$Genus)
      
      Genusmerge <- GenusNtPres %>%
        inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
        mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
        distinct()
      
      Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
      
      # Select most similar species by weight within genus
      Genusmergemin <- Genusmerge %>%
        group_by(New_Species.x) %>%
        filter(Dist < Ind.wgt.g) %>%
        ungroup()
      
      # Extract matches and merge with diet info
      newAllGenus <- Genusmergemin %>%
        dplyr::select(Species = New_Species.y, Genus) %>%
        distinct()
      
      DietGenus <- inner_join(newAllGenus, NewIntdfAll, by = c("Species" = "New_Species"), multiple = "all")
      
      FishDietGenus <- DietGenus %>%
        transmute(
          consumer = Species,
          Genus,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        distinct()
      
      # Restore original species name and finalize
      FWGenusFin <- inner_join(FishDietGenus, GenusNtPres, by = "Genus") %>%
        transmute(
          consumer = New_Species,
          resource,
          Prey_lng,
          prey_funcgrp,
          prey_count,
          stomach_id
        ) %>%
        na.omit() %>%
        distinct()
      
      # Combine diet data from species-level and genus-level matches
      FishInfoNtPresGen.all.two <- rbind(FishInfoNtPresGen.all, FWGenusFin) %>%
        distinct() %>%
        rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
      
      # Resolve potential taxonomic synonyms or misspellings
      newprey <- NewIntdfAll %>%
        filter(rev_pred_taxa %in% FishInfoNtPresGen.all$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      # Replace general prey labels with specific species if available
      FishInfoNtPresGen.all.two <- FishInfoNtPresGen.all.two %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Keep only species that are present in the master list
      FishInfoNtPresGen.all.two <- FishInfoNtPresGen.all.two %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      FishInfoNtPresGen.all.two$prey_count[is.na(FishInfoNtPresGen.all.two$prey_count)] <- 1
      FishInfoNtPresGen.all.two <- FishInfoNtPresGen.all.two[!is.na(FishInfoNtPresGen.all.two$resource), ]
      
      # Handle missing prey count values, then group and split by consumer
      df_list_gen.all.two <- FishInfoNtPresGen.all.two %>%
        mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
        group_by(consumer) %>%
        group_split()
      pred_gen<-as.vector(unlist(sapply(df_list_gen.all.two, function(x) x[1, 1])))
      
      sp_gen<-vector("list", length(df_list_gen.all.two))
      best_model_fit_fsp_gen<-vector("list", length(df_list_gen.all.two))
      per_com_gen.all.two<-vector("list", length(df_list_gen.all.two))
      
      # creating and predicting yield effort curves for each predator in the food web
      for (i in 1:length(df_list_gen.all.two)){
        tryCatch({
          z<-i
          df_gen<-df_list_gen.all.two[[i]]
          
          if(length(unique(df_list_gen.all.two[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen.all.two[[i]]$resource)) > 1) {
            df_gen$prey_count<-as.numeric(df_gen$prey_count)
            grouped_data_gen <- df_gen %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_gen <- dcast(
              grouped_data_gen,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
            community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
            
            sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits_gen <- list()
            aic_values_gen <- numeric(length(models))
            names(aic_values_gen) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_gen, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_gen[[m]] <- NULL
                aic_values_gen[m] <- NA
              } else {
                fits_gen[[m]] <- fit_gen
                aic_values_gen[m] <- AIC(fit_gen)
              }
            }
            # Filter out failed fits
            successful_fits_gen <- Filter(Negate(is.null), fits_gen)
            
            # Show AICs of successful models
            aic_values_gen <- na.omit(aic_values_gen)
            
            best_model_name_gen <- names(which.min(aic_values_gen))
            best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
            sone<-2 * max(sp_gen[[i]]$sites)
            stwo<-50
            new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
            last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
            last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
            per_com_gen.all.two[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
          } else {
            per_com_gen.all.two[[i]] <- 0
          } }, error=function(e){
            message(paste("Iteration", z))})
      }
      
      per_com_gen.all.two <- unlist(per_com_gen.all.two)
      per_com_gen.all.two[is.na(per_com_gen.all.two) | sapply(per_com_gen.all.two, is.null)] <- 0
      per_com_gen.all.two <- as.numeric(per_com_gen.all.two)
      n_pred_gen <- as.character(unlist(pred_gen))
      per_com_df_gen.all.two <- data.frame(
        pred = as.character(n_pred_gen),
        per_com = per_com_gen.all.two,
        stringsAsFactors = FALSE
      )
      per_com_df_gen.all.two<-as.data.frame(per_com_df_gen.all.two)
      per_com_df_gen.all.two<-filter(per_com_df_gen.all.two, pred %in% FW$consumer)
      
      # filter by threshold
      x_under_75_gen.all.two <- subset(per_com_df_gen.all.two, per_com < 75)
      x_over_75_gen.all.two  <- subset(per_com_df_gen.all.two, per_com >= 75)
      
      # Get list of under-sampled species
      pred_under_75_spec_gen.all.two <- x_under_75_gen.all.two$pred
      
      # Calculate percent well-sampled predators
      pred_per_com_gen.all.two <- (nrow(x_over_75_gen.all.two) / FW_N) * 100
      pred_per_com_gen.all.two<-as.numeric(pred_per_com_gen.all.two)
      
      ## 2.4. FEEDING GUILD THAT IS CLOSEST IN SIZE AT STOMACH CONTENTS LEVEL
      
      # Identify consumers under-sampled and not represented
      FWNotComGe.all <- FW %>% 
        filter(consumer %in% pred_under_75_spec_gen.all.two)
      
      FguildNtPres.all <- filter(FW, !consumer %in% FishInfoNtPresGen.all.two$consumer)
      
      FWNotComGef.all <- rbind(FWNotComGe.all, FguildNtPres.all) %>% 
        distinct()
      
      # Extract potential replacements from same feeding guild
      AllFguildv1 <- PredStomachInfo.all %>% 
        filter(F.guild %in% FWNotComGef.all$F.Guild)
      
      # Remove species already present
      AllFguild <- AllFguildv1 %>%
        mutate(F.Guild = F.guild) %>%
        dplyr::select(Species, Genus, Ind.wgt.g, F.Guild) %>%
        distinct()
      
      # Match replacement species from same guild, similar weight
      FWNotComGef.all<-as.data.frame(FWNotComGef.all)
      Fguildmerge.all <- merge(FWNotComGef.all, AllFguild, by = "F.Guild", multiple = "all") %>%
        mutate(
          Ind_weight_g = as.numeric(Ind_weight_g),
          Ind_wgt_match = as.numeric(Ind.wgt.g),
          Dist = abs(Ind_weight_g - Ind_wgt_match)
        )
      
      Fguildmerge.all <- Fguildmerge.all[Fguildmerge.all$consumer != Fguildmerge.all$Species.y, ]
      
      # Select closest weight match per consumer
      Fguildmergemin <- Fguildmerge.all %>%
        group_by(consumer) %>%
        filter(Dist == min(Dist, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct()
      
      # Map new substitute species
      newAllFguild.all <- Fguildmergemin %>%
        transmute(Species = Species.y, F.guild = F.Guild, Old_Species = consumer)
      
      # Join with dietary data
      DietFguild.all <- inner_join(newAllFguild.all, NewIntdfAll, by = c("Species" = "New_Species"))
      
      # Extract relevant diet fields
      FishDietFguild.all <- DietFguild.all %>%
        transmute(
          consumer = Old_Species,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        )  %>%
        distinct()
      
      # Final unified dataset
      ZooFishDiet.all <- rbind(FishInfoNtPresGen.all.two, FishDietFguild.all) %>%
        distinct()
      
      rm(FWNotComGef, DietFguild, FishDietFguild, AllFguild)
      
      # Map prey taxon names to updated species names
      newprey <- NewIntdfAll %>%
        filter(rev_pred_taxa %in% ZooFishDiet$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      # Update prey names in ZooFishDiet, then clean and filter
      
      ZooFishDiet.all <- ZooFishDiet.all %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa")) %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct() %>%
        filter(
          consumer %in% All_species$Species,
          resource %in% All_species$Species,
          !resource %in% c("Chordata", "Mollusca", "Arthropoda")
        )
      ZooFishDiet.all<- ZooFishDiet.all[!is.na(ZooFishDiet.all$resource), ]
      
      # Replace NA prey counts with 1 and split by consumer
      pred_grouped_data.all <- ZooFishDiet.all %>%
        group_by(consumer) %>%
        mutate()
      pred_grouped_data.all$prey_count[is.na(pred_grouped_data.all$prey_count)] <- 1
      df_list_fg.all <- pred_grouped_data.all %>% group_split()
      pred_fg<-as.vector(unlist(sapply(df_list_fg.all, function(x) x[1, 1])))
      
      # creating and predicting yield effort curves for each predator in the food web
      
      sp_fg<-vector("list", length(df_list_fg.all))
      best_model_fit_fsp_fg<-vector("list", length(df_list_fg.all))
      per_com_fg.all<-vector("list", length(df_list_fg.all))
      
      for (i in 1:length(df_list_fg.all)){
        tryCatch({
          z<-i
          df_fg<-df_list_fg.all[[i]]
          if(length(unique(df_list_fg.all[[i]]$stomach_id)) >= 5&& length(unique(df_list_fg.all[[i]]$resource)) > 1) {
            df_fg$prey_count<-as.numeric(df_fg$prey_count)
            grouped_data_fg <- df_fg %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_fg <- dcast(
              grouped_data_fg,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
            community_matrix_fg <- community_matrix_fg[, -1]  # remove stomach_id column
            
            sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis") 
            
            fits_fg <- list()
            aic_values_fg <- numeric(length(models))
            names(aic_values_fg) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_fg, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_fg[[m]] <- NULL
                aic_values_fg[m] <- NA
              } else {
                fits_fg[[m]] <- fit_fg
                aic_values_fg[m] <- AIC(fit_fg)
              }
            }
            
            # Filter out failed fits
            successful_fits_fg <- Filter(Negate(is.null), fits_fg)
            
            # Show AICs of successful models
            aic_values_fg <- na.omit(aic_values_fg)
            
            best_model_name_fg <- names(which.min(aic_values_fg))
            best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
            sone<-2 * max(sp_fg[[i]]$sites)
            stwo<-50
            new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
            last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
            last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
            per_com_fg.all[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com_fg.all[[i]] <- 0
          } }, error=function(e){
            message(paste("Iteration", z))})
      }
      
      per_com_fg.all <- lapply(per_com_fg.all, function(x) if (is.null(x) || is.na(x)) 0 else x)
      per_com_fg.all <- as.numeric(unlist(per_com_fg.all))  # Flatten and convert to numeric
      
      n_pred_fg <- unlist(pred_fg) %>% as.character()
      
      per_com_df_fg.all <- data.frame(
        pred = as.character(n_pred_fg),
        per_com = per_com_fg.all,
        stringsAsFactors = FALSE
      )
      per_com_df_fg.all<-as.data.frame(per_com_df_fg.all)
      per_com_df_fg.all<-filter(per_com_df_fg.all, pred %in% FW$consumer)
      
      # Filter based on thresholds
      x_under_75_fg <- per_com_df_fg.all %>% filter(per_com < 75)
      pred_under_75_spec_fg.all <- x_under_75_fg$pred
      
      x_over_75_fg <- per_com_df_fg.all %>% filter(per_com >= 75)
      n_per_com_fg <- nrow(x_over_75_fg)
      
      pred_per_com_fg.all <- (n_per_com_fg / FW_N) * 100
      pred_per_com_fg.all<-as.numeric(pred_per_com_fg.all)
      
      ## 2.4 FEEDING GUILDS THAT ARE NOT MORE THAN DOUBLE IN SIZE AT THE STOMACH CONTENTS LEVEL
      
      # identify under-sampled and not represented species
      
      FWNotComGe.all.two <- FW %>% 
        filter(consumer %in% pred_under_75_spec_fg.all)
      
      FguildNtPres.all.two <- filter(FW, !consumer %in% ZooFishDiet.all$consumer) %>%
        distinct()
      
      FWNotComGef.all.two <- unique(rbind(FWNotComGe.all.two, FguildNtPres.all.two))
      
      FWNotComGef.all.two<-as.data.frame(FWNotComGef.all.two)
      
      # Get all entries where feeding guild matches
      AllFguildv1 <- PredStomachInfo.all %>% 
        filter(F.guild %in% FWNotComGef.all.two$F.Guild) %>%
        mutate(Species = as.character(Species))
      
      FWNotComGef.all.two <- FWNotComGef.all.two %>% 
        mutate(Species = as.character(consumer))
      
      # Remove overlapping records and resolve guild assignment
      AllFguild <- AllFguildv1 %>%
        mutate(F.Guild = F.guild, 
               Ind_weight_g= Ind.wgt.g) %>%
        dplyr::select(Species, F.Guild, Ind_weight_g, Ind.wgt.g)
      
      # Merge based on F.Guild and compute weight distance
      
      Fguildmerge <- merge(FWNotComGef.all.two, AllFguild, by = "F.Guild") %>%
        mutate(
          Ind_weight_g.x = as.numeric(Ind_weight_g.x),
          Ind_weight_g.y = as.numeric(Ind.wgt.g),
          Dist = abs(Ind_weight_g.x - Ind_weight_g.y)
        )
      
      Fguildmerge <- Fguildmerge[Fguildmerge$consumer != Fguildmerge$Species.y, ]
      
      Fguildmergemin <- Fguildmerge %>%
        group_by(consumer) %>%
        filter(Dist < Ind_weight_g.x) %>%
        ungroup()
      
      # Construct new guild assignment table
      newAllFguild.all.two <- Fguildmergemin %>%
        transmute(
          Species = Species.y,
          F.guild = F.Guild,
          Old_Species = consumer
        )%>% distinct ()
      
      # Join new guild information with interaction data
      DietFguild.all.two <- inner_join(newAllFguild.all.two, NewIntdfAll, by = c("Species" = "New_Species"), multiple = "all")
      
      # Select and rename relevant columns
      FWFguildFin.all.two <- DietFguild.all.two %>%
        transmute(
          consumer     = Old_Species,
          resource     = prey_taxa,
          Prey_lng     = .[[19]],
          prey_funcgrp = .[[61]],
          prey_count   = .[[16]],
          stomach_id   = stomach_id
        ) %>%
        distinct()
      
      rm(FWNotComGef.all.two, DietFguild.all.two, AllFguild)
      
      # Combine with previous diet data
      ZooFishDiet.all.two <- rbind(ZooFishDiet.all, FWFguildFin.all.two) %>%
        distinct()
      
      # Update resource names based on revised predator taxa
      newprey <- NewIntdfAll %>%
        filter(rev_pred_taxa %in% ZooFishDiet.all.two$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      ZooFishDiet.all.two <- ZooFishDiet.all.two %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      # Filter to master species list
      ZooFishDiet.all.two <- ZooFishDiet.all.two %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      
      ZooFishDiet.all.two<- ZooFishDiet.all.two[!is.na(ZooFishDiet.all.two$resource), ]
      pred_grouped_data.all.two <- ZooFishDiet.all.two %>% 
        group_by(consumer) %>%
        mutate()
      pred_grouped_data.all.two$prey_count[is.na(pred_grouped_data.all.two$prey_count)] <- 1
      df_list_fg.all.two <- pred_grouped_data.all.two %>% group_split()
      pred_fg<-as.vector(unlist(sapply(df_list_fg.all.two, function(x) x[1, 1])))
      
      # creating and predicting yield effort curves for each predator in the food web
      sp_fg<-vector("list", length(df_list_fg.all.two))
      best_model_fit_fsp_fg<-vector("list", length(df_list_fg.all.two))
      per_com_fg.all.two<-vector("list", length(df_list_fg.all.two))
      
      for (i in 1:length(df_list_fg.all.two)){
        tryCatch({
          z<-i
          df_fg<-df_list_fg.all.two[[i]]
          if(length(unique(df_list_fg.all.two[[i]]$stomach_id)) >= 5&& length(unique(df_list_fg.all.two[[i]]$resource)) > 1) {
            df_fg$prey_count<-as.numeric(df_fg$prey_count)
            grouped_data_fg <- df_fg %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(
                prey_count = sum(prey_count),
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix_fg <- dcast(
              grouped_data_fg,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
            community_matrix_fg <- community_matrix_fg[, -1]  # remove stomach_id column
            
            sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis") 
            
            # Containers for results
            fits_fg <- list()
            aic_values_fg <- numeric(length(models))
            names(aic_values_fg) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
              
              if (inherits(fit_fg, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits_fg[[m]] <- NULL
                aic_values_fg[m] <- NA
              } else {
                fits_fg[[m]] <- fit_fg
                aic_values_fg[m] <- AIC(fit_fg)
              }
            }
            
            # Filter out failed fits
            successful_fits_fg <- Filter(Negate(is.null), fits_fg)
            
            # Show AICs of successful models
            aic_values_fg <- na.omit(aic_values_fg)
            
            best_model_name_fg <- names(which.min(aic_values_fg))
            best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
            sone<-2 * max(sp_fg[[i]]$sites)
            stwo<-50
            new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
            last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
            last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
            per_com_fg.all.two[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com_fg.all.two[[i]] <- 0
          }
        }, error=function(e){
          message(paste("Iteration", z))})
      }
      
      per_com_fg.all.two <- unlist(per_com_fg.all.two)
      per_com_fg.all.two[is.na(per_com_fg.all.two) | sapply(per_com_fg.all.two, is.null)] <- 0
      per_com_fg.all.two <- as.numeric(per_com_fg.all.two)
      
      n_pred_fg <- unlist(pred_fg)
      per_com_df_fg.all.two <- data.frame(
        pred = as.character(n_pred_fg),
        per_com = per_com_fg.all.two,
        stringsAsFactors = FALSE
      )
      per_com_df_fg.all.two<-as.data.frame(per_com_df_fg.all.two)
      per_com_df_fg.all.two<-filter(per_com_df_fg.all.two, pred %in% FW$consumer)
      
      # filter by threshold
      x_under_75 <- per_com_df_fg.all.two %>% filter(per_com< 75)
      pred_under_75_spec_fg.all.two <- x_under_75$pred
      
      x_over_75_fg.all.two <- per_com_df_fg.all.two %>% filter(per_com >= 75)
      n_per_com_fg.all.two <- nrow(x_over_75_fg.all.two)
      pred_per_com_fg.all.two <- (n_per_com_fg.all.two / FW_N) * 100
      pred_per_com_fg.all.two<-as.numeric(pred_per_com_fg.all.two)
      
      ##save percentage complete information
      Lon_name<-round(Lon, 4)
      Lat_name<-round(Lat, 4)
      
      saveRDS(per_com_df, paste0("per_com_df_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_gen, paste0("per_com_df_gen_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_gen.two, paste0("per_com_df_gen.two_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_fg, paste0("per_com_df_fg_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_fg.two, paste0("per_com_df_fg.two_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df.all, paste0("per_com_df.all_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_gen.all, paste0("per_com_df_gen.all_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_gen.all.two, paste0("per_com_df_gen.all.two_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_fg.all, paste0("per_com_df_fg.all_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      saveRDS(per_com_df_fg.all.two, paste0("per_com_df_fg.all.two_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      
      ALL_per_com<-c(pred_per_com, pred_per_com_gen, pred_per_com_gen.two, pred_per_com_fg, pred_per_com_fg.two, pred_per_com.all, pred_per_com_gen.all, pred_per_com_gen.all.two,
                     pred_per_com_fg.all,
                     pred_per_com_fg.all.two)
      ALL_per_com_df<-data.frame(Level = c("Spec.grid", "Gen.size.grid", "Gen.grid", "Fg.size.grid", "Fg.grid", "Spec.all", "Gen.size.all", "Gen.all", "Fg.size.all", "Fg.all"),
                                 per_com = ALL_per_com)
      
      saveRDS(ALL_per_com_df,
              paste0(
                "ALL_per_com_df_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
      
      ### 3. THE PREY FOOD WEB 
      ## 3.1 FIHS PREY THAT EAT OTHER FISH
    }
    
    if(nrow(ZooFishDiet.all.two) != 0) {
      
      ## Prey species that are non-fish
      
      OtherFishDiet <- ZooFishDiet.all.two %>%
        filter(prey_funcgrp %in% c("benthos", "macrobenthos", "zooplankton", "phytoplankton", "meiobenthos", "nekton", "macroplankton", "other", 
                                   "epibenthos", "mesoplankton", "endobenthos", "megabenthos", "megaplankton", "microplankton", "macro", "plankton", "ichthyoplankton"))
      
      # Join other fish diet with All_species (for correcting species names)
      OtherFishDiet <- OtherFishDiet %>%
        dplyr::rename(Species = resource) %>%
        left_join(All_species, by = "Species", multiple = "all") %>%
        filter(Species != "Porifera") %>% dplyr::select(Species, prey_funcgrp) %>% distinct()
      
      
      # pull out prey species that are also fish 
      FishFishDiet <- ZooFishDiet.all.two %>%
        filter(prey_funcgrp == "fish")
      
      ## finding interaction data of fish prey species (uses the same methodology as above with original predators)
      ## 3.1 SPECIES
      
      fish_Nt_Inc <- FishFishDiet %>%
        filter(!resource %in% c(FishFishDiet$consumer, OtherFishDiet$consumer)) %>%
        distinct(resource) %>%
        pull(resource)
      
      fish_Nt_Inc<-as.data.frame(fish_Nt_Inc)
      colnames(fish_Nt_Inc)<-c("New_Species")
      
      fish_Nt_Inc<-filter(NewIntdfAll, New_Species %in% fish_Nt_Inc$New_Species) %>% dplyr::select(New_Species, pred_genus, pred_weight_g)
      colnames(fish_Nt_Inc)<-c("New_Species", "Genus", "Ind_weight_g")
      fish_Nt_Inc$F.Guild <- str_sub(fish_Nt_Inc$New_Species, -2)
      
      # FWDiet <- fish_Nt_Inc %>%
      #   inner_join(NewIntdf, by = "New_Species", multiple = "all") %>%
      #   distinct()
      NewIntdf<-unique(NewIntdf)
      fish_Nt_Inc<-unique(fish_Nt_Inc)
      
      setDT(fish_Nt_Inc)
      setDT(NewIntdf)
      
      FWDiet <- fish_Nt_Inc[NewIntdf, on = .(New_Species), nomatch = 0L, allow.cartesian = TRUE]
      FWDiet <- as.data.frame(unique(FWDiet))
      
      fish_Nt_Inc<-as.data.frame(fish_Nt_Inc)
      NewIntdf<-as.data.frame(NewIntdf)
      
      ZooFishDietInt <- FWDiet %>%
        transmute(
          consumer = New_Species,
          resource = prey_taxa,
          Prey_lng = prey_length_cm,
          prey_funcgrp,
          prey_count,
          stomach_id
        )
      
      ZooFishDietInt <- rbind(ZooFishDietInt, New) %>%
        distinct()
      
      newprey <- NewIntdf %>%
        filter(rev_pred_taxa %in% ZooFishDietInt$resource) %>%
        dplyr::select(rev_pred_taxa, New_Species)%>%
        distinct()
      
      ZooFishDietInt <- ZooFishDietInt %>%
        left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
        mutate(resource = coalesce(New_Species, resource)) %>%
        dplyr::select(-New_Species) %>%
        distinct()
      
      ZooFishDietInt <- ZooFishDietInt %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species,
               !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
      ZooFishDietInt <- ZooFishDietInt[!is.na(ZooFishDietInt$resource), ]
      
      pred_grouped_data <- ZooFishDietInt %>%
        mutate(prey_count = replace_na(prey_count, 1)) %>%
        group_by(consumer)
      pred_grouped_data$prey_count[is.na(pred_grouped_data$prey_count)] <- 1
      
      df_list <- pred_grouped_data %>% group_split()
      pred<-as.vector(unlist(sapply(df_list, function(x) x[1, 1])))
      
      sp<-vector("list", length(df_list))
      best_model_fit_fsp<-vector("list", length(df_list))
      per_com<-vector("list", length(df_list))
      
      for (i in 1:length(df_list)){
        tryCatch({
          z<-i
          #pred[[i]]<-(df_list[[i]]$consumer)[1]
          df<-df_list[[i]]
          if(length(unique(df_list[[i]]$stomach_id)) >= 5&& length(unique(df_list[[i]]$resource)) > 1) {
            df$prey_count<-as.numeric(df$prey_count)
            grouped_data <- df %>%
              group_by(stomach_id, resource, consumer) %>%
              dplyr::summarise(          
                prey_count = sum(prey_count),    
                .groups = "drop"
              )%>% as.data.table()
            
            community_matrix <- dcast(
              grouped_data,
              stomach_id ~ resource,
              value.var = "prey_count",
              fill = 0
            )
            
            rownames(community_matrix) <- community_matrix$stomach_id
            community_matrix <- community_matrix[, -1]  # remove stomach_id column
            
            sp[[i]] <- specaccum(community_matrix, method = "exact")
            
            # List of models to try
            models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")
            
            # Containers for results
            fits <- list()
            aic_values <- numeric(length(models))
            names(aic_values) <- models
            
            # Fit models and collect AICs
            for (m in models) {
              cat("Fitting model:", m, "\n")
              fit <- try(fitspecaccum(sp[[i]], model = m), silent = TRUE)
              
              if (inherits(fit, "try-error")) {
                cat("Model", m, "failed to fit.\n")
                fits[[m]] <- NULL
                aic_values[m] <- NA
              } else {
                fits[[m]] <- fit
                aic_values[m] <- AIC(fit)
              }
            }
            
            # Filter out failed fits
            successful_fits <- Filter(Negate(is.null), fits)
            
            # Show AICs of successful models
            aic_values <- na.omit(aic_values)
            
            best_model_name <- names(which.min(aic_values))
            best_model_fit_fsp[[i]] <- fits[[best_model_name]]
            sone<-2 * max(sp[[i]]$sites)
            stwo<-50
            new_x <- seq(1, 2 * max(sone, stwo), by = 1)
            
            pred_richness <- predict(best_model_fit_fsp[[i]], newdata = data.frame(sites = new_x))
            last_richness_sp <- sp[[i]]$richness[length(sp[[i]]$richness)]
            last_richness_fsp <- pred_richness[length(pred_richness)]
            per_com[[i]]<-(last_richness_sp/last_richness_fsp)*100
          } else {
            # Not enough stomachs: set % completeness to 0
            per_com[[i]] <- 0
          }}, error=function(e){
            message(paste("Iteration", z))})
      }
      
      per_com <- per_com %>%
        lapply(function(x) if (is.null(x)) 0 else x) %>%
        lapply(function(x) ifelse(is.na(x), 0, x)) %>%
        unlist() %>%
        as.numeric()
      
      per_com_df <- data.frame(
        pred = as.character(unlist(pred)),
        per_com = per_com,
        stringsAsFactors = FALSE
      )
      
      x_under_75 <- filter(per_com_df, per_com < 75)
      pred_under_75_spec <- x_under_75$pred
      
      x_over_75 <- filter(per_com_df, per_com >= 75)
      x_over_75.fw<-filter(x_over_75, pred %in% fish_Nt_Inc$New_Species)
      
      n_per_com <- nrow(x_over_75.fw)
      pred_per_com <- (n_per_com / length(unique(fish_Nt_Inc$New_Species))) * 100
      
      ## 3.2 establishing interactions based on genus name at grid 
      
      tryCatch({
        names(fish_Nt_Inc)[names(fish_Nt_Inc) == 'New_Species']<- "consumer"
        FWDietNtPres <- anti_join(fish_Nt_Inc, ZooFishDietInt, by = "consumer") %>% distinct()
        FWNotComSpe <- fish_Nt_Inc %>% filter(consumer %in% pred_under_75_spec)
        FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
        
        #PredStomachInfo<-unique(merge(PredStomachInfov1, NewIntdf, by.x = "Species", by.y = "New_Species")[, c(1, 2, 3, 5, 74)])
        
        GenusNtPres <- FWNotComSpef %>%
          dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
          distinct()
        
        AllGenusv1 <- NewIntdf %>% filter(pred_genus %in% GenusNtPres$Genus)
        
        Genusmerge <- GenusNtPres %>%
          inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
          mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
          distinct()
        Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
        
        Genusmergemin <- Genusmerge %>%
          group_by(New_Species.x) %>%
          slice_min(order_by = Dist, n = 1, with_ties = FALSE) %>%
          ungroup()
        
        newAllGenus <- Genusmergemin %>%
          dplyr::select(Species = New_Species.y, Genus) %>%
          distinct()
        
        DietGenus <- inner_join(newAllGenus, NewIntdf, by = c("Species" = "New_Species"), multiple = "all")
        
        FishDietGenus <- DietGenus %>%
          transmute(
            consumer = Species,
            Genus,
            resource = prey_taxa,
            Prey_lng = prey_length_cm,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          distinct()
        
        FWGenusFin <- inner_join(FishDietGenus, GenusNtPres, by = "Genus") %>%
          transmute(
            consumer = New_Species,
            resource,
            Prey_lng,
            prey_funcgrp,
            prey_count,
            stomach_id
          )  %>%
          distinct()
        
        FishInfoNtPresGen <- rbind(ZooFishDietInt, FWGenusFin) %>%
          distinct() %>%
          rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
        
        newprey <- NewIntdf %>%
          filter(rev_pred_taxa %in% FishInfoNtPresGen$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        FishInfoNtPresGen <- FishInfoNtPresGen %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        FishInfoNtPresGen <- FishInfoNtPresGen %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        FishInfoNtPresGen$prey_count[is.na(FishInfoNtPresGen$prey_count)] <- 1
        FishInfoNtPresGen <- FishInfoNtPresGen[!is.na(FishInfoNtPresGen$resource), ]
        
        df_list_gen <- FishInfoNtPresGen %>%
          mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
          group_by(consumer) %>%
          group_split()
        pred_gen<-as.vector(unlist(sapply(df_list_gen, function(x) x[1, 1])))
        
        sp_gen<-vector("list", length(df_list_gen))
        best_model_fit_fsp_gen<-vector("list", length(df_list_gen))
        per_com_gen<-vector("list", length(df_list_gen))
        
        for (i in 1:length(df_list_gen)){
          tryCatch({
            z<-i
            df_gen<-df_list_gen[[i]]
            
            if(length(unique(df_list_gen[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen[[i]]$resource)) > 1) {
              df_gen$prey_count<-as.numeric(df_gen$prey_count)
              grouped_data_gen <- df_gen %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_gen <- dcast(
                grouped_data_gen,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
              community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
              
              sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
              
              # List of models to try
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       

              # Containers for results
              fits_gen <- list()
              aic_values_gen <- numeric(length(models))
              names(aic_values_gen) <- models
              
              # Fit models and collect AICs
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_gen, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_gen[[m]] <- NULL
                  aic_values_gen[m] <- NA
                } else {
                  fits_gen[[m]] <- fit_gen
                  aic_values_gen[m] <- AIC(fit_gen)
                }
              }
              successful_fits_gen <- Filter(Negate(is.null), fits_gen)
              
              aic_values_gen <- na.omit(aic_values_gen)
              
              best_model_name_gen <- names(which.min(aic_values_gen))
              best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
              sone<-2 * max(sp_gen[[i]]$sites)
              stwo<-50
              new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
              last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
              last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
              per_com_gen[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
            } else {
              per_com_gen[[i]] <- 0
            } }, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com_gen <- lapply(per_com_gen, function(x) if (is.null(x) || is.na(x)) 0 else x)
        per_com_gen <- as.numeric(unlist(per_com_gen))
        
        n_pred_gen <- as.character(unlist(pred_gen))
        
        per_com_df_gen <- data.frame(
          pred = n_pred_gen,
          per_com = per_com_gen
        )
        
        x_under_75_gen <- subset(per_com_df_gen, per_com_gen < 75)
        x_over_75_gen  <- subset(per_com_df_gen, per_com_gen >= 75)
        x_over_75_gen.fw<-filter(x_over_75, pred %in% fish_Nt_Inc$consumer)
        
        pred_under_75_spec_gen <- x_under_75_gen$pred
        
        pred_per_com_gen <- (nrow(x_over_75_gen.fw) / length(unique(fish_Nt_Inc$consumer))) * 100
      }, error=function(e){
        message(paste("failed for Iteration", x))})
      
      ## 3.3 establishing interactions based on genus name at grid that is not more than double in size
      
      # Identify fish species with no observed interactions and under-sampled species
      #FW$New_Species <- with(FW, trimws(paste(Species, F.Guild, sep = "_"), whitespace = "_NA"))
      #names(FW)[names(FW) == 'New_Species']<- "consumer"
      
      tryCatch({
        FWDietNtPres <- filter(FW,!consumer %in% FishInfoNtPresGen$consumer) %>% distinct()
        FWNotComSpe <- FW %>% filter(consumer %in% pred_under_75_spec_gen)
        FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
        
        # Prepare genus-level match info
        GenusNtPres <- FWNotComSpef %>%
          dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
          distinct()
        
        # Match by genus in diet data
        AllGenusv1 <- NewIntdf %>% filter(pred_genus %in% GenusNtPres$Genus)
        
        Genusmerge <- GenusNtPres %>%
          inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
          mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
          distinct()
        
        Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
        
        # Select most similar species by weight within genus
        Genusmergemin <- Genusmerge %>%
          group_by(New_Species.x) %>%
          filter(Dist < Ind.wgt.g) %>%
          ungroup()
        
        # Extract matches and merge with diet info
        newAllGenus <- Genusmergemin %>%
          dplyr::select(Species = New_Species.y, Genus) %>%
          distinct()
        
        DietGenus <- inner_join(newAllGenus, NewIntdf, by = c("Species" = "New_Species"), multiple = "all")
        
        FishDietGenus <- DietGenus %>%
          transmute(
            consumer = Species,
            Genus,
            resource = prey_taxa,
            Prey_lng = prey_length_cm,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          distinct()
        
        # Restore original species name and finalize
        FWGenusFin <- inner_join(FishDietGenus, GenusNtPres, by = "Genus") %>%
          transmute(
            consumer = New_Species,
            resource,
            Prey_lng,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          na.omit() %>%
          distinct()
        
        # Combine diet data from species-level and genus-level matches
        FishInfoNtPresGen.two <- rbind(FishInfoNtPresGen, FWGenusFin) %>%
          distinct() %>%
          rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
        
        # Resolve potential taxonomic synonyms or misspellings
        newprey <- NewIntdf %>%
          filter(rev_pred_taxa %in% FishInfoNtPresGen.two$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        # Replace general prey labels with specific species if available
        FishInfoNtPresGen.two <- FishInfoNtPresGen.two %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        # Keep only species that are present in the master list
        FishInfoNtPresGen.two <- FishInfoNtPresGen.two %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        FishInfoNtPresGen.two$prey_count[is.na(FishInfoNtPresGen.two$prey_count)] <- 1
        FishInfoNtPresGen.two <- FishInfoNtPresGen.two[!is.na(FishInfoNtPresGen.two$resource), ]
        
        # Handle missing prey count values, then group and split by consumer
        df_list_gen.two <- FishInfoNtPresGen.two %>%
          mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
          group_by(consumer) %>%
          group_split()
        pred_gen<-as.vector(unlist(sapply(df_list_gen.two, function(x) x[1, 1])))
        
        sp_gen<-vector("list", length(df_list_gen.two))
        best_model_fit_fsp_gen<-vector("list", length(df_list_gen.two))
        per_com_gen.two<-vector("list", length(df_list_gen.two))
        
        # creating and predicting yield effort curves for each predator in the food web
        for (i in 1:length(df_list_gen.two)){
          tryCatch({
            z<-i
            df_gen<-df_list_gen.two[[i]]
            
            if(length(unique(df_list_gen.two[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen.two[[i]]$resource)) > 1) {
              df_gen$prey_count<-as.numeric(df_gen$prey_count)
              grouped_data_gen <- df_gen %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_gen <- dcast(
                grouped_data_gen,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
              community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
              
              sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
              
              # List of models to try
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              # Containers for results
              fits_gen <- list()
              aic_values_gen <- numeric(length(models))
              names(aic_values_gen) <- models
              
              # Fit models and collect AICs
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_gen, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_gen[[m]] <- NULL
                  aic_values_gen[m] <- NA
                } else {
                  fits_gen[[m]] <- fit_gen
                  aic_values_gen[m] <- AIC(fit_gen)
                }
              }
              # Filter out failed fits
              successful_fits_gen <- Filter(Negate(is.null), fits_gen)
              
              # Show AICs of successful models
              aic_values_gen <- na.omit(aic_values_gen)
              
              best_model_name_gen <- names(which.min(aic_values_gen))
              best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
              sone<-2 * max(sp_gen[[i]]$sites)
              stwo<-50
              new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
              last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
              last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
              per_com_gen.two[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
            } else {
              per_com_gen.two[[i]] <- 0
            } }, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com_gen.two <- unlist(per_com_gen.two)
        per_com_gen.two[is.na(per_com_gen.two) | sapply(per_com_gen.two, is.null)] <- 0
        per_com_gen.two <- as.numeric(per_com_gen.two)
        n_pred_gen <- as.character(unlist(pred_gen))
        
        per_com_df_gen.two <- data.frame(
          pred = as.character(n_pred_gen),
          per_com = per_com_gen.two,
          stringsAsFactors = FALSE
        )
        per_com_df_gen.two<-as.data.frame(per_com_df_gen.two)
        
        # filter by threshold
        x_under_75_gen.two <- subset(per_com_df_gen.two, per_com < 75)
        x_over_75_gen.two  <- subset(per_com_df_gen.two, per_com >= 75)
        
        # Get list of under-sampled species
        pred_under_75_spec_gen.two <- x_under_75_gen.two$pred
        
        # Calculate percent well-sampled predators
        pred_per_com_gen.two <- (nrow(x_over_75_gen.two) / FW_N) * 100
        pred_per_com_gen.two<-as.numeric(pred_per_com_gen.two)
      })
      
      ## 3.4. FEEDING GUILD THAT IS CLOSEST IN SIZE AT GRID
      
      tryCatch({
        FWNotComGe <- fish_Nt_Inc %>% 
          filter(consumer %in% pred_under_75_spec_gen.two)
        
        FguildNtPres <- fish_Nt_Inc %>%
          anti_join(FishInfoNtPresGen.two, by = "consumer") %>%
          distinct()
        
        FWNotComGef <- rbind(FWNotComGe, FguildNtPres) %>% 
          distinct()
        
        AllFguildv1 <- PredStomachInfo.all %>% 
          filter(F.guild %in% FWNotComGef$F.Guild)
        
        AllFguild <- AllFguildv1 %>%
          mutate(F.Guild = F.guild) %>%
          dplyr::select(Species, Genus, Ind.wgt.g, F.Guild) %>%
          distinct()
        
        Fguildmerge <- merge(FWNotComGef, AllFguild, by = "F.Guild") %>%
          mutate(
            Ind_weight_g = as.numeric(Ind_weight_g),
            Ind_wgt_match = as.numeric(Ind.wgt.g),
            Dist = abs(Ind_weight_g - Ind_wgt_match)
          )
        Fguildmerge <- Fguildmerge[Fguildmerge$consumer != Fguildmerge$Species, ]
        
        Fguildmergemin <- Fguildmerge %>%
          group_by(consumer) %>%
          filter(Dist == min(Dist, na.rm = TRUE)) %>%
          ungroup() %>%
          distinct()
        
        newAllFguild <- Fguildmergemin %>%
          transmute(Species = Species, F.guild = F.Guild, Old_Species = consumer)%>% distinct ()
        
        DietFguild <- inner_join(newAllFguild, NewIntdf, by = c("Species" = "New_Species"))
        
        FishDietFguild <- DietFguild %>%
          transmute(
            consumer = Old_Species,
            resource = prey_taxa,
            Prey_lng = prey_length_cm,
            prey_funcgrp,
            prey_count,
            stomach_id
          )  %>%
          distinct()
        
        ZooFishDiet <- rbind(FishInfoNtPresGen, FishDietFguild) %>%
          distinct()
        
        rm(FWNotComGef, DietFguild, FishDietFguild, AllFguild)
        
        newprey <- NewIntdf %>%
          filter(rev_pred_taxa %in% ZooFishDiet$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        ZooFishDiet <- ZooFishDiet %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa")) %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct() %>%
          filter(
            consumer %in% All_species$Species,
            resource %in% All_species$Species,
            !resource %in% c("Chordata", "Mollusca", "Arthropoda")
          )
        ZooFishDiet <- ZooFishDiet[!is.na(ZooFishDiet$resource), ]
        
        pred_grouped_data <- ZooFishDiet %>% 
          group_by(consumer) %>%
          mutate()
        pred_grouped_data$prey_count[is.na(pred_grouped_data$prey_count)] <- 1
        df_list_fg <- pred_grouped_data %>% group_split()
        pred_fg<-as.vector(unlist(sapply(df_list_fg, function(x) x[1, 1])))
        
        sp_fg<-vector("list", length(df_list_fg))
        best_model_fit_fsp_fg<-vector("list", length(df_list_fg))
        per_com_fg<-vector("list", length(df_list_fg))
        
        for (i in 1:length(df_list_fg)){
          tryCatch({
            z<-i
            df_fg<-df_list_fg[[i]]
            if(length(unique(df_list_fg[[i]]$stomach_id)) >= 5&& length(unique(df_list_fg[[i]]$resource)) > 1) {
              df_fg$prey_count<-as.numeric(df_fg$prey_count)
              grouped_data_fg <- df_fg %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_fg <- dcast(
                grouped_data_fg,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
              community_matrix_fg <- community_matrix_fg[, -1]  
              
              sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
              
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              fits_fg <- list()
              aic_values_fg <- numeric(length(models))
              names(aic_values_fg) <- models
              
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_fg, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_fg[[m]] <- NULL
                  aic_values_fg[m] <- NA
                } else {
                  fits_fg[[m]] <- fit_fg
                  aic_values_fg[m] <- AIC(fit_fg)
                }
              }
              
              successful_fits_fg <- Filter(Negate(is.null), fits_fg)
              
              aic_values_fg <- na.omit(aic_values_fg)
              
              best_model_name_fg <- names(which.min(aic_values_fg))
              best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
              sone<-2 * max(sp_fg[[i]]$sites)
              stwo<-50
              new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
              last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
              last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
              per_com_fg[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
            } else {
              per_com_fg[[i]] <- 0
            } }, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com_fg <- lapply(per_com_fg, function(x) if (is.null(x) || is.na(x)) 0 else x)
        per_com_fg <- as.numeric(unlist(per_com_fg))
        
        n_pred_fg <- unlist(pred_fg) %>% as.character()
        
        per_com_df_fg <- tibble(pred = n_pred_fg, per_com = per_com_fg)
        
        x_under_75_fg <- per_com_df_fg %>% filter(per_com_fg < 75)
        pred_under_75_spec_fg <- x_under_75_fg$pred
        
        x_over_75_fg <- per_com_df_fg %>% filter(per_com_fg >= 75)
        x_over_75_fg.fw<-filter(x_over_75_fg, pred %in% fish_Nt_Inc$consumer)
        n_per_com_fg <- nrow(x_over_75_fg.fw)
        
        pred_per_com_fg <- (n_per_com_fg / length(fish_Nt_Inc$consumer)) * 100
      }, error = function(e) {
        message(paste("Iteration", z))
      })
      
      ## 3.5 FEEDING GUILDS THAT ARE NOT MORE THAN DOUBLE IN SIZE AT GIRD
      
      tryCatch({
        FWNotComGe <- fish_Nt_Inc %>% 
          filter(consumer %in% pred_under_75_spec_fg)
        
        FguildNtPres <- anti_join(fish_Nt_Inc, ZooFishDiet, by = "consumer") %>%
          distinct() 
        
        FWNotComGef <- rbind(FWNotComGe, FguildNtPres)%>%
          distinct()
        
        AllFguildv1 <- PredStomachInfo %>% 
          filter(F.guild %in% FWNotComGef$F.Guild) %>%
          mutate(Species = as.character(Species))
        
        FWNotComGef <- FWNotComGef %>% 
          mutate(Species = as.character(consumer))
        
        AllFguild <- AllFguildv1 %>%
          mutate(F.Guild = F.guild, 
                 Ind_weight_g = Ind.wgt.g) %>%
          dplyr::select(Species, F.Guild, Ind_weight_g, Ind.wgt.g)
        
        Fguildmerge <- merge(FWNotComGef, AllFguild, by = "F.Guild") %>%
          mutate(
            Ind_weight_g.x = as.numeric(Ind_weight_g.x),
            Ind_weight_g.y = as.numeric(Ind.wgt.g),
            Dist = abs(Ind_weight_g.x - Ind_weight_g.y)
          )
        Fguildmerge <- Fguildmerge[Fguildmerge$consumer != Fguildmerge$Species.y, ]
        
        Fguildmergemin <- Fguildmerge %>%
          group_by(consumer) %>%
          filter(Dist < Ind_weight_g.x) %>%
          ungroup()
        
        newAllFguild <- Fguildmergemin %>%
          transmute(
            Species = Species.y,
            F.guild = F.Guild,
            Old_Species = consumer
          ) %>% distinct ()
        
        DietFguild <- inner_join(newAllFguild, NewIntdf, by = c("Species" = "New_Species"), multiple = "all")
        
        FWFguildFin.two <- DietFguild %>%
          transmute(
            consumer     = Old_Species,
            resource     = prey_taxa,
            Prey_lng     = .[[19]],
            prey_funcgrp = .[[61]],
            prey_count   = .[[16]],
            stomach_id   = stomach_id
          ) %>%
          distinct() %>%
          drop_na()
        
        rm(FWNotComGef, DietFguild, AllFguild)
        
        ZooFishDiet.two <- rbind(ZooFishDiet, FWFguildFin.two) %>%
          distinct()
        
        newprey <- NewIntdf %>%
          filter(rev_pred_taxa %in% ZooFishDiet.two$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        ZooFishDiet.two <- ZooFishDiet.two %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        ZooFishDiet.two <- ZooFishDiet.two %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        
        ZooFishDiet.two<- ZooFishDiet.two[!is.na(ZooFishDiet.two$resource), ]
        pred_grouped_data <- ZooFishDiet.two %>%
          group_by(consumer) %>%
          mutate()
        pred_grouped_data$prey_count[is.na(pred_grouped_data$prey_count)] <- 1
        df_list_fg.two <- pred_grouped_data %>% group_split()
        pred_fg<-as.vector(unlist(sapply(df_list_fg.two, function(x) x[1, 1])))
        
        sp_fg<-vector("list", length(df_list_fg.two))
        best_model_fit_fsp_fg<-vector("list", length(df_list_fg.two))
        per_com_fg.two<-vector("list", length(df_list_fg.two))
        
        for (i in 1:length(df_list_fg.two)){
          tryCatch({
            z<-i
            df_fg<-df_list_fg.two[[i]]
            if(length(unique(df_list_fg.two[[i]]$stomach_id)) >= 5 && length(unique(df_list_fg.two[[i]]$resource)) > 1) {
              df_fg$prey_count<-as.numeric(df_fg$prey_count)
              grouped_data_fg <- df_fg %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(
                  prey_count = sum(prey_count),
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_fg <- dcast(
                grouped_data_fg,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
              community_matrix_fg <- community_matrix_fg[, -1]  # remove stomach_id column
              
              sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
              
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              fits_fg <- list()
              aic_values_fg <- numeric(length(models))
              names(aic_values_fg) <- models
              
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_fg, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_fg[[m]] <- NULL
                  aic_values_fg[m] <- NA
                } else {
                  fits_fg[[m]] <- fit_fg
                  aic_values_fg[m] <- AIC(fit_fg)
                }
              }
              
              successful_fits_fg <- Filter(Negate(is.null), fits_fg)
              
              aic_values_fg <- na.omit(aic_values_fg)
              
              best_model_name_fg <- names(which.min(aic_values_fg))
              best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
              sone<-2 * max(sp_fg[[i]]$sites)
              stwo<-50
              new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
              last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
              last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
              per_com_fg.two[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
            } else {
              per_com_fg.two[[i]] <- 0
            }
          }, error=function(e){
            message(paste("Iteration", z))})
        }
        
        per_com_fg.two <- unlist(per_com_fg.two)
        per_com_fg.two[is.na(per_com_fg.two) | sapply(per_com_fg.two, is.null)] <- 0
        per_com_fg.two <- as.numeric(per_com_fg.two)
        
        n_pred_fg <- unlist(pred_fg)
        per_com_fg.two<- per_com_fg.two[1:length(n_pred_fg)]
        per_com_df_fg.two <- data.frame(
          pred = as.character(n_pred_fg),
          per_com = per_com_fg.two,
          stringsAsFactors = FALSE
        )
        
        x_under_75 <- per_com_df_fg.two %>% filter(per_com_fg.two< 75)
        pred_under_75_spec_fg.two <- x_under_75$pred
        
        x_over_75_fg.two <- per_com_df_fg.two %>% filter(per_com_fg.two >= 75)
        x_over_75.fw_fg.two<-filter(x_over_75_fg.two, pred %in% fish_Nt_Inc$consumer)
        
        n_per_com_fg <- nrow(x_over_75.fw_fg.two)
        
        pred_per_com_fg.two <- (n_per_com_fg / length(fish_Nt_Inc$consumer)) * 100
      }, error=function(e){
        message(paste("failed for Iteration", x))})
      
      ## 4.1 SPECIES AT STOMACH CONTENTS LEVEL
      tryCatch({
        FWNotCom <- fish_Nt_Inc %>% 
          filter(consumer %in% pred_under_75_spec_fg.two)
        
        NtPres <- anti_join(fish_Nt_Inc, ZooFishDiet.two, by = "consumer") %>%
          distinct()
        
        FWNotComf <- rbind(FWNotCom, NtPres)%>%
          distinct()%>%
          group_by(consumer, Genus, F.Guild) %>%
          dplyr::summarise(          
            Ind_weight_g = mean(Ind_weight_g)
          )
        
        
        NewIntdfAll<-unique(NewIntdfAll)
        
        NewIntdfAll1 <- filter(NewIntdfAll, New_Species %in% FWNotComf$consumer)
        
        setDT(FWNotComf)
        setDT(NewIntdfAll1)
        
        FWDiet <- FWNotComf[NewIntdfAll1, on = .(consumer = New_Species), nomatch = 0L, allow.cartesian = TRUE]
        FWDiet <- as.data.frame(unique(FWDiet))
        
        FWNotComf<-as.data.frame(FWNotComf)
        
        ZooFishDietInt.all <- FWDiet %>%
          transmute(consumer = consumer,
                    resource = prey_taxa,
                    Prey_lng = prey_length_cm,
                    prey_funcgrp,
                    prey_count,
                    stomach_id
          )
        
        ZooFishDietInt.all <- rbind(ZooFishDietInt.all, ZooFishDiet.two) %>%
          distinct()
        
        newprey <- NewIntdfAll %>%
          filter(rev_pred_taxa %in% ZooFishDietInt.all$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        ZooFishDietInt.all <- ZooFishDietInt.all %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        ZooFishDietInt.all <- ZooFishDietInt.all %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        ZooFishDietInt.all<- ZooFishDietInt.all[!is.na(ZooFishDietInt.all$resource), ]
        
        pred_grouped_data.all <- ZooFishDietInt.all %>%
          mutate(prey_count = replace_na(prey_count, 1)) %>%
          group_by(consumer)
        pred_grouped_data.all$prey_count[is.na(pred_grouped_data.all$prey_count)] <- 1
        
        df_list.all <- pred_grouped_data.all %>% group_split()
        pred<-as.vector(unlist(sapply(df_list.all, function(x) x[1, 1])))
        
        sp<-vector("list", length(df_list.all))
        best_model_fit_fsp<-vector("list", length(df_list.all))
        per_com.all<-vector("list", length(df_list.all))
        
        for (i in 1:length(df_list.all)){
          tryCatch({
            z<-i
            df<-df_list.all[[i]]
            if(length(unique(df_list.all[[i]]$stomach_id)) >= 5&& length(unique(df_list.all[[i]]$resource)) > 1) {
              df$prey_count<-as.numeric(df$prey_count)
              grouped_data <- df %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix <- dcast(
                grouped_data,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix) <- community_matrix$stomach_id
              community_matrix <- community_matrix[, -1]  
              
              sp[[i]] <- specaccum(community_matrix, method = "exact")
              
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              fits <- list()
              aic_values <- numeric(length(models))
              names(aic_values) <- models
              
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit <- try(fitspecaccum(sp[[i]], model = m), silent = TRUE)
                
                if (inherits(fit, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits[[m]] <- NULL
                  aic_values[m] <- NA
                } else {
                  fits[[m]] <- fit
                  aic_values[m] <- AIC(fit)
                }
              }
              
              successful_fits <- Filter(Negate(is.null), fits)
              
              aic_values <- na.omit(aic_values)
              
              best_model_name <- names(which.min(aic_values))
              best_model_fit_fsp[[i]] <- fits[[best_model_name]]
              sone<-2 * max(sp[[i]]$sites)
              stwo<-50
              new_x <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness <- predict(best_model_fit_fsp[[i]], newdata = data.frame(sites = new_x))
              last_richness_sp <- sp[[i]]$richness[length(sp[[i]]$richness)]
              last_richness_fsp <- pred_richness[length(pred_richness)]
              per_com.all[[i]]<-(last_richness_sp/last_richness_fsp)*100
            } else {
              per_com.all[[i]] <- 0
            }}, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com.all <- per_com.all %>%
          lapply(function(x) if (is.null(x)) 0 else x) %>%
          lapply(function(x) ifelse(is.na(x), 0, x)) %>%
          unlist() %>%
          as.numeric()
        
        per_com_df.all <- data.frame(
          pred = as.character(unlist(pred)),
          per_com = per_com.all,
          stringsAsFactors = FALSE
        )
        
        x_under_75.all <- filter(per_com_df.all, per_com.all < 75)
        pred_under_75_spec.all <- x_under_75.all$pred
        
        x_over_75.all <- filter(per_com_df.all, per_com.all >= 75)
        x_over_75.all.fw<-filter(x_over_75.all, pred %in% fish_Nt_Inc$consumer)
        
        n_per_com.all <- nrow(x_over_75.all.fw)
        pred_per_com.all <- (n_per_com.all / length(unique(fish_Nt_Inc$consumer))) * 100
        
        ## 4.2. GENUS AT STOMACH CONTENTS LEVEL
        
        FWDietNtPres <- anti_join(fish_Nt_Inc, ZooFishDietInt.all, by = "consumer") %>% distinct()
        FWNotComSpe <- fish_Nt_Inc %>% filter(consumer %in% pred_under_75_spec.all)
        FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
        
        GenusNtPres.all <- FWNotComSpef %>%
          dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
          distinct()
        
        AllGenusv1 <- NewIntdfAll %>% filter(pred_genus %in% GenusNtPres.all$Genus)
        
        Genusmerge <- GenusNtPres.all %>%
          inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
          mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
          distinct()
        Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
        
        Genusmergemin <- Genusmerge %>%
          group_by(New_Species.x) %>%
          slice_min(order_by = Dist, n = 1, with_ties = FALSE) %>%
          ungroup()
        
        newAllGenus <- Genusmergemin %>%
          dplyr::select(Species = New_Species.y, Genus) %>%
          distinct()
        
        DietGenus.all <- inner_join(newAllGenus, NewIntdfAll, by = c("Species" = "New_Species"), multiple = "all")
        
        FishDietGenus.all <- DietGenus.all %>%
          transmute(
            consumer = Species,
            Genus,
            resource = prey_taxa,
            Prey_lng = prey_length_cm,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          distinct()
        
        FWGenusFin.all <- inner_join(FishDietGenus.all, GenusNtPres.all, by = "Genus") %>%
          transmute(
            consumer = New_Species,
            resource,
            Prey_lng,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          distinct()
        
        FishInfoNtPresGen.all <- rbind(ZooFishDietInt.all, FWGenusFin.all) %>%
          distinct() %>%
          rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
        
        newprey <- NewIntdfAll %>%
          filter(rev_pred_taxa %in% FishInfoNtPresGen.all$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        FishInfoNtPresGen.all <- FishInfoNtPresGen.all %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        FishInfoNtPresGen.all <- FishInfoNtPresGen.all %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        FishInfoNtPresGen.all$prey_count[is.na(FishInfoNtPresGen.all$prey_count)] <- 1
        FishInfoNtPresGen.all<- FishInfoNtPresGen.all[!is.na(FishInfoNtPresGen.all$resource), ]
        
        df_list_gen.all <- FishInfoNtPresGen.all %>%
          mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
          group_by(consumer) %>%
          group_split()
        pred_gen<-as.vector(unlist(sapply(df_list_gen.all, function(x) x[1, 1])))
        
        sp_gen<-vector("list", length(df_list_gen.all))
        best_model_fit_fsp_gen<-vector("list", length(df_list_gen.all))
        per_com_gen.all<-vector("list", length(df_list_gen.all))
        
        for (i in 1:length(df_list_gen.all)){
          tryCatch({
            z<-i
            df_gen<-df_list_gen.all[[i]]
            
            if(length(unique(df_list_gen.all[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen.all[[i]]$resource)) > 1) {
              df_gen$prey_count<-as.numeric(df_gen$prey_count)
              grouped_data_gen <- df_gen %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_gen <- dcast(
                grouped_data_gen,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
              community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
              
              sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
              
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              fits_gen <- list()
              aic_values_gen <- numeric(length(models))
              names(aic_values_gen) <- models
              
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_gen, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_gen[[m]] <- NULL
                  aic_values_gen[m] <- NA
                } else {
                  fits_gen[[m]] <- fit_gen
                  aic_values_gen[m] <- AIC(fit_gen)
                }
              }
              
              successful_fits_gen <- Filter(Negate(is.null), fits_gen)
              
              aic_values_gen <- na.omit(aic_values_gen)
              
              best_model_name_gen <- names(which.min(aic_values_gen))
              best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
              sone<-2 * max(sp_gen[[i]]$sites)
              stwo<-50
              new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
              last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
              last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
              per_com_gen.all[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
            } else {
              per_com_gen.all[[i]] <- 0
            } }, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com_gen.all <- lapply(per_com_gen.all, function(x) if (is.null(x) || is.na(x)) 0 else x)
        per_com_gen.all <- as.numeric(unlist(per_com_gen.all))
        
        n_pred_gen <- as.character(unlist(pred_gen))
        
        per_com_df_gen.all <- data.frame(
          pred = n_pred_gen,
          per_com = per_com_gen.all
        )
        
        x_under_75_gen.all <- subset(per_com_df_gen.all, per_com_gen.all < 75)
        x_over_75_gen.all  <- subset(per_com_df_gen.all, per_com_gen.all >= 75)
        x_over_75_gen.fw.all<-filter(x_over_75_gen.all, pred %in% fish_Nt_Inc$consumer)
        
        pred_under_75_spec_gen.all <- x_under_75_gen.all$pred
        
        pred_per_com_gen.all <- (nrow(x_over_75_gen.fw.all) / length(unique(fish_Nt_Inc$consumer))) * 100
        
        ## 4.3 establishing interactions based on genus name at stomach contents level that is not more than double in size
        
        # Identify fish species with no observed interactions and under-sampled species
        #FW$New_Species <- with(FW, trimws(paste(Species, F.Guild, sep = "_"), whitespace = "_NA"))
        #names(FW)[names(FW) == 'New_Species']<- "consumer"
        
        FWDietNtPres <- filter(FW,!consumer %in% FishInfoNtPresGen.all$consumer) %>% distinct()
        FWNotComSpe <- FW %>% filter(consumer %in% pred_under_75_spec_gen.all)
        FWNotComSpef <- rbind(FWNotComSpe, FWDietNtPres) %>% distinct()
        
        # Prepare genus-level match info
        GenusNtPres <- FWNotComSpef %>%
          dplyr::select(New_Species = consumer, Genus, Ind.wgt.g = Ind_weight_g) %>%
          distinct()
        
        # Match by genus in diet data
        AllGenusv1 <- NewIntdfAll %>% filter(pred_genus %in% GenusNtPres$Genus)
        
        Genusmerge <- GenusNtPres %>%
          inner_join(AllGenusv1, by = c("Genus" = "pred_genus"), multiple = "all") %>%
          mutate(Dist = abs(as.numeric(Ind.wgt.g) - as.numeric(pred_weight_g))) %>%
          distinct()
        
        Genusmerge <- Genusmerge[Genusmerge$New_Species.x != Genusmerge$New_Species.y, ]
        
        # Select most similar species by weight within genus
        Genusmergemin <- Genusmerge %>%
          group_by(New_Species.x) %>%
          filter(Dist < Ind.wgt.g) %>%
          ungroup()
        
        # Extract matches and merge with diet info
        newAllGenus <- Genusmergemin %>%
          dplyr::select(Species = New_Species.y, Genus) %>%
          distinct()
        
        DietGenus <- inner_join(newAllGenus, NewIntdfAll, by = c("Species" = "New_Species"), multiple = "all")
        
        FishDietGenus <- DietGenus %>%
          transmute(
            consumer = Species,
            Genus,
            resource = prey_taxa,
            Prey_lng = prey_length_cm,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          distinct()
        
        # Restore original species name and finalize
        FWGenusFin <- inner_join(FishDietGenus, GenusNtPres, by = "Genus") %>%
          transmute(
            consumer = New_Species,
            resource,
            Prey_lng,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          na.omit() %>%
          distinct()
        
        # Combine diet data from species-level and genus-level matches
        FishInfoNtPresGen.all.two <- rbind(FishInfoNtPresGen.all, FWGenusFin) %>%
          distinct() %>%
          rename_with(~c("consumer", "resource", "Prey_lng", "prey_funcgrp", "prey_count", "stomach_id"))
        
        # Resolve potential taxonomic synonyms or misspellings
        newprey <- NewIntdfAll %>%
          filter(rev_pred_taxa %in% FishInfoNtPresGen.all$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        # Replace general prey labels with specific species if available
        FishInfoNtPresGen.all.two <- FishInfoNtPresGen.all.two %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        # Keep only species that are present in the master list
        FishInfoNtPresGen.all.two <- FishInfoNtPresGen.all.two %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        FishInfoNtPresGen.all.two$prey_count[is.na(FishInfoNtPresGen.all.two$prey_count)] <- 1
        FishInfoNtPresGen.all.two <- FishInfoNtPresGen.all.two[!is.na(FishInfoNtPresGen.all.two$resource), ]
        
        # Handle missing prey count values, then group and split by consumer
        df_list_gen.all.two <- FishInfoNtPresGen.all.two %>%
          mutate(prey_count = ifelse(is.na(prey_count), 1, prey_count)) %>%
          group_by(consumer) %>%
          group_split()
        pred_gen<-as.vector(unlist(sapply(df_list_gen.all.two, function(x) x[1, 1])))
        
        sp_gen<-vector("list", length(df_list_gen.all.two))
        best_model_fit_fsp_gen<-vector("list", length(df_list_gen.all.two))
        per_com_gen.all.two<-vector("list", length(df_list_gen.all.two))
        
        # creating and predicting yield effort curves for each predator in the food web
        for (i in 1:length(df_list_gen.all.two)){
          tryCatch({
            z<-i
            df_gen<-df_list_gen.all.two[[i]]
            
            if(length(unique(df_list_gen.all.two[[i]]$stomach_id)) >= 5&& length(unique(df_list_gen.all.two[[i]]$resource)) > 1) {
              df_gen$prey_count<-as.numeric(df_gen$prey_count)
              grouped_data_gen <- df_gen %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_gen <- dcast(
                grouped_data_gen,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_gen) <- community_matrix_gen$stomach_id
              community_matrix_gen <- community_matrix_gen[, -1]  # remove stomach_id column
              
              sp_gen[[i]] <- specaccum(community_matrix_gen, method = "exact")
              
              # List of models to try
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              # Containers for results
              fits_gen <- list()
              aic_values_gen <- numeric(length(models))
              names(aic_values_gen) <- models
              
              # Fit models and collect AICs
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_gen <- try(fitspecaccum(sp_gen[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_gen, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_gen[[m]] <- NULL
                  aic_values_gen[m] <- NA
                } else {
                  fits_gen[[m]] <- fit_gen
                  aic_values_gen[m] <- AIC(fit_gen)
                }
              }
              # Filter out failed fits
              successful_fits_gen <- Filter(Negate(is.null), fits_gen)
              
              # Show AICs of successful models
              aic_values_gen <- na.omit(aic_values_gen)
              
              best_model_name_gen <- names(which.min(aic_values_gen))
              best_model_fit_fsp_gen[[i]] <- fits_gen[[best_model_name_gen]]
              sone<-2 * max(sp_gen[[i]]$sites)
              stwo<-50
              new_x_gen <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_gen <- predict(best_model_fit_fsp_gen[[i]], newdata = data.frame(sites = new_x_gen))
              last_richness_sp_gen <- sp_gen[[i]]$richness[length(sp_gen[[i]]$richness)]
              last_richness_fsp_gen <- pred_richness_gen[length(pred_richness_gen)]
              per_com_gen.all.two[[i]]<-(last_richness_sp_gen/last_richness_fsp_gen)*100
            } else {
              per_com_gen.all.two[[i]] <- 0
            } }, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com_gen.all.two <- unlist(per_com_gen.all.two)
        per_com_gen.all.two[is.na(per_com_gen.all.two) | sapply(per_com_gen.all.two, is.null)] <- 0
        per_com_gen.all.two <- as.numeric(per_com_gen.all.two)
        n_pred_gen <- as.character(unlist(pred_gen))
        
        per_com_df_gen.all.two <- data.frame(
          pred = as.character(n_pred_gen),
          per_com = per_com_gen.all.two,
          stringsAsFactors = FALSE
        )
        per_com_df_gen.all.two<-as.data.frame(per_com_df_gen.all.two)
        
        # filter by threshold
        x_under_75_gen.all.two <- subset(per_com_df_gen.all.two, per_com < 75)
        x_over_75_gen.all.two  <- subset(per_com_df_gen.all.two, per_com >= 75)
        
        # Get list of under-sampled species
        pred_under_75_spec_gen.all.two <- x_under_75_gen.all.two$pred
        
        # Calculate percent well-sampled predators
        pred_per_com_gen.all.two <- (nrow(x_over_75_gen.all.two) / FW_N) * 100
        pred_per_com_gen.all.two<-as.numeric(pred_per_com_gen.all.two)
        
        ## 4.3. FEEDING GUILD THAT IS CLOSEST IN SIZE AT STOMACH CONTENTS
        
        FWNotComGe.all <- fish_Nt_Inc %>% 
          filter(consumer %in% pred_under_75_spec_gen.all.two)
        
        FguildNtPres.all <- fish_Nt_Inc %>%
          anti_join(FishInfoNtPresGen.all.two, by = "consumer")%>%
          distinct()
        
        FWNotComGef.all <- rbind(FWNotComGe.all, FguildNtPres.all) %>% 
          distinct()
        
        AllFguildv1 <- PredStomachInfo.all %>% 
          filter(F.guild %in% FWNotComGef.all$F.Guild)
        
        AllFguild <- AllFguildv1 %>%
          mutate(F.Guild = F.guild) %>%
          dplyr::select(Species, Genus, Ind.wgt.g, F.Guild) %>%
          distinct()
        
        FWNotComGef.all<-as.data.frame(FWNotComGef.all)
        Fguildmerge.all <- merge(FWNotComGef.all, AllFguild, by = "F.Guild") %>%
          mutate(
            Ind_weight_g = as.numeric(Ind_weight_g),
            Ind_wgt_match = as.numeric(Ind.wgt.g),
            Dist = abs(Ind_weight_g - Ind_wgt_match)
          )
        Fguildmerge.all <- Fguildmerge.all[Fguildmerge.all$consumer != Fguildmerge.all$Species, ]
        Fguildmergemin <- Fguildmerge.all %>%
          group_by(consumer) %>%
          filter(Dist == min(Dist, na.rm = TRUE)) %>%
          ungroup() %>%
          distinct()
        
        newAllFguild.all <- Fguildmergemin %>%
          transmute(Species = Species, F.guild = F.Guild, Old_Species = consumer)%>% distinct ()
        
        DietFguild.all <- inner_join(newAllFguild.all, NewIntdfAll, by = c("Species" = "New_Species"))
        
        FishDietFguild.all <- DietFguild.all %>%
          transmute(
            consumer = Old_Species,
            resource = prey_taxa,
            Prey_lng = prey_length_cm,
            prey_funcgrp,
            prey_count,
            stomach_id
          ) %>%
          distinct()
        
        ZooFishDiet.all <- rbind(FishInfoNtPresGen.all, FishDietFguild.all) %>%
          distinct()
        
        rm(FWNotComGef, AllFguild)
        
        newprey <- NewIntdfAll %>%
          filter(rev_pred_taxa %in% ZooFishDiet$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        ZooFishDiet.all <- ZooFishDiet.all %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa")) %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct() %>%
          filter(
            consumer %in% All_species$Species,
            resource %in% All_species$Species,
            !resource %in% c("Chordata", "Mollusca", "Arthropoda")
          )
        ZooFishDiet.all<- ZooFishDiet.all[!is.na(ZooFishDiet.all$resource), ]
        pred_grouped_data.all <- ZooFishDiet.all %>% 
          group_by(consumer) %>%
          mutate()
        pred_grouped_data.all$prey_count[is.na(pred_grouped_data.all$prey_count)] <- 1
        df_list_fg.all <- pred_grouped_data.all %>% group_split()
        pred_fg<-as.vector(unlist(sapply(df_list_fg.all, function(x) x[1, 1])))
        
        sp_fg<-vector("list", length(df_list_fg.all))
        best_model_fit_fsp_fg<-vector("list", length(df_list_fg.all))
        per_com_fg.all<-vector("list", length(df_list_fg.all))
        
        for (i in 1:length(df_list_fg.all)){
          tryCatch({
            z<-i
            df_fg<-df_list_fg.all[[i]]
            if(length(unique(df_list_fg.all[[i]]$stomach_id)) >= 5&& length(unique(df_list_fg.all[[i]]$resource)) > 1) {
              df_fg$prey_count<-as.numeric(df_fg$prey_count)
              grouped_data_fg <- df_fg %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(          
                  prey_count = sum(prey_count),    
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_fg <- dcast(
                grouped_data_fg,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
              community_matrix_fg <- community_matrix_fg[, -1] 
              
              sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
              
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              fits_fg <- list()
              aic_values_fg <- numeric(length(models))
              names(aic_values_fg) <- models
              
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_fg, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_fg[[m]] <- NULL
                  aic_values_fg[m] <- NA
                } else {
                  fits_fg[[m]] <- fit_fg
                  aic_values_fg[m] <- AIC(fit_fg)
                }
              }
              
              successful_fits_fg <- Filter(Negate(is.null), fits_fg)
              
              aic_values_fg <- na.omit(aic_values_fg)
              
              best_model_name_fg <- names(which.min(aic_values_fg))
              best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
              sone<-2 * max(sp_fg[[i]]$sites)
              stwo<-50
              new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
              last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
              last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
              per_com_fg.all[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
            } else {
              per_com_fg.all[[i]] <- 0
            } }, error=function(e){
              message(paste("Iteration", z))})
        }
        
        per_com_fg.all <- lapply(per_com_fg.all, function(x) if (is.null(x) || is.na(x)) 0 else x)
        per_com_fg.all <- as.numeric(unlist(per_com_fg.all))
        
        n_pred_fg <- unlist(pred_fg) %>% as.character()
        
        per_com_df_fg.all <- tibble(pred = n_pred_fg, per_com = per_com_fg.all)
        
        x_under_75_fg <- per_com_df_fg.all %>% filter(per_com_fg.all < 75)
        pred_under_75_spec_fg.all <- x_under_75_fg$pred
        
        x_over_75_fg <- per_com_df_fg.all %>% filter(per_com_fg.all >= 75)
        x_over_75_fg.fw<-filter(x_over_75_fg, pred %in% fish_Nt_Inc$consumer)
        n_per_com_fg <- nrow(x_over_75_fg.fw)
        
        pred_per_com_fg.all <- (n_per_com_fg / length(fish_Nt_Inc$consumer)) * 100
        
        ## 4.4 FEEDING GUILDS THAT ARE NOT MORE THAN DOUBLE IN SIZE AT STOMACH CONTENTS
        
        FWNotComGe.all.two <- fish_Nt_Inc %>% 
          filter(consumer %in% pred_under_75_spec_fg.all)
        
        FguildNtPres.all.two <- anti_join(fish_Nt_Inc, ZooFishDiet.all, by = "consumer") %>%
          distinct()
        
        FWNotComGef.all.two <- rbind(FWNotComGe.all.two, FguildNtPres.all.two)%>%
          distinct()
        FWNotComGef.all.two <-as.data.frame(FWNotComGef.all.two )
        AllFguildv1 <- PredStomachInfo.all %>% 
          filter(F.guild %in% FWNotComGef.all.two$F.Guild) %>%
          mutate(Species = as.character(Species))
        
        FWNotComGef.all.two <- FWNotComGef.all.two %>% 
          mutate(Species = as.character(consumer))
        
        AllFguild <- AllFguildv1 %>%
          mutate(F.Guild = F.guild, 
                 Ind_weight_g= Ind.wgt.g) %>%
          dplyr::select(Species, F.Guild, Ind_weight_g, Ind.wgt.g)
        
        Fguildmerge <- merge(FWNotComGef.all.two, AllFguild, by = "F.Guild") %>%
          mutate(
            Ind_weight_g.x = as.numeric(Ind_weight_g.x),
            Ind_weight_g.y = as.numeric(Ind.wgt.g),
            Dist = abs(Ind_weight_g.x - Ind_weight_g.y)
          )
        Fguildmerge <- Fguildmerge[Fguildmerge.all$consumer != Fguildmerge$Species.y, ]
        Fguildmergemin <- Fguildmerge %>%
          group_by(consumer) %>%
          filter(Dist < Ind_weight_g.x) %>%
          ungroup()
        
        newAllFguild.all.two <- Fguildmergemin %>%
          transmute(
            Species = Species.y,
            F.guild = F.Guild,
            Old_Species = consumer
          ) %>% distinct ()
        
        DietFguild.all.two <- inner_join(newAllFguild.all.two, NewIntdfAll, by = c("Species" = "New_Species"), multiple = "all")
        
        FWFguildFin.all.two <- DietFguild.all.two %>%
          transmute(
            consumer     = Old_Species,
            resource     = prey_taxa,
            Prey_lng     = .[[19]],
            prey_funcgrp = .[[61]],
            prey_count   = .[[16]],
            stomach_id   = stomach_id
          ) %>%
          distinct()
        
        rm(FWNotComGef.all.two, DietFguild.all.two, AllFguild)
        
        FishFishDiet.all.two <- rbind(ZooFishDiet.all, FWFguildFin.all.two) %>%
          distinct()
        
        newprey <- NewIntdfAll %>%
          filter(rev_pred_taxa %in% FishFishDiet.all.two$resource) %>%
          dplyr::select(rev_pred_taxa, New_Species)%>%
          distinct()
        
        FishFishDiet.all.two <- FishFishDiet.all.two %>%
          left_join(newprey, by = c("resource" = "rev_pred_taxa"), multiple = "all") %>%
          mutate(resource = coalesce(New_Species, resource)) %>%
          dplyr::select(-New_Species) %>%
          distinct()
        
        FishFishDiet.all.two <- FishFishDiet.all.two %>%
          filter(consumer %in% All_species$Species,
                 resource %in% All_species$Species,
                 !resource %in% c("Chordata", "Mollusca", "Arthropoda"))
        FishFishDiet.all.two<- FishFishDiet.all.two[!is.na(FishFishDiet.all.two$resource), ]
        
        pred_grouped_data.all.two <- FishFishDiet.all.two %>%
          group_by(consumer) %>%
          mutate()
        pred_grouped_data.all.two$prey_count[is.na(pred_grouped_data.all.two$prey_count)] <- 1
        df_list_fg.all.two <- pred_grouped_data.all.two %>% group_split()
        pred_fg<-as.vector(unlist(sapply(df_list_fg.all.two, function(x) x[1, 1])))
        
        sp_fg<-vector("list", length(df_list_fg.all.two))
        best_model_fit_fsp_fg<-vector("list", length(df_list_fg.all.two))
        per_com_fg.all.two<-vector("list", length(df_list_fg.all.two))
        
        for (i in 1:length(df_list_fg.all.two)){
          tryCatch({
            z<-i
            df_fg<-df_list_fg.all.two[[i]]
            if(length(unique(df_list_fg.all.two[[i]]$stomach_id)) >= 5&& length(unique(df_list_fg.all.two[[i]]$resource)) > 1) {
              df_fg$prey_count<-as.numeric(df_fg$prey_count)
              grouped_data_fg <- df_fg %>%
                group_by(stomach_id, resource, consumer) %>%
                dplyr::summarise(
                  prey_count = sum(prey_count),
                  .groups = "drop"
                )%>% as.data.table()
              
              community_matrix_fg <- dcast(
                grouped_data_fg,
                stomach_id ~ resource,
                value.var = "prey_count",
                fill = 0
              )
              
              rownames(community_matrix_fg) <- community_matrix_fg$stomach_id
              community_matrix_fg <- community_matrix_fg[, -1]  # remove stomach_id column
              
              sp_fg[[i]] <- specaccum(community_matrix_fg, method = "exact")
              
              models <- c("lomolino", "arrhenius", "gleason", "gitay", "asymp", "logis")       
              
              fits_fg <- list()
              aic_values_fg <- numeric(length(models))
              names(aic_values_fg) <- models
              
              for (m in models) {
                cat("Fitting model:", m, "\n")
                fit_fg <- try(fitspecaccum(sp_fg[[i]], model = m), silent = TRUE)
                
                if (inherits(fit_fg, "try-error")) {
                  cat("Model", m, "failed to fit.\n")
                  fits_fg[[m]] <- NULL
                  aic_values_fg[m] <- NA
                } else {
                  fits_fg[[m]] <- fit_fg
                  aic_values_fg[m] <- AIC(fit_fg)
                }
              }
              
              successful_fits_fg <- Filter(Negate(is.null), fits_fg)
              
              aic_values_fg <- na.omit(aic_values_fg)
              
              
              best_model_name_fg <- names(which.min(aic_values_fg))
              best_model_fit_fsp_fg[[i]] <- fits_fg[[best_model_name_fg]]
              sone<-2 * max(sp_fg[[i]]$sites)
              stwo<-50
              new_x_fg <- seq(1, 2 * max(sone, stwo), by = 1)
              
              pred_richness_fg <- predict(best_model_fit_fsp_fg[[i]], newdata = data.frame(sites = new_x_fg))
              last_richness_sp_fg <- sp_fg[[i]]$richness[length(sp_fg[[i]]$richness)]
              last_richness_fsp_fg <- pred_richness_fg[length(pred_richness_fg)]
              per_com_fg.all.two[[i]]<-(last_richness_sp_fg/last_richness_fsp_fg)*100
            } else {
              per_com_fg.all.two[[i]] <- 0
            }
          }, error=function(e){
            message(paste("Iteration", z))})
        }
        
        per_com_fg.all.two <- unlist(per_com_fg.all.two)
        per_com_fg.all.two[is.na(per_com_fg.all.two) | sapply(per_com_fg.all.two, is.null)] <- 0
        per_com_fg.all.two <- as.numeric(per_com_fg.all.two)
        
        n_pred_fg <- unlist(pred_fg)
        per_com_df_fg.all.two <- data.frame(
          pred = as.character(n_pred_fg),
          per_com = per_com_fg.all.two,
          stringsAsFactors = FALSE
        )
        
        x_under_75 <- per_com_df_fg.all.two %>% filter(per_com_fg.all.two< 75)
        pred_under_75_spec_fg.all.two <- x_under_75$pred
        
      }, error=function(e){
        message(paste("failed for Iteration", x))})
      
      Ad_Fish <- FishFishDiet.all.two %>%
        filter(consumer %in% All_species$Species,
               resource %in% All_species$Species) %>% distinct()
      
      
      
      # Merge with ZooIntdf to bring in any zoo prey interaction data
      ZooDjoined <- merge(OtherFishDiet, ZooIntdf, by = "Species", all = TRUE)
      
      # Extract relevant columns and duplicates
      OtherZoo <- ZooDjoined %>%
        dplyr::select(Species, feeding.group, diet) %>%
        distinct()
      colnames(OtherZoo)<-c("consumer", "Feeding.Group", "resource")
      
      ZooDietAll <- distinct(OtherZoo)
      
      # Extract species without spaces (likely genus-only entries) to link to species within the same genus
      base_species <- All_species %>%
        filter(!str_detect(Species, "\\s")) %>%
        pull(Species)
      
      # Create pattern and filter ZooDietAll consumers that match base_species pattern
      pattern <- paste0("^(", paste(base_species, collapse = "|"), ")")
      filtered_df <- ZooDietAll %>%
        filter(!is.na(consumer)) %>%
        filter(str_detect(consumer, regex(pattern)))
      
      # Add matched consumers to All_species
      matched_species <- filtered_df %>%
        distinct(Species = consumer)
      
      All_species <- bind_rows(All_species, matched_species) %>%
        distinct()
      
      # Filter ZooDietAll to keep only consumers in master species list
      ZooDietAll <- ZooDietAll %>%
        filter(consumer %in% All_species$Species)
      
      # Prepare resource replacement map
      replacements <- matched_species$Species %>%
        split(str_extract(., "^[^ ]+")) %>%
        (\(x) x[names(x) %in% base_species])()
      
      # Split ZooFishDiet.all.two into entries to replace and those to keep
      to_replace <- ZooFishDiet.all.two$resource %in% names(replacements)
      df_keep <- ZooFishDiet.all.two[!to_replace, ]
      df_replace <- ZooFishDiet.all.two[to_replace, ]
      
      # Expand replacement entries
      tryCatch({
        df_expanded <- df_replace %>%
          rowwise() %>%
          mutate(resource = list(replacements[[resource]])) %>%
          unnest(resource) %>%
          ungroup()
      }, error=function(e){
        message(paste("failed for Iteration", x))})
      
      # Combine datasets into final version
      
      ZooFishDiet.all.two <- df_keep
      tryCatch({
        ZooFishDiet.all.two <- rbind(ZooFishDiet.all.two, df_expanded)
      }, error=function(e){
        message(paste("failed for Iteration", x))})
      
      ## Find feeding group information for all zoo prey species 
      ZooIntInfo<- filter(IntSpInfo, Species %in% ZooDietAll$consumer)%>%
        dplyr::select(Species, feeding.group, diet)
      
      colnames(ZooIntInfo)<-c("consumer", "Feeding.Group", "resource")
      
      ZooDietAll<-na.omit(unique(as.data.frame(rbind(ZooDietAll, ZooIntInfo))))
      
      if(nrow(ZooDietAll) != 0) {
        
        dietsplit <- colsplit(ZooDietAll$resource, ", ", paste0("V", 1:10))
        
        ZooDietcrsv1 <- bind_cols(
          ZooDietAll %>% dplyr::select(consumer, Feeding.Group),
          dietsplit
        ) %>% distinct()
        
        ZooDietcrs <- reshape2::melt(
          ZooDietcrsv1,
          measure.vars = paste0("V", 1:10)
        ) %>%
          dplyr::select(consumer, Feeding.Group, resource = value) %>%
          dplyr::filter(!is.na(resource) & resource != "") %>%
          dplyr::distinct()
        
        # Add new records
        ZooDietcrs <- bind_rows(ZooDietcrs, NewZoo)
        
        # Standardize spelling
        ZooDietcrs <- ZooDietcrs %>%
          mutate(resource = str_replace(resource, "Omnivourous", "Omnivorous"))
        
        # Define basal resources
        basal <- c("FPOM", "Microalgae", "Phytoplankton", "Macroalgae", "Fungi", "Bacteria", "CPOM")
        
        # Extract basal food web links
        BasalFW <- ZooDietcrs %>%
          filter(resource %in% basal) %>%
          distinct(consumer, resource)
        
        # Remove basal from main diet data
        ZooDietcrs <- ZooDietcrs %>%
          filter(!resource %in% basal)
        
        # Helper function to build diet pairings by linking consumer prey species with resouce prey speceis present
        build_diet <- function(group, data) {
          sp <- unique(subset(data, Feeding.Group == group)$consumer)
          re <- unique(subset(data, Feeding.Group == group)$resource)
          resp <- unique(subset(data, Feeding.Group %in% re)$consumer)
          
          n_sp <- length(sp)
          n_resp <- length(resp)
          
          if (n_sp > 0 && n_resp > 0) {
            df <- unique(data.frame(
              consumer = rep(sp, each = n_resp),
              resource = rep(resp, times = n_sp)
            ))
            return(df)
          } else {
            return(data.frame(consumer = character(0), resource = character(0)))
          }
        }
        
        # Feeding groups to process
        feeding_groups <- c(
          "Herbivorous Zooplankton",
          "Omnivorous Zooplankton",
          "Decapoda",
          "Hydrozoa",
          "Tunicate",
          "Barnacles",
          "Bivalves",
          "Bryozoa",
          "Omnivorous Echinoderms",
          "Herbivorous Echinoderms",
          "Herbivorous Gastropoda",
          "Ominivorous Gastropoda",
          "Cephalopoda", 
          "Omnivirous Cnidaria", 
          "Herbivorous Cnidaria", 
          "Spongiidae"
        )
        
        # Apply function across feeding groups
        diet_list <- lapply(feeding_groups, build_diet, data = ZooDietcrs)
        names(diet_list) <- feeding_groups
        
        # Individual diet data of each feeding group
        HerbZooDietFin <- diet_list[["Herbivorous Zooplankton"]]
        OmniZooDietFin <- diet_list[["Omnivorous Zooplankton"]]
        DecaDietFin    <- diet_list[["Decapoda"]]
        HydrozoaDietFin <- diet_list[["Hydrozoa"]]
        TunicateDietFin <- diet_list[["Tunicate"]]
        BarnaclesDietFin <- diet_list[["Barnacles"]]
        BivalvesDietFin <- diet_list[["Bivalves"]]
        BryozoaDietFin <- diet_list[["Bryozoa"]]
        OmEchiDietFin <- diet_list[["Omnivorous Echinoderms"]]
        HerbEchiDietFin <- diet_list[["Herbivorous Echinoderms"]]
        HerbGastropodaDietFin <- diet_list[["Herbivorous Gastropoda"]]
        OmniGastropodaDietFin <- diet_list[["Ominivorous Gastropoda"]]
        CephalopodaDietFin <- diet_list[["Cephalopoda"]]
        OmniCnidariaDietFin <- diet_list[["Omnivirous Cnidaria"]]
        HerbCnidariaDietFin <- diet_list[["Herbivorous Cnidaria"]]
        SpongiidaeDietFin <- diet_list[["Spongiidae"]]
        
        # Special case for prey speceis that consumer small teleosts
        Fish <- rbind(ZooFishDiet.all.two, Ad_Fish)
        Fishinfomerge <- merge(Fish, FishInfo, by.x = "consumer", by.y = "Species", all.x = TRUE)
        SmallTeleostRESp <- unique(subset(Fishinfomerge, small.teleost == 1)$consumer)
        SmallTeleostSp <- unique(subset(ZooDietcrs, resource == "SmallTeleost")$consumer)
        
        n_st_sp <- length(SmallTeleostSp)
        n_st_resp <- length(SmallTeleostRESp)
        
        SmallTeleostDietFin <- if (n_st_sp > 0 && n_st_resp > 0) {
          unique(data.frame(
            consumer = rep(SmallTeleostSp, each = n_st_resp),
            resource = rep(SmallTeleostRESp, times = n_st_sp)
          ))
        } else {
          data.frame(consumer = character(0), resource = character(0))
        }
        
        tryCatch({
          ZooDiet<-na.omit(as.data.frame(HerbZooDietFin))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, OmniZooDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, DecaDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, HydrozoaDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, TunicateDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, BarnaclesDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, BivalvesDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, OmEchiDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, HerbGastropodaDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, OmniGastropodaDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-(na.omit(as.data.frame(rbind(ZooDiet, CephalopodaDietFin))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-unique((na.omit(as.data.frame(rbind(ZooDiet, CnidariaDietFin)))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-unique((na.omit(as.data.frame(rbind(ZooDiet, HerbEchiDietFin)))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-unique((na.omit(as.data.frame(rbind(ZooDiet, OmniCnidariaDietFin)))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-unique((na.omit(as.data.frame(rbind(ZooDiet, HerbCnidariaDietFin)))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-unique((na.omit(as.data.frame(rbind(ZooDiet, SpongiidaeDietFin )))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        tryCatch({
          ZooDiet<-unique((na.omit(as.data.frame(rbind(ZooDiet, SmallTeleostDietFin)))))
        }, error=function(e){
          message(paste("failed for Iteration", x))})
        
        # Clean and optimize food web construction pipeline
        
        # Clean ZooDiet
        ZooDiet <- ZooDiet[ZooDiet$consumer != "Decapoda" & ZooDiet$resource != "Decapoda", ]
        ZooDiet <- as.data.frame(ZooDiet)
        ZooDiet$consumer <- as.factor(as.character(unlist(ZooDiet$consumer)))
        ZooDiet$resource <- as.factor(as.character(unlist(ZooDiet$resource)))
        
        if(nrow(ZooDiet)!= 0) {
          
          # Combine all interactions: fish-fish, fish-zoo, zoo-zoo, and basal
          FWTL <- unique(rbind(
            ZooFishDiet.all.two[, c(1, 2)],
            Ad_Fish[, c(1, 2)],
            ZooDiet,
            BasalFW
          ))
          
          # Standardize some taxonomic names
          FWTL$consumer[FWTL$consumer == "Ophiurida"] <- "Ophiuridae"
          
          replace_terms <- function(x) {
            gsub("(?i)fish (larvae|eggs)", "ichthyoplankton", x, perl = TRUE)
          }
          FWTL$consumer <- replace_terms(FWTL$consumer)
          FWTL$resource <- replace_terms(FWTL$resource)
          
          # Remove rows with duplicate consumer/resource (self-loops or carnivorous repeats)
          FWTL <- FWTL[FWTL$consumer != FWTL$resource, ]
          
          # Create list of all unique nodes
          nodes <- unique(data.frame(node = c(FWTL$consumer, FWTL$resource)))
          
          # Merge with species attributes biomas (M) and abundance (N)
          FWMerge <- nodes %>%
            left_join(Fishweight, by = c("node" = "Species"), multiple = "all") %>%
            left_join(NewIntdfAll, by = c("node" = "New_Species"), multiple = "all") %>%
            dplyr::select(node, M, N, pred_weight_g, prey_ind_weight_g, prey_count) %>%
            mutate(
              MM = coalesce(M, pred_weight_g),
              MMM = coalesce(MM, prey_ind_weight_g),
              MMM = as.numeric(MMM),
              N = as.numeric(N),
              prey_count = as.numeric(prey_count)
            ) %>%
            group_by(node) %>%
            dplyr::summarise(
              M = min(na.omit(MMM)),
              N = mean(na.omit(N)),
              pc = sum(prey_count, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(N = coalesce(N, pc)) %>%
            dplyr::select(node, M, N) %>%
            filter(!is.na(M))
          
          FWMerge$N[FWMerge$N == 0] <- 1
          FWMerge$M<-FWMerge$M+0.000001
          
          # Add basal species traits
          Basel <- c("FPOM", "Phytoplankton", "Macroalgae", "Fungi", "Bacteria", "CPOM", "Microalgae")
          BaselM <- rep(min(FWMerge$M) / 2, length(Basel))
          BaselM[Basel == "Fungi" | Basel == "Microalgae"] <- 1
          BaselN <- rep(max(na.omit(FWMerge$N)) * 10, length(Basel))
          Baseldf <- data.frame(node = Basel, M = BaselM, N = BaselN)
          
          # Final node trait table
          i<-x
          FinFW[[i]] <- unique(rbind(FWMerge, Baseldf)) %>%
            mutate(across(c(M, N), ~na_if(., Inf))) %>%
            replace(is.na(.), 1) %>%
            filter(!(node %in% Basel & N == 1)) 
          
          baselintsp<-filter(IntSpInfo, feeding.group %in% Baseldf$node)
          FinFW[[i]] <- FinFW[[i]] %>%
            mutate(node = as.character(node)) %>%
            left_join(baselintsp %>% dplyr::select(Species, feeding.group),
                      by = c("node" = "Species")) %>%
            mutate(node = if_else(!is.na(feeding.group), feeding.group, node)) %>%
            dplyr::select(-feeding.group)
          
          FinFW[[i]] <- FinFW[[i]] %>%
            group_by(node) %>%
            dplyr::summarise(          
              M = mean(M),
              N = mean(N)
            )
          
          FinFW[[i]]<-as.data.frame(FinFW[[i]])
          
          # Filter TL by existing nodes
          TL[[i]] <- FWTL %>% filter(consumer %in% FinFW[[i]]$node & resource %in% FinFW[[i]]$node) %>% unique()
          
          # Remove species with no links and not basal
          consumer<-unique(as.data.frame(TL[[i]]$consumer))
          colnames(consumer)<-c("Species")
          resource<-unique(as.data.frame(TL[[i]]$resource))
          colnames(resource)<-c("Species")
          
          NoLinks<-resource %>% filter(!resource$Species %in% consumer$Species)
          NoLinks<-NoLinks %>% filter(!NoLinks$Species %in% Baseldf$node)
          
          TL[[i]] <- TL[[i]] %>%
            filter(!if_any(everything(), ~ . %in% NoLinks$Species))
          FinFW[[i]] <- FinFW[[i]] %>%
            filter(!if_any(everything(), ~ . %in% NoLinks$Species))
          
          
          # Final filter to sync TL and FinFW[[i]]  (ensure no species are present in FW and not in trophic links and vice versa)
          FinFW[[i]] <- as.data.frame(FinFW[[i]][FinFW[[i]]$node %in% unique(c(TL[[i]]$consumer, TL[[i]]$resource)), ])
          TL[[i]] <- TL[[i]][TL[[i]]$consumer %in% FinFW[[i]]$node & TL[[i]]$resource %in% FinFW[[i]]$node, ]
          
          
          # Do a final conversion of less specific naming to species of the same genus present
          All_species_one_word <- unique(subset(FinFW[[i]], !grepl("_", node) & !grepl(" ", node)))
          base_species<-unique(as.vector(as.character(All_species_one_word$node)))
          pattern <- paste(base_species, collapse = "|")
          
          filtered_df <- FinFW[[i]] %>%
            filter(!is.na(node)) %>%
            filter(str_detect(node, regex(pattern, ignore_case = FALSE)))
          filtered_df<-as.data.frame(filtered_df$node)
          colnames(filtered_df)<-c("Species")
          filtered_df<-filter(filtered_df, !Species %in% base_species)
          filtered_df<-as.vector(filtered_df$Species)
          
          replacements <- filtered_df %>%
            split(str_extract(., "^[^ ]+")) %>%
            keep(names(.) %in% base_species) 
          
          to_replace <-  FinFW[[i]]$node%in% names(replacements)
          df_keep <- FinFW[[i]][!to_replace, ]
          df_replace <- FinFW[[i]][to_replace, ]
          
          tryCatch({
            df_expanded <- df_replace %>%
              rowwise() %>%
              mutate(node = list(replacements[[node]])) %>%
              unnest(node) %>%
              ungroup()
          }, error=function(e){
            message(paste("failed for Iteration", x))})
          
          FinFW[[i]] <- df_keep
          tryCatch({
            FinFW[[i]] <- rbind(FinFW[[i]], df_expanded)
          }, error=function(e){
            message(paste("failed for Iteration", x))})
          
          to_replace <-  TL[[i]]$consumer%in% names(replacements)
          df_keep <- TL[[i]][!to_replace, ]
          df_replace <- TL[[i]][to_replace, ]
          
          tryCatch({
            df_expanded <- df_replace %>%
              rowwise() %>%
              mutate(consumer = list(replacements[[consumer]])) %>%
              unnest(consumer) %>%
              ungroup()
          }, error=function(e){
            message(paste("failed for Iteration", x))})
          
          TL[[i]] <- df_keep
          tryCatch({
            TL[[i]] <- rbind(TL[[i]], df_expanded)
          }, error=function(e){
            message(paste("failed for Iteration", x))})
          
          to_replace <-  TL[[i]]$resource%in% names(replacements)
          df_keep <- TL[[i]][!to_replace, ]
          df_replace <- TL[[i]][to_replace, ]
          
          tryCatch({
            df_expanded <- df_replace %>%
              rowwise() %>%
              mutate(resource = list(replacements[[resource]])) %>%
              unnest(resource) %>%
              ungroup()
          }, error=function(e){
            message(paste("failed for Iteration", x))})
          
          TL[[i]] <- df_keep
          tryCatch({
            TL[[i]] <- rbind(TL[[i]], df_expanded)
          }, error=function(e){
            message(paste("failed for Iteration", x))})
          
          # finalise the FW by finding means of duplicated species
          FinFW[[i]] <- FinFW[[i]] %>%
            group_by(node) %>%
            dplyr::summarise(          
              M = mean(M),
              N = mean(N)
            )
          
          
          
          FinFW[[i]]<-as.data.frame(FinFW[[i]])
          
          TL[[i]]<-unique(TL[[i]])
          
          SST_sub <- SST %>%
            filter(
              between(Longitude, Lon - 2, Lon + 2),
              between(Latitude,  Lat - 2, Lat + 2),
              Year == Year_i
            )
          
          # Convert to sf objects (WGS84)
          pts1 <- st_as_sf(FW, coords = c("Longitude", "Latitude"), crs = 4326)
          pts2 <- st_as_sf(SST_sub, coords = c("Longitude", "Latitude"), crs = 4326)
          
          pts1_m <- st_transform(pts1, 3857)
          pts2_m <- st_transform(pts2, 3857)
          
          dist_matrix <- st_distance(pts1_m, pts2_m) #/ 1000  # km
          cols_within_200 <- apply(dist_matrix, 2, function(x) any(x <= 100000))
          SST_sub <- SST_sub[cols_within_200, ]
          
          ##Temperature is needed to compute fluxes in the next step. 
          ##Daily sea surface temperature data (°C) from both satellite and in situ observations were 
          ##extracted from the Copernicus open access data repository 
          ##(Good et al., 2020, Donlon et al., 2012, Stark et al., 2007). 
          
          SST_mean<-mean(SST_sub$Avg_SST, na.rm = TRUE) ## 
          SST_sd<-sd(SST_sub$Avg_SST, na.rm = TRUE)
          
          FinFW[[i]]$SST_mean <- SST_mean
          FinFW[[i]]$SST_sd <- SST_sd
          FinFW[[i]]$Longitude<-Lon
          FinFW[[i]]$Latitude<-Lat
          FinFW[[i]]$Year<-Year_i
          
          #getting feeding group information of species present in the food web
          FinFW[[i]] <- FinFW[[i]] %>%
            left_join(IntSpInfo, by = c("node" =  "Species")) %>% dplyr::select(node, M, N, SST_mean, SST_sd, feeding.group, Longitude, Latitude, Year)
          FinFW[[i]]$feeding.group[is.na(FinFW[[i]]$feeding.group)] <- "fish"
          FinFW[[i]]$feeding.group[FinFW[[i]]$feeding.group == ""] <- "fish"
          
          
          #save final data sets , this saves each individual FW an TL, delete if you dont want this
          saveRDS(FinFW[[i]], paste0("FinFW_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
          saveRDS(TL[[i]], paste0("FWTL_", Lon_name, "_", Lat_name, "_", Year_i, ".rdata"))
          rm(SST_sub, FWDiet, NewIntdf, AllGenusv1, Temp1, Genusmerge,Fish, ZooFishDiet.all.two, Fishinfomerge, DietGenus, NewIntdf11)
          gc()
        }
      }
    }
  }, error=function(e){
    message(paste("failed for Iteration", x))})
}

setwd()  #set working directory to save final lists
saveRDS(FinFW, "FinFW.rdata")
saveRDS(TL, "FWTL.rdata")

##------Estimate trophic level weight with Fluxweb-----

## Change to macth you own biomas estimates of basel species
min_weight <- if (exists("NewIntdfAll")) min(na.omit(NewIntdfAll$prey_ind_weight_g[NewIntdfAll$prey_ind_weight_g != 0]), na.rm = TRUE) else 1e-6
max_Abu    <- if (exists("fish_survey")) max(na.omit(fish_survey$DensAbund_N_Sqkm), na.rm = TRUE) else 1

basel_weight <- min_weight / 2
basel_Abu    <- max_Abu * 10
basel <- c("FPOM", "Microalgae", "Phytoplankton", "Macroalgae", "Fungi", "Bacteria", "CPOM")

CPOM_biomass<-25000
FPOM_biomass<-25000
Phytoplankton_biomass <- 7500
Microalage_biomass <- (105+1460)/6
Bacteria_biomass <- (105+1460)/3
Fungi_biomass<- (105+1460)/3
Macroalgae_biomass<- (105+1460)/6

# Apply baselines across FinFW list if it is a list-of-dataframes
if (is.list(FinFW)) {
  for (ii in seq_along(FinFW)) {
    if (!is.null(FinFW[[ii]]$node)) {
      FinFW[[ii]]$M[FinFW[[ii]]$node %in% basel] <- basel_weight
      FinFW[[ii]]$N[FinFW[[ii]]$node %in% basel] <- basel_Abu
    }
  }
}


# --- constants & feed groups ---
boltz <- 0.00008617343
feeding_groups <- c(
  "Herbivorous Zooplankton","Omnivorous Zooplankton","Decapoda","Hydrozoa","Tunicate",
  "Barnacles","Bivalves","Bryozoa","Omnivorous Echinoderms","Herbivorous Echinoderms",
  "Herbivorous Gastropoda","Ominivorous Gastropoda","Cephalopoda","Omnivirous Cnidaria",
  "Herbivorous Cnidaria","Spongiidae"
)

# --- prepare lists ---
n_webs <- length(TL)
weight <- vector("list", n_webs)
TST <- vector("list", n_webs)
flux <- vector("list", n_webs)
It <- vector("list", n_webs)
LatID <- vector("list", n_webs)
Year <- vector("list", n_webs)
LonID <- vector("list", n_webs)
problematic_list <- vector("list", n_webs)
deleted_links_list <- vector("list", n_webs)
failed_iterations <- list()
failed_iterations <- list()

for (i in 1:length(TL)) { 
  cat("\n\n===== Processing food web", i, "=====\n")
  problematic_nodes <- list()
  deleted_links <- list()
  
  links <- TL[[i]] %>% filter(!is.na(resource), !is.na(consumer), resource != consumer)
  biomass_df <- FinFW[[i]]
  
  # Prepare biomass_df
  biomass_df <- biomass_df %>%
    filter(!is.na(node)) %>%
    mutate(M = as.numeric(M), N = as.numeric(N)) %>%
    mutate(B = M * N,
           ind_mass = ifelse(N > 0, B / N, NA_real_),
           group = case_when(
             feeding.group %in% feeding_groups ~ "invertebrate",
             TRUE ~ "fish"
           ))
  
  biomass_df$B[biomass_df$node == "Microalgae"] <- Microalage_biomass
  biomass_df$B[biomass_df$node == "Phytoplankton"] <- Phytoplankton_biomass
  biomass_df$B[biomass_df$node == "Macroalgae"] <- Macroalgae_biomass
  biomass_df$B[biomass_df$node == "Fungi"] <- Fungi_biomass
  biomass_df$B[biomass_df$node == "Bacteria"] <- Bacteria_biomass
  biomass_df$B[biomass_df$node == "FPOM"] <- FPOM_biomass
  biomass_df$B[biomass_df$node == "CPOM"] <- CPOM_biomass
  
  biomass_df$group[biomass_df$node %in% c("Microalgae","Phytoplankton","Macroalgae","Fungi","Bacteria")] <- "plant"
  biomass_df$group[biomass_df$node %in% c("FPOM", "CPOM")] <- "detritus"
  
  temp_col <- if ("Temp" %in% names(biomass_df)) "Temp" else names(biomass_df)[min(4, ncol(biomass_df))]
  temp_val <- suppressWarnings(as.numeric(biomass_df[1, temp_col]))
  if (is.na(temp_val)) temp_val <- 10
  
  TS.kT.e <- ((273.15 + temp_val) - 293.15) / (boltz * (273.15 + temp_val) * 293.15)
  
  biomass_df <- biomass_df %>%
    mutate(efficiency = case_when(
      group == "plant" ~ exp(0.179) * exp(0.164 * TS.kT.e) / (1 + exp(0.179) * exp(0.164 * TS.kT.e)),
      group == "detritus" ~ exp(-1.670) * exp(0.164 * TS.kT.e) / (1 + exp(-1.670) * exp(0.164 * TS.kT.e)),
      group %in% c("invertebrate","fish") ~ exp(2.266) * exp(0.164 * TS.kT.e) / (1 + exp(2.266) * exp(0.164 * TS.kT.e)),
      TRUE ~ NA_real_
    ))
  
  # precompute lookup vectors (named)
  B_vec    <- setNames(biomass_df$B, biomass_df$node)
  M_vec    <- setNames(biomass_df$M, biomass_df$node)
  indM_vec <- setNames(biomass_df$ind_mass, biomass_df$node)
  G_vec    <- setNames(biomass_df$group, biomass_df$node)
  Eff_vec  <- setNames(biomass_df$efficiency, biomass_df$node)
  
  reciprocals <- links %>%
    inner_join(links, by = c("resource" = "consumer", "consumer" = "resource")) %>%
    distinct() %>% filter(resource < consumer)
  
  cleaned_links <- links %>%
    anti_join(reciprocals, by = c("resource","consumer")) %>%
    anti_join(reciprocals %>% rename(resource = consumer, consumer = resource), by = c("resource","consumer"))
  
  try_fluxing <- function(cleaned_links) {
    tryCatch({
      mat <- List.to.matrix(cleaned_links)
      nodes <- unique(c(cleaned_links$resource, cleaned_links$consumer))
      valid_nodes <- nodes[!is.na(B_vec[nodes]) & !is.na(Eff_vec[nodes])]
      mat <- mat[valid_nodes, valid_nodes, drop = FALSE]
      fluxing(mat,
              biomasses = B_vec[valid_nodes],
              losses = 0.71 * M_vec[valid_nodes]^(-0.25),
              efficiencies = Eff_vec[valid_nodes],
              bioms.prefs = TRUE, bioms.losses = TRUE)
      TRUE
    }, error = function(e) FALSE)
  }
  
  # process fluxing iterating through the reciprocals
  if (nrow(reciprocals) > 0) {
    for (j in seq_len(nrow(reciprocals))) {
      A <- reciprocals$resource[j]; B <- reciprocals$consumer[j]
      gA <- G_vec[A]; gB <- G_vec[B]
      mA <- indM_vec[A]; mB <- indM_vec[B]
      
      if (is.na(gA) || is.na(gB) || is.na(mA) || is.na(mB)) {
        problematic_nodes <- c(problematic_nodes, list(c(A,B)))
        next
      }
      
      if (gA == "fish" && gB == "fish") {
        candidate <- bind_rows(cleaned_links, tibble(resource = A, consumer = B), tibble(resource = B, consumer = A))
        if (try_fluxing(candidate)) cleaned_links <- candidate else problematic_nodes <- c(problematic_nodes, list(c(A,B)))
      } else {
        if (mA < mB) {
          candidate <- bind_rows(cleaned_links, tibble(resource = B, consumer = A))
          if (try_fluxing(candidate)) {
            cleaned_links <- candidate
            deleted_links <- c(deleted_links, list(c(A,B)))
          } else problematic_nodes <- c(problematic_nodes, list(c(A,B)))
        } else {
          candidate <- bind_rows(cleaned_links, tibble(resource = A, consumer = B))
          if (try_fluxing(candidate)) {
            cleaned_links <- candidate
            deleted_links <- c(deleted_links, list(c(B,A)))
          } else problematic_nodes <- c(problematic_nodes, list(c(A,B)))
        }
      }
    }
  }
  
  # --- compute link deletion statistic ---
  orig_links <- nrow(links)
  final_links <- nrow(cleaned_links)
  deleted_pct <- (1 - (final_links / orig_links)) * 100
  
  # save the summary
  Lat  <- unique(biomass_df$Latitude)
  Long <- unique(biomass_df$Longitude)
  Year <- unique(biomass_df$Year)
  
  link_summary <- data.frame(
    Year = Year,
    Longitude = Long,
    Latitude = Lat,
    Web = i,
    Original_Links = orig_links,
    Final_Links = final_links,
    Deleted_Links = orig_links - final_links,
    Deleted_Percent = deleted_pct
  )
  
  # save it
  setwd()
  write.csv(link_summary, paste0("LinkSummary_", Long, "_", Lat, "_", Year, ".csv"), row.names = FALSE)
  
  # --- update TL and FinFW for consistency ---
  TL[[i]] <- cleaned_links
  active_nodes <- union(TL[[i]]$resource, TL[[i]]$consumer)
  FinFW[[i]] <- biomass_df %>% filter(node %in% active_nodes)
  
  # --- save filtered FinFW ---
  setwd()
  saveRDS(FinFW[[i]], paste0("FinFW_", Long, "_", Lat, "_", Year, ".rdata"))
  
  problematic_list[[i]] <- problematic_nodes
  deleted_links_list[[i]] <- deleted_links
  
  # --- Final fluxing attempt with succesful links---
  final_ok <- tryCatch({
    mat <- List.to.matrix(TL[[i]])
    nodes <- active_nodes[!is.na(B_vec[active_nodes]) & !is.na(Eff_vec[active_nodes])]
    mat <- mat[nodes, nodes, drop = FALSE]
    
    mat.fluxes <- fluxing(mat,
                          biomasses = B_vec[nodes],
                          losses = 0.71 * M_vec[nodes]^(-0.25),
                          efficiencies = Eff_vec[nodes],
                          bioms.prefs = TRUE, bioms.losses = TRUE)
    
    flux_df <- melt(as.matrix(mat.fluxes))
    colnames(flux_df) <- c("resource", "consumer", "weight")
    flux_df <- flux_df %>% filter(weight > 0)
    
    flux_df$Year <- Year
    flux_df$Long <- Long
    flux_df$Lat  <- Lat
    flux_df$It   <- i
    
# saves each individual flux delete if you dont want this
    saveRDS(flux_df, paste0("Flux_", Long, "_", Lat, "_", Year, ".rdata"))
    
    flux[[i]] <- flux_df
    cat("? Final fluxing succeeded for web", i, "\n")
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    cat("? Final fluxing failed for web", i, ":", msg, "\n")
    failed_iterations[[length(failed_iterations) + 1]] <<- list(iteration = i, message = msg)
    FALSE
  })
}

setwd() # save final flux data
saveRDS(flux, "flux.rdata")


## Final data lists for further analysis are FinFW and flux
