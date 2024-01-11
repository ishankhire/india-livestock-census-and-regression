#Importing libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(gt)
library(gtsummary)
library(broom)
library(sf)
library(rvest)
library(viridisLite)
library(ggrepel)
library(ggthemes)
library(openxlsx)
library(writexl)
library(units)
library(gridExtra)
library(RColorBrewer)
library(scales)

#Settings

theme_set(theme_bw())
options(scipen=10000)
set_gtsummary_theme(list("tbl_regression-chr:tidy_columns" = c("estimate", "std.error", "p.value")))

#Function to create gtsummary tables
create_table_function <- function(regression_model) {
  regression_table <- tbl_regression(regression_model,
                                     intercept = TRUE)
  regression_table_2 <- add_glance_table(
    regression_table,
    include = c(nobs, r.squared)
  )
  
  return(regression_table_2)
  
}

weighted_lm_func <- function(df, x_var, y_var, weight) {
  regression_data <- df[, c(x_var, y_var, weight), drop = FALSE]
  regression_model <- lm(df[[y_var]] ~ df[[x_var]], regression_data, weights = regression_data[[weight]])
  return(regression_model)
}

#Import data
poultry_data_latest <- read_excel("Final Datasets/poultry_data_latest.xlsx")
cattle_data_latest <- read_excel("Final Datasets/cattle_data_latest.xlsx")
all_district_data <- read_excel("Final Datasets/all_district_data.xlsx", 
                                sheet = "map_data")
all_district_data_regression <- read_excel("Final Datasets/all_district_data.xlsx", 
                                           sheet = "data_for_regression_analysis")
cross_census_data <- read_excel("Final Datasets/2012_2019_analysis.xlsx")
View(all_district_data)
View(all_district_data_regression)
#Remove the last column (all India data) from the dataset
poultry_data_latest <- poultry_data_latest[-37, ]
cattle_data_latest <- cattle_data_latest[-37, ]
cross_census_data <- cross_census_data[-37, ]


#Log GDP per capita and productivity
poultry_data_latest$log_percapita <- log(poultry_data_latest$percapita_USD_PPP,
                                         base = 10)
cattle_data_latest$log_percapita <- log(cattle_data_latest$percapita_USD_PPP,
                                        base = 10)
cattle_data_latest$log_productivity <- log(cattle_data_latest$estimated_cattle_productivity,
                                           base = 10)
all_district_data_regression$log_percapita <- log(all_district_data_regression$percapita_USD_PPP,
                                        base = 10)

#Plotting the map
district_shapefile <- read_sf("D:/Animal Welfare Research/district.shp")
areas <- st_area(district_shapefile)
district_shapefile$area <- drop_units(areas)
district_shapefile$area_in_km2 <- district_shapefile$area / 1000000
district_shapefile_filtered <- district_shapefile[!is.null(district_shapefile$d_name) & !is.na(district_shapefile$d_name), ]
merged_data <- cbind(district_shapefile_filtered, all_district_data)

#Function to create maps
create_map_function <- function(map_breaks, map_labels, variable, map_title, map_colours)   {
  map_data <- merged_data_filtered
  map_data$discrete_variable <- cut(map_data[[variable]], map_breaks, 
                                    labels = map_labels)
  the_map <- ggplot(map_data) +
    geom_sf(aes(fill = discrete_variable), color=NA) +
    theme_map() +
    theme(legend.position = "right") +
    labs(title = map_title, fill = "Animals/km^2") +
    scale_fill_manual(values = map_colours, drop = FALSE)
  
  return(the_map)
}

#################################
###   Making district maps    ###
################################

#Intensive Fowl Map
merged_data$intensive_fowl_density <- merged_data$total_commercial_fowl / merged_data$area_in_km2

#The only value which has gotten filtered out is the "Mumbai suburban" district which had a null value as it was not included in the census
merged_data_filtered <- merged_data[!is.null(merged_data$intensive_fowl_density) & !is.na(merged_data$intensive_fowl_density), ]

#Intensive Fowl Map
fowl_breaks <- c(-0.01, 1, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, Inf)
fowl_labels = c("0-1", "1-10", "10-25", "25-50", "50-100","100-250", "250-500", 
                "500-1000", "1000-2500","2500-5000", "5000-10000", "10000+")
intensive_fowl_title <- "Density of Intensive Fowl"
fowl_colours <- colorRampPalette(c("#E6E6A8", "#910202"))(length(fowl_labels))
intensive_fowl_map <- create_map_function(map_breaks = fowl_breaks, map_labels = fowl_labels, 
                                          variable = "intensive_fowl_density", map_title = intensive_fowl_title, map_colours = fowl_colours)

#Extensive Fowl Map
merged_data_filtered$extensive_fowl_density <- merged_data_filtered$fowl_backyard / merged_data_filtered$area_in_km2
extensive_fowl_title <- "Density of Extensive Fowl"
extensive_fowl_map <- create_map_function(map_breaks = fowl_breaks, map_labels = fowl_labels,
                                          variable = "extensive_fowl_density", map_title = extensive_fowl_title, map_colours = fowl_colours)

#Exotic Cattle
merged_data_filtered$exotic_cattle_density <- merged_data_filtered$exotic / merged_data_filtered$area_in_km2
cattle_breaks <- c(-0.01, 1, 5, 10, 25, 50, 100, 250, Inf)
cattle_labels <- c("0-1", "1-5", "5-10", "10-25", "25-50", "50-100", "100-250", "250+")
exotic_cattle_title <- "Density of Exotic Cattle"
cattle_colours <- colorRampPalette(c("lightblue", "purple"))(length(cattle_labels))
exotic_cattle_map <- create_map_function(map_breaks = cattle_breaks, map_labels = cattle_labels,
                                          variable = "exotic_cattle_density", map_title = exotic_cattle_title, map_colours = cattle_colours)

#Indigenous Cattle
merged_data_filtered$indigenous_cattle_density <- merged_data_filtered$indigenous / merged_data_filtered$area_in_km2
indigenous_cattle_title <- "Density of Indigenous Cattle"
indigenous_cattle_map <- create_map_function(map_breaks = cattle_breaks, map_labels = cattle_labels,
                                         variable = "indigenous_cattle_density", map_title = indigenous_cattle_title, map_colours = cattle_colours)

combined_maps <- arrangeGrob(intensive_fowl_map, extensive_fowl_map, 
                                 exotic_cattle_map, indigenous_cattle_map, ncol = 2)
ggsave("combined_maps.png", plot = combined_maps, width = 12, height = 9)


#################################
###   Regression analysis    ###
################################

#Fowl

poultry_fowl_data <- select(poultry_data_latest, log_percapita,
                            proportion_fowl_commercial, total_fowl)

poultry_fowl_model <- lm(proportion_fowl_commercial ~ log_percapita, data=poultry_fowl_data,
                         weights = poultry_fowl_data$total_fowl)
summary(poultry_fowl_model)

#Scatterplot of proportion of commercial fowl against income
poultry_fowl_data$predicted <- predict(poultry_fowl_model, newdata = poultry_fowl_data)
fowl_scatterplot <- ggplot(data = poultry_fowl_data, aes(x = log_percapita,
                     y = proportion_fowl_commercial)) +
  geom_point(aes(size=total_fowl), alpha=0.5, color="#A03333") +
  scale_size_continuous(labels = comma) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = seq(0, 1, by = 0.25)
  ) +
  scale_x_continuous(
    limits = c(2.9, 4.4),
    breaks = seq(3.2, 4.4, by = 0.4),
    labels = seq(3.2, 4.4, by = 0.4),
  ) +
  geom_line(aes(y=predicted), linewidth = 1.6) +
  labs(title    = "(A) Fowl by State",
       x = "Log of GDP per capita in USD (PPP)",
       y = "Proportion of fowl that are commercial")

#Per capita availability of eggs
poultry_egg_percapita_availability_data <- select(poultry_data_latest, log_percapita,
                                                  egg_percapita_availability, population_2019)

poultry_egg_percapita_availability_model <- lm(egg_percapita_availability ~ log_percapita, data=poultry_egg_percapita_availability_data,
                                               weights = poultry_egg_percapita_availability_data$population_2019)
summary(poultry_egg_percapita_availability_model)

#Per capita availability of poultry meat
poultry_meat_percapita_availability_data <- select(poultry_data_latest, log_percapita,
                                                   poultry_meat_percapita_availability, population_2019)

poultry_meat_percapita_availability_model <- lm(poultry_meat_percapita_availability ~ log_percapita, data=poultry_meat_percapita_availability_data,
                                               weights = poultry_meat_percapita_availability_data$population_2019)
summary(poultry_egg_percapita_availability_model)

#Cattle
#cattle and income, proxy: breed
cattle_breed_proxy_data <- select(cattle_data_latest, log_percapita,
                                  proportion_cattle_exotic, total_cattle)

cattle_breed_proxy_model <- lm(proportion_cattle_exotic ~ log_percapita, data=cattle_breed_proxy_data,
                               weights = cattle_breed_proxy_data$total_cattle)

cattle_breed_proxy_data$predicted <- predict(cattle_breed_proxy_model, newdata=cattle_breed_proxy_data)
summary(cattle_breed_proxy_model)

cattle_scatterplot <- ggplot(data = cattle_breed_proxy_data, aes(x = log_percapita,
                  y = proportion_cattle_exotic)) +
  geom_point(aes(size=total_cattle), alpha = 0.5, color="#4562AE") +
  scale_size_continuous(labels = comma) + 
  geom_line(aes(y = predicted), linewidth = 1.6) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = seq(0, 1, by = 0.25)
  ) +
  scale_x_continuous(
    limits = c(2.9, 4.4),
    breaks = seq(3.2, 4.4, by = 0.4),
    labels = seq(3.2, 4.4, by = 0.4),
  ) +
  labs(title    = "(B) Cattle by State",
       x = "Log of GDP per capita in USD (PPP)",
       y = "Proportion of cattle that are exotic")

cattle_scatterplot

#milk availability per capita and income per capita
ggplot(data = cattle_data_latest, aes(x = log_percapita,
                                      y = percapita_milk_availability)) +
  geom_point(aes(size=population_2019)) +
  labs(title    = "Milk per capita availability to income",
       x = "Log of GDP per Capita in USD (PPP)",
       y = "Per capita milk availability (grams/day)")

cattle_milk_availability_data <- select(cattle_data_latest, log_percapita,
                                        percapita_milk_availability, population_2019)

cattle_milk_availability_model <- lm(percapita_milk_availability ~ log_percapita, data=cattle_milk_availability_data,
                                     weights = cattle_milk_availability_data$population_2019)

summary(cattle_milk_availability_model)
create_table_function(cattle_milk_availability_model)

#District fowl analysis
district_fowl_data <- select(all_district_data_regression, proportion_commercial_fowl,
                                log_percapita, total_fowl)

district_fowl_data_model <- lm(proportion_commercial_fowl ~ log_percapita, data=district_fowl_data,
                             weights = district_fowl_data$total_fowl)
# create_table_function(district_fowl_data_model)
summary(district_fowl_data_model)
fowl_district_outliers <- all_district_data_regression[all_district_data_regression$log_percapita > 3.9 &
                                                         all_district_data_regression$proportion_commercial_fowl < 0.3, ]
all_district_data_regression$fowl_predicted <- predict(district_fowl_data_model, newdata = district_fowl_data)
fowl_district_scatterplot <- ggplot(data = all_district_data_regression, aes(x = log_percapita,
                                         y = proportion_commercial_fowl)) +
  geom_point(alpha = 0.5, aes(size=total_fowl), color = "#A03333") +
  scale_size_continuous(labels = comma) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = seq(0, 1, by = 0.25)
  ) +
  scale_x_continuous(
    limits = c(2.9, 4.4),
    breaks = seq(3.2, 4.4, by = 0.4),
    labels = seq(3.2, 4.4, by = 0.4),
  ) +
  geom_line(aes(y = fowl_predicted), linewidth = 1.6) +
  labs(title    = "(C) Fowl by District",
       x = "Log of GDP per Capita in USD (PPP)",
       y = "Proportion of fowl that are commercial")
fowl_district_scatterplot

View(fowl_district_outliers)
#District cattle analysis

district_cattle_data <- select(all_district_data_regression, proportion_cattle_exotic,
                                log_percapita, total_cattle)

district_cattle_data_model <- lm(proportion_cattle_exotic ~ log_percapita, data=district_cattle_data,
                                  weights = district_cattle_data$total_cattle)
summary(district_cattle_data_model)

# create_table_function(district_cattle_data_model)
all_district_data_regression$cattle_predicted <- predict(district_cattle_data_model, newdata = district_cattle_data)

cattle_district_scatterplot <- ggplot(data = all_district_data_regression, aes(x = log_percapita,
                                                y = proportion_cattle_exotic)) +
  geom_point(aes(size=total_cattle), alpha=0.5, color="#4562AE") +
  scale_size_continuous(labels = comma) +
  geom_line(aes(y=cattle_predicted), linewidth=1.6, na.rm = TRUE) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = seq(0, 1, by = 0.25)
  ) +
  scale_x_continuous(
    limits = c(2.9, 4.4),
    breaks = seq(3.2, 4.4, by = 0.4),
    labels = seq(3.2, 4.4, by = 0.4),
  ) +
  labs(title    = "(D) Cattle by District",
       x = "Log of GDP per Capita in USD (PPP)",
       y = "Proportion of cattle that are exotic")
cattle_district_scatterplot
ggsave("cattle_district_scatterplot.png", plot = cattle_district_scatterplot, width=12, height=8)

cattle_district_outliers <- all_district_data_regression[all_district_data_regression$log_percapita > 3.9 &
                                                         all_district_data_regression$proportion_cattle_exotic < 0.4, ]
View(cattle_district_outliers)

#Making a four-panel image of fowl and cattle scatterplots
combined_plots <- grid.arrange(fowl_scatterplot, cattle_scatterplot, fowl_district_scatterplot, cattle_district_scatterplot, ncol=1)
ggsave("combined_scatterplots_new.png", combined_plots, width = 21, height = 29.7, units = "cm")

#Comparing 2012 and 2019 data
ggplot(data = cross_census_data, aes(x = percentage_change_in_income,
                                                y = change_in_poultry_intensification)) +
  geom_point(aes(size=total_fowl_2012)) +
  labs(title    = "Change in poultry intensification in response to income",
       x = "Percentage change in income",
       y = "Change in poultry intensification")

cross_census_fowl_data <- select(cross_census_data, change_in_fowl_intensification,
                               percentage_change_in_income, total_fowl_2012)

cross_census_fowl_data_model <- lm(change_in_fowl_intensification ~ percentage_change_in_income, data=cross_census_fowl_data,
                                 weights = cross_census_fowl_data$total_fowl_2012)
create_table_function(cross_census_fowl_data_model)

cross_census_cattle_data <- select(cross_census_data, change_in_cattle_intensification,
                            percentage_change_in_income, total_cattle_2012)

cross_census_cattle_data_model <- lm(change_in_cattle_intensification ~ percentage_change_in_income, data=cross_census_cattle_data,
                              weights = cross_census_cattle_data$total_cattle_2012)
create_table_function(cross_census_cattle_data_model)
summary(cross_census_cattle_data_model)

cross_census_data$cattle_predicted <- predict(cross_census_cattle_data_model, newdata = cross_census_cattle_data)
ggplot(data = cross_census_data, aes(x = percentage_change_in_income,
                                     y = change_in_cattle_intensification)) +
  geom_point(aes(size=total_cattle_2012)) +
  geom_line(aes(y=cattle_predicted)) +
  labs(title    = "Change in cattle intensification in response to income",
       x = "Change in Income",
       y = "Change in cattle intensification")

#proportion change in commercial poultry
cross_census_data$proportion_change_commercial_fowl <- cross_census_data$total_fowl_commercial_2019 - cross_census_data$total_fowl_commercial_2012
change_in_commercial_poultry_model <- weighted_lm_func(df = cross_census_data, x_var = "percentage_change_in_income", y_var = "proportion_change_commercial_fowl", weight = "total_fowl_commercial_2012")
summary(change_in_commercial_poultry_model)

####################################
### Code for Supplementary file  ###
####################################

#Correlation between proportion of fowl of improved breed and proportion of fowl in commercial conditions

ggplot(data = poultry_data_latest, aes(x = proportion_fowl_improved,
                                       y = proportion_fowl_commercial)) +
  geom_point(aes(size=total_fowl)) +
  labs(title    = "Scatterplot of breed and intensification",
       x = "Proportion of fowls of improved breed",
       y = "Proportion of chickens that are commercial")

poultry_breed_commercial_data <- select(poultry_data_latest, proportion_fowl_improved,
                                        proportion_fowl_commercial, total_fowl)

poultry_breed_commercial_model <- lm(proportion_fowl_commercial ~ proportion_fowl_improved, data=poultry_breed_commercial_data,
                                     weights = poultry_breed_commercial_data$total_fowl)

summary(poultry_breed_commercial_model)
create_table_function(poultry_breed_commercial_model)

#Duck
ggplot(data = poultry_data_latest, aes(x = log_percapita,
                                       y = proportion_ducks_commercial)) +
  geom_point(aes(size=total_poultry)) +
  labs(title    = "Ducks Scatterplot",
       x = "log of GDP per Capita in USD (PPP)",
       y = "Proportion of ducks that are commercial")

poultry_ducks_data <- select(poultry_data_latest, log_percapita,
                             proportion_ducks_commercial, total_poultry)

poultry_ducks_model <- lm(proportion_ducks_commercial ~ log_percapita, data=poultry_ducks_data,
                          weights = poultry_ducks_data$total_poultry)

create_table_function(poultry_ducks_model)

#Turkeys
ggplot(data = poultry_data_latest, aes(x = log_percapita,
                                       y = proportion_turkey_commercial)) +
  geom_point(aes(size=total_poultry)) +
  labs(title    = "Turkey Scatterplot",
       x = "log of GDP per Capita in USD (PPP)",
       y = "Proportion of turkey that are commercial")

poultry_turkey_data <- select(poultry_data_latest, log_percapita,
                              proportion_turkey_commercial, total_poultry)

poultry_turkey_model <- lm(proportion_turkey_commercial ~ log_percapita, data=poultry_turkey_data,
                           weights = poultry_turkey_data$total_poultry)

create_table_function(poultry_turkey_model)

#Quail
ggplot(data = poultry_data_latest, aes(x = log_percapita,
                                       y = proportion_quail_commercial)) +
  geom_point(aes(size=total_poultry)) +
  labs(title    = "Quail Scatterplot",
       x = "log of GDP per Capita in USD (PPP)",
       y = "Proportion of quail that are commercial")

poultry_quail_data <- select(poultry_data_latest, log_percapita,
                             proportion_quail_commercial, total_poultry)

poultry_quail_model <- lm(proportion_quail_commercial ~ log_percapita, data=poultry_quail_data,
                          weights = poultry_quail_data$total_poultry)

create_table_function(poultry_quail_model)


#Other Poultry Birds
ggplot(data = poultry_data_latest, aes(x = log_percapita,
                                       y = proportion_other_poultry_birds_commercial)) +
  geom_point(aes(size=total_poultry)) +
  labs(title    = "other poultry birds Scatterplot",
       x = "log of GDP per Capita in USD (PPP)",
       y = "Proportion of other poultry birds that are commercial")

poultry_other_poultry_birds_data <- select(poultry_data_latest, log_percapita,
                                           proportion_other_poultry_birds_commercial, total_poultry)

poultry_other_poultry_birds_model <- lm(proportion_other_poultry_birds_commercial ~ log_percapita, data=poultry_other_poultry_birds_data,
                                        weights = poultry_other_poultry_birds_data$total_poultry)

create_table_function(poultry_other_poultry_birds_model)


#Productivity of cattle and breed - may want to move to supplementary file
ggplot(data = cattle_data_latest, aes(x = estimated_cattle_productivity,
                                      y = proportion_cattle_exotic)) +
  geom_point(aes(size=total_cattle)) +
  labs(title    = "Correlation between breed and productivity",
       x = "Productivity of cattle (kg/day)",
       y = "Proportion of cattle that are exotic")

cattle_productivity_breed_correlation_data <- select(cattle_data_latest, estimated_cattle_productivity,
                                                     proportion_cattle_exotic, total_cattle)

cattle_productivity_breed_correlation_model <- lm(proportion_cattle_exotic ~ estimated_cattle_productivity, data=cattle_productivity_breed_correlation_data,
                                                  weights = cattle_productivity_breed_correlation_data$total_cattle)

summary(cattle_productivity_breed_correlation_model)

#Cattle and income, proxy: productivity
ggplot(data = cattle_data_latest, aes(x = log_percapita,
                                      y = estimated_cattle_productivity)) +
  geom_point(aes(size=total_cattle)) +
  labs(title    = "Intensification and income, proxy: productivity",
       x = "log of GDP per Capita in USD (PPP)",
       y = "Cow productivity (kg/day)")

cattle_productivity_proxy_data <- select(cattle_data_latest, log_percapita,
                                         estimated_cattle_productivity, total_cattle)

cattle_productivity_proxy_model <- lm(estimated_cattle_productivity ~ log_percapita, data=cattle_productivity_proxy_data,
                                      weights = cattle_productivity_proxy_data$total_cattle)

summary(cattle_productivity_proxy_model)
create_table_function(cattle_productivity_proxy_model)

#Trying out the code taking log of productivity and percapita (got lower R^2 value)
ggplot(data = cattle_data_latest, aes(x = log_percapita,
                                      y = log_productivity)) +
  geom_point(aes(size=total_cattle)) +
  labs(title    = "Chicken Scatterplot",
       x = "log of GDP per Capita in USD (PPP)",
       y = "Proportion of chickens that are commercial")

cattle_data <- select(cattle_data_latest, log_percapita,
                      log_productivity, total_cattle)

cattle_model <- lm(log_productivity ~ log_percapita, data=cattle_data,
                   weights = cattle_data$total_cattle)

summary(cattle_model)
create_table_function(cattle_model)
