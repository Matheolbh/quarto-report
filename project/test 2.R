install.packages("tidyverse")
install.packages("plotly")
install.packages("maps")
library(maps)
library(tidyverse)
library(plotly)
unicef_indicator_2 <- read_csv("Quarto obs/unicef_indicator_2.csv")
Health_expe_GDP2 <- read_csv("Quarto obs/Health_expe-GDP2.csv")

data_join <- full_join(Health_expe_GDP2,unicef_metadata)
data_join <- full_join(Health_expe_GDP2, unicef_indicator_2)
Joining with `by = join_by(CodeC, country, `Country Code`, year, Region, `Sub-region`)`
> names(data_join)
[1] "CodeC"                   "country"                 "Country Code"           
[4] "Indicator Name"          "year"                    "pourcent_health-exp...6"
[7] "test_percent"            "pourcent_health-exp...8" "Region"                 
[10] "Sub-region"              "time_period"             "obs_value"              
[13] "sex"                    
> data_join <- full_join(Health_expe_GDP2,unicef_metadata)
Joining with `by = join_by(country, year)`
> data_join <- full_join(Health_expe_GDP2, unicef_indicator_2)
Joining with `by = join_by(CodeC, country, `Country Code`, year, Region, `Sub-region`)`
> data_join <- full_join(unicef_indicator_2, unicef_metadata)
Joining with `by = join_by(country, year)`
> data_join <- full_join (unicef_metadata, unicef_indicator_2)
Joining with `by = join_by(country, year)`
> map_world <- map_data("world")
> avg_lifeExp <- data_join %>%
  +   dplyr::group_by(country) %>%
  +   dplyr::summarise(avg_lifeExp = mean(obs_value, na.rm = TRUE))
> # Map 1
  > map_data_join_avg <- dplyr::full_join(avg_lifeExp, map_world, by = c("country" = "region"))
> ggplot2::ggplot(map_data_join_avg, aes(x = long, y = lat, group = group, fill = avg_lifeExp)) +
  +   ggplot2::geom_polygon() +
  +   ggplot2::scale_fill_gradient(low = "red", high = "green", name = "Average Life Expectancy") +
  +   ggplot2::labs(title = "Average Life Expectancy Across Countries",
                    +                 fill = "Average Life Expectancy",
                    +                 caption = "Source: Your Data Source") +
  +   ggplot2::theme_minimal()
> ggplot2::ggsave("choropleth_life_exp.png", width = 10, height = 6)
> #Map 2 obs Value
  > map_data_join_obs_value <- left_join (map_world, unicef_indicator_2 %>% select(country, obs_value), 
                                          +                                      by = c("region" = "country"), 
                                          +                                      suffix = c("", ".obs_value"),
                                          +                                      copy = TRUE,
                                          +                                      keep = TRUE,
                                          +                                      na_matches = "na",
                                          +                                      relationship = "many-to-many")
> map_data_join_obs_value <- filter(map_data_join_obs_value, !is.na(obs_value))
> ggplot(map_data_join_obs_value, aes(x = long, y = lat, group = group, fill = obs_value)) +
  +   geom_polygon() +
  +   scale_fill_viridis_c(name = "obs_value", na.value = "grey90", guide = "colorbar") +
  +   labs(title = "UNICEF Indicator (obs_value) Across Countries",
           +        fill = "obs_value",
           +        caption = "Source: Your Data Source") +
  +   theme_minimal()
> unicef_indicator_2_filtered <- filter(unicef_indicator_2, !is.na(obs_value))
> continent_order <- unicef_indicator_2_filtered %>%
  +   arrange(Region) %>%
  +   pull(Region) %>%
  +   unique()
> unicef_indicator_2_filtered$Region <- factor(unicef_indicator_2_filtered$Region, levels = continent_order)
> ggplot(unicef_indicator_2_filtered, aes(x = Region, y = obs_value, fill = sex)) +
  +   geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  +   scale_fill_manual(values = c("blue", "pink", "purple"), 
                        +                     breaks = c("Male", "Female", "Total"),
                        +                     labels = c("Male", "Female", "Total")) +
  +   labs(title = "Average UNICEF Indicator (obs_value) by Continent and Sex",
           +        x = "Continent",
           +        y = "Average obs_value",
           +        fill = "Sex",
           +        caption = "Source: Your Data Source") +
  +   theme_minimal()
No summary function supplied, defaulting to `mean_se()`
Warning message:
  In geom_bar(stat = "summary", fun.y = "mean", position = "dodge") :
  Ignoring unknown parameters: `fun.y`
> #Scatter plot       
  > scatter_data <- left_join(avg_values, avg_health_exp, by = "country") %>%
  +   left_join(select(unicef_indicator_2_filtered, country, Region), by = "country")
> continent_colors <- c("Oceania" = "chartreuse", 
                        +                       "Americas" = "yellow", 
                        +                       "Europe" = "blue", 
                        +                       "Asia" = "purple", 
                        +                       "Africa" = "red")
> ggplot(scatter_data, aes(x = avg_health_exp, y = avg_obs_value, color = Region)) +
  +   geom_point(size = 3) +
  +   scale_color_manual(values = continent_colors) +
  +   labs(title = "Average Health_exp vs Average obs_value by Country",
           +        x = "Average Health_exp",
           +        y = "Average obs_value",
           +        color = "Continent",
           +        caption = "Source: Your Data Source") +
  +   theme_minimal()
Warning message:
  Removed 65 rows containing missing values or values outside the scale range
(`geom_point()`). 
> ggsave("scatter_plot_avg_health_exp_vs_obs_value.png", width = 10, height = 6)
Warning message:
  Removed 65 rows containing missing values or values outside the scale range
(`geom_point()`). 
> #Graph Histogram 
  > avg_obs_value_sub_region <- unicef_indicator_2_filtered %>%
  +   group_by(`Sub-region`) %>%
  +   summarise(avg_obs_value = mean(obs_value, na.rm = TRUE)) %>%
  +   arrange(avg_obs_value)
> sub_region_colors <- c("Australia and New Zealand" = "green", 
                         +                        "Caribbean" = "yellow", 
                         +                        "Central America" = "yellow",
                         +                        "Eastern Africa" = "red", 
                         +                        "Eastern Asia" = "purple",
                         +                        "Eastern Europe" = "blue",
                         +                        "Melanesia" = "green",
                         +                        "Micronesia" = "green",
                         +                        "Middle Africa" = "red",
                         +                        "Northern Africa" = "red",
                         +                        "Northern America" = "yellow",
                         +                        "Northern Europe" = "blue",
                         +                        "Polynesia" = "green",
                         +                        "South America" = "yellow",
                         +                        "South-Eastern Asia" = "purple",
                         +                        "Southern Africa" = "red",
                         +                        "Southern Asia" = "purple",
                         +                        "Southern Europe" = "blue",
                         +                        "Western Africa" = "red",
                         +                        "Western Asia" = "purple",
                         +                        "Western Europe" = "blue",
                         +                        "Latin America and the Caribbean" = "yellow",
                         +                        "South-eastern Asia" = "purple",
                         +                        "Central Asia"= "purple",
                         +                        "Sub-Saharan Africa" = "red")
> ggplot(avg_obs_value_sub_region, aes(x = avg_obs_value, y = reorder(`Sub-region`, avg_obs_value), fill = `Sub-region`)) +
  +   geom_bar(stat = "identity") +
  +   scale_fill_manual(values = sub_region_colors, 
                        +                     breaks = names(sub_region_colors)) +
  +   labs(title = "Average obs_value by Sub-region",
           +        x = "Average obs_value",
           +        y = "Sub-region",
           +        fill = "Sub-region",
           +        caption = "Source: Your Data Source") +
  +   theme_minimal() +
  +   theme(axis.text.y = element_text(hjust = 1))
> 
  > current_directory <- getwd()
> print(paste("Current Working Directory:", current_directory))
[1] "Current Working Directory: /cloud/project"
> files_in_directory <- list.files()
> print(files_in_directory)
[1] "bar_chart_obs_value_by_continent_and_sex.png"
[2] "choropleth_life_exp.png"                     
[3] "project.Rproj"                               
[4] "Quarto obs"                                  
[5] "scatter_plot_avg_health_exp_vs_obs_value.png"