install.packages("tidyverse")
library(tidyverse)


Reading files 
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")


Joining data 
combined_data <- unicef_indicator_1 %>%
  full_join(unicef_indicator_2, by = c("country", "time_period")) %>%
  full_join(unicef_metadata, by = c("country", "time_period"))


Filter -> 1971
data_1971 <- combined_data %>%
  filter(time_period == 1971)

Check time period is numeric... yes (dbl)
str(combined_data)


Prepare map data 
world_map <- map_data("world")


Join data with map data
map_data_1971 <- world_map %>%
  left_join(data_1971, by = c("region" = "country"))

Plot map 
ggplot(map_data_1971) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  scale_fill_viridis_c(option = "C", name = "Life Expectancy") +
  labs(title = "Life Expectancy by Country in 1971") +
  theme_minimal() +
  theme(legend.position = "bottom")

Change title, font etc.
ggplot(map_data_1971) +
  ...
theme(text = element_text(family = "Times New Roman"))

ggplot(map_data_1971) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  labs(title = "Life Expectancy by Country, 1971") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_fill_viridis_c(name = "Life Expectancy (years)")

Filter for year - 2021 
data_2021 <- combined_data %>%
  filter(time_period == 2021)

Prepare map data
world_map <- map_data("world")

Join data with map data
map_data_2021 <- world_map %>%
  left_join(data_2021, by = c("region" = "country"))

Plot map
ggplot(map_data_2021) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  scale_fill_viridis_c(option = "C", name = "Life Expectancy") +
  labs(title = "Life Expectancy by Country, 2021") +
  theme_minimal() +
  theme(legend.position = "bottom")

Change font, heading etc
ggplot(map_data_2021) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  labs(title = "Life Expectancy by Country, 2021") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_fill_viridis_c(name = "Life Expectancy")


BAR CHART
filter the data for the year 2022 and the countries of interest
library(tidyverse)
data_2022 <- combined_data %>%
  filter(time_period == 2022, country %in% c("Botswana", "Burkina Faso", "Cameroon", "Ethiopia",
                                             "Ghana", "Kenya", "Mozambique", "Niger", "Senegal",
                                             "Tanzania, United Republic of", "Uganda", "Zambia", "Zimbabwe"))

CREATE BAR CHART
ggplot(data_2022, aes(x = country, y = `Per cent of pregnant women living with HIV receiving lifelong ART`, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Pregnant Women with HIV Receiving ART in 2022",
       x = "Country",
       y = "% Women with HIV Receiving ART") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

MAKE IT LOOK NICER
ggplot(data_2022, aes(x = reorder(country, `Per cent of pregnant women living with HIV receiving lifelong ART`), 
                      y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  geom_text(aes(label = round(`Per cent of pregnant women living with HIV receiving lifelong ART`, 1)), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Percentage of Pregnant Women with HIV Receiving ART in 2022",
       x = "Country",
       y = "% Women with HIV Receiving ART") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



Chnage MAP 
ggplot(map_data_2021) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  scale_fill_viridis_c(name = "Life Expectancy", option = "C") +
  labs(title = "Life Expectancy by Country in 2021") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 10, family = "sans"),
        legend.text = element_text(size = 8, family = "sans"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, family = "sans"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

# final map data - 2021
ggplot(map_data_2021) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  scale_fill_gradientn(colours = c("pink", "deepskyblue"), name = "Life Expectancy") +
  labs(title = "Life Expectancy by Country, 2021") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, family = "sans"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  scale_fill_gradient(low = "pink", high = "deepskyblue4", name = "Life Expectancy") + # Adjust the 'low' and 'high' to your preference
  labs(title = "Life Expectancy by Country in 2021") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, family = "sans"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

#final map data - 1971
ggplot(map_data_1971) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  labs(title = "Life Expectancy by Country in 1971") +
  theme_minimal() +
  theme(plot.title = element_text(family = "sans", face = "bold", size = 14, hjust = 0.5),
        legend.title = element_text(family = "sans", face = "bold", size = 10),
        legend.text = element_text(family = "sans", size = 8),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank(),   # Hide axis text
        axis.title = element_blank(),  # Hide axis titles
        axis.ticks = element_blank()) + # Hide axis ticks
  scale_fill_gradientn(colors = c("#0000FF", "#FFC0CB"), name = "Life Expectancy", limits = c(30, 80))


#final bar chart - 2022
ggplot(data_2022, aes(x = reorder(country, `Per cent of pregnant women living with HIV receiving lifelong ART`), 
                      y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_bar(stat = "identity", fill = "#FFC0CB") +  # This is a common light pink color
  geom_text(aes(label = round(`Per cent of pregnant women living with HIV receiving lifelong ART`, 1)), 
            position = position_stack(vjust = 0.5), color = "black", size = 3.5) +  # Changed text color to black for contrast
  labs(title = "Percentage of Pregnant Women with HIV Receiving ART in 2022",
       x = "Country",
       y = "% Women with HIV Receiving ART") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


Load Package
library(dplyr)
library(ggplot2)


SCATTERPLOT
Filter data
car_data <- combined_data %>%
  filter(country == "Central African Republic", time_period >= 2010, time_period <= 2022)

Create time series plot
ggplot(car_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line() + # Use geom_line for a line chart
  geom_point() + # Use geom_point to show individual data points
  labs(title = "HIV ART Coverage Over Time in Central African Republic",
       x = "Year",
       y = "Percentage of Pregnant Women with HIV Receiving ART") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Adjust text angle for x-axis labels if needed
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))

ggplot(car_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line() +
  geom_point() +
  labs(
    title = "HIV ART Coverage Over Time in Central African Republic",
    x = "Year",
    y = "Percentage of Pregnant Women Receiving ART"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), # Rotate x-axis labels for better readability
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


#Final time series - CAR
ggplot(car_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line(color = "#FFC0CB") +  # This is the hex code for a light pink color
  geom_point(color = "#FFC0CB") +
  labs(
    title = "HIV ART Coverage Over Time in Central African Republic",
    x = "Year",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

library(scales)
ggplot(car_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line(color = "pink") +  # Change to the specific pink you want
  geom_point(color = "pink") +
  labs(
    title = "HIV ART Coverage Over Time in Central African Republic",
    x = "Year",
    y = "%"  # Changed to just display '%'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  scale_y_continuous(labels = percent_format())ggplot(car_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line(color = "pink") +  # Change to the specific pink you want
  geom_point(color = "pink") +
  labs(
    title = "HIV ART Coverage Over Time in Central African Republic",
    x = "Year",
    y = "%"  # Changed to just display '%'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  scale_y_continuous(labels = percent_format())

TIME SERIES - CONGO
CDR_data <- combined_data %>%
  filter(country == "Congo, the Democratic Republic of the", time_period >= 2010, time_period <= 2022)

ggplot(CDR_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line() + # Use geom_line for a line chart
  geom_point() + # Use geom_point to show individual data points
  labs(title = "HIV ART Coverage Over Time in Central African Republic",
       x = "Year",
       y = "%") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Adjust text angle for x-axis labels if needed
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))

ggplot(CDR_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line() +
  geom_point() +
  labs(
    title = "HIV ART Coverage Over Time in Deomocratic Republic of the Congo",
    x = "Year",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), # Rotate x-axis labels for better readability
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


ggplot(CDR_data, aes(x = time_period, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_line(color = "pink") +  # Add color argument here for the line
  geom_point(color = "pink") +  # And also here for the points if you want them in pink
  labs(
    title = "HIV ART Coverage Over Time in the Democratic Republic of the Congo",
    x = "Year",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), # Rotate x-axis labels for better readability
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


Scatter Plot 
data_2021 <- combined_data %>%
  filter(time_period == 2021, country %in% c("Botswana", "Burkina Faso", "Cameroon", "Ethiopia",
                                             "Ghana", "Kenya", "Mozambique", "Niger", "Senegal",
                                             "Tanzania, United Republic of", "Uganda", "Zambia", "Zimbabwe"))

Create scatter plot
ggplot(data_2021, aes(x = `Life expectancy at birth, total (years)`, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_point() +
  geom_text(aes(label = country), hjust = 1.5, vjust = 1.5) +  # Add country labels
  labs(
    title = "Life Expectancy vs % of Women with HIV Receiving ART, 2021",
    x = "Life Expectancy",
    y = "% of Women with HIV Receiving ART"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12)
  )

Make it look nicer 
install.packages("ggrepel")
library(ggrepel)

ggplot(data_2021, aes(x = `Life expectancy at birth, total (years)`, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_point(color = 'blue', size = 3, alpha = 0.6) +
  geom_text_repel(
    aes(label = country), 
    box.padding = 0.35, 
    point.padding = 0.5,
    size = 3
  ) +
  labs(
    title = "Life Expectancy vs % of Women with HIV Receiving ART, 2021",
    x = "Life Expectancy (years)",
    y = "%"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  ) +
  scale_color_gradient(low = "skyblue", high = "navy") + 
  theme_light(base_size = 14) +
  theme(axis.text = element_text(color = "grey20"),
        axis.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"),
        legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(color = "white")
  )


font_size <- 12
font_family <- "sans"
label_size <- 3


ggplot(data_2021, aes(x = `Life expectancy at birth, total (years)`, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_point(color = 'pink', size = 3) +  # Set the point color to pink
  geom_text_repel(
    aes(label = country),
    box.padding = 0.35,
    point.padding = 0.5,
    size = label_size  # Use the new, smaller label size
  ) +
  labs(
    title = "Life Expectancy vs % of Women with HIV Receiving ART, 2021",
    x = "Life Expectancy",
    y = "% of Women with HIV Receiving ART"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size, family = font_family),  # Apply the font size and family globally
    plot.title = element_text(face = "bold", size = font_size),
    axis.title = element_text(face = "bold", size = font_size),
    legend.position = "none"  # Hide the legend if it's not necessary
  )


ggplot(data_2021, aes(x = `Life expectancy at birth, total (years)`, y = `Per cent of pregnant women living with HIV receiving lifelong ART`)) +
  geom_point(color = 'pink', size = 3) +  # Change point color to pink and adjust size as needed
  geom_text_repel(
    aes(label = country),
    box.padding = 0.35,
    point.padding = 0.5,
    size = label_size  # Use the smaller size for labels
  ) +
  labs(
    title = "Life Expectancy vs % of Women with HIV Receiving ART, 2021",
    x = "Life Expectancy",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = font_size, family = font_family),
    plot.title = element_text(face = "bold", hjust = 0.5, size = font_size),  # Center the title
    axis.title = element_text(face = "bold", size = font_size),
    legend.position = "none",  # Hide the legend if it's not necessary
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margins if needed
  )
