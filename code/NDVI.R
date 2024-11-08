library(readxl)
library(tidyverse)

NDVI_season <- read_xlsx("./Data/ndvi_seasonal.xlsx")

str(NDVI_season)


ggplot(NDVI_season, aes(x=DOY, y=as.numeric(NDVI), color=Site))+
  geom_point()+
  geom_smooth()

ggplot(NDVI_season, aes(x=as.Date(Date), y=relative, color=Site))+
  geom_point()+
  geom_smooth(se=F)+
  facet_grid(cols=vars(Plot))

ggplot(NDVI_season, aes(x=as.Date(Date), y=relative, color=Site))+
  geom_point()+
  geom_smooth(method="lm")

# Calculate the mean vegetation height for each Site, Plot, and Date
veg_height_mean <- NDVI_season %>%
  group_by(Site, Plot, Date) %>%    # Group by Site, Plot, and Date
  summarise(Mean_Height = mean(relative, na.rm = TRUE))  # Calculate the mean height

# Display the first few rows of the summarized data
head(veg_height_mean)

# Plot the data
ggplot(subset(veg_height_mean, Date>"2024-07-16 UTC"), aes(x = as.Date(Date), y = Mean_Height, color=Site)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(title = "Mean Vegetation Height Over Time",
       x = "Date",
       y = "Mean Vegetation Height") +
  theme_minimal()

ggplot(veg_height_mean, aes(x = as.Date(Date), y = Mean_Height, color=Site)) +
  geom_point() +
  geom_smooth(se=F)+
  labs(title = "Mean Vegetation Height Over Time",
       x = "Date",
       y = "Mean Vegetation Height") +
  theme_minimal()


ggplot(subset(veg_height_mean, Date>"2024-07-16 UTC"), aes(x = as.Date(Date), y = Mean_Height, color=Site)) +
  geom_point() +
  geom_smooth()+
  labs(title = "Mean Vegetation Height Over Time",
       x = "Date",
       y = "Mean Vegetation Height") +
  theme_minimal()

# Perform a linear regression on the entire dataset
model <- lm(Mean_Height ~ as.Date(Date), data = veg_height_mean)

# Summary of the linear model
summary(model)

# Late period
late_model <- lm(Mean_Height ~ as.Date(Date) + Site, data = subset(veg_height_mean, Date > "2024-07-16"))
summary(late_model)

# Perform linear regression for each Site
site_trends <- subset(NDVI_season, Date > "2024-07-30") %>%
  group_by(Site) %>%
  do(model = lm(relative ~ as.Date(Date), data = .))

# Extract the slope and p-value for each site
site_trends_summary <- site_trends %>%
  summarise(Site = Site,
            Slope = coef(model)[2],
            p_value = summary(model)$coefficients[2, 4])

# Display the results
print(site_trends_summary)
