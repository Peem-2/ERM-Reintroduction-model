setwd ("C:/Users/Peem/Desktop/EnviTech/Critial_Analytic_ERM/Model_Reintroduction_coding/results/R_Excel")
library(readxl)
library(ggplot2)
library(dplyr)

#read data
baseline_deer <- read_excel("deer_table_b.xlsx")
wolf_deer <- read_excel("deer_table_w.xlsx")
lynx_deer <- read_excel("deer_table_l.xlsx")
baseline_veg <- read_excel("veg_table_b.xlsx")
wolf_veg <- read_excel("veg_table_w.xlsx")
lynx_veg <- read_excel("veg_table_l.xlsx")
baseline_econ <- read_excel("econ_table_b.xlsx")
wolf_econ <- read_excel("econ_table_w.xlsx")
lynx_econ <- read_excel("econ_table_l.xlsx")
baseline_overall_econ <- read_excel("overall_econ_table_b.xlsx")
wolf_overall_econ <- read_excel("overall_econ_table_w.xlsx")
lynx_overall_econ <- read_excel("overall_econ_table_l.xlsx")
baseline_econ_sum <- read_excel("overall_econ_sum_table_b.xlsx")
wolf_econ_sum <- read_excel("overall_econ_sum_table_w.xlsx")
lynx_econ_sum <- read_excel("overall_econ_sum_table_l.xlsx")

# combine deer density
baseline_deer <- baseline_deer %>%
  mutate(combined_density = Red_deer_density +Roe_deer_density)
wolf_deer <- wolf_deer %>%
  mutate(combined_density = Red_deer_density +Roe_deer_density)
lynx_deer <- lynx_deer %>%
  mutate(combined_density = Red_deer_density +Roe_deer_density)

# compare forest gain
# Initialize the woodland_change column
baseline_veg$woodland_change <- NA
wolf_veg$woodland_change <- NA
lynx_veg$woodland_change <- NA

# Get the number of rows in the dataset
num_rows <- nrow(baseline_deer)

# Create a for loop that iterates over each row
for (i in 1:num_rows) {
  if (i == 1) {
    baseline_veg$woodland_change[i] <- 0
    wolf_veg$woodland_change[i] <- 0
    lynx_veg$woodland_change[i] <- 0
  } else {
    baseline_veg$woodland_change[i] <- baseline_veg$Woodland_Coverage[i] - baseline_veg$Woodland_Coverage[i-1]
    wolf_veg$woodland_change[i] <- wolf_veg$Woodland_Coverage[i] - wolf_veg$Woodland_Coverage[i-1]
    lynx_veg$woodland_change[i] <- lynx_veg$Woodland_Coverage[i] - lynx_veg$Woodland_Coverage[i-1]
  }
}


# Plot the data
red_deer_plot <- ggplot() +
  geom_line(data = baseline_deer, aes(x = Year, y = Red_deer_population, color = 'Baseline')) +
  geom_point(data = baseline_deer, aes(x = Year, y = Red_deer_population, color = 'Baseline', shape = 'Baseline')) +
  geom_line(data = wolf_deer, aes(x = Year, y = Red_deer_population, color = 'Wolf reintroduction')) +
  geom_point(data = wolf_deer, aes(x = Year, y = Red_deer_population, color = 'Wolf reintroduction', shape = 'Wolf reintroduction')) +
  geom_line(data = lynx_deer, aes(x = Year, y = Red_deer_population, color = 'Lynx reintroduction')) +
  geom_point(data = lynx_deer, aes(x = Year, y = Red_deer_population, color = 'Lynx reintroduction', shape = 'Lynx reintroduction')) +
  scale_color_manual(values = c('Baseline' = 'red', 'Wolf reintroduction' = 'blue', 'Lynx reintroduction' = 'green')) +
  scale_shape_manual(values = c('Baseline' = 16, 'Wolf reintroduction' = 17, 'Lynx reintroduction' = 18)) +  # Assign different shapes
  theme_bw() +
  ggtitle("Red deer population in each scenario") +
  xlab("Year") +
  ylab("Red deer population") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Scenarios", shape = "Scenarios") +  # Change the legend title here
  scale_y_continuous(breaks = seq(0, 16000, by = 2000), limits = c(0, 16000))  # Extend y-axis to 16000
red_deer_plot

roe_deer_plot <- ggplot() +
  geom_line(data = baseline_deer, aes(x = Year, y = Roe_deer_population, color = 'Baseline')) +
  geom_point(data = baseline_deer, aes(x = Year, y = Roe_deer_population, color = 'Baseline', shape = 'Baseline')) +
  geom_line(data = wolf_deer, aes(x = Year, y = Roe_deer_population, color = 'Wolf reintroduction')) +
  geom_point(data = wolf_deer, aes(x = Year, y = Roe_deer_population, color = 'Wolf reintroduction', shape = 'Wolf reintroduction')) +
  geom_line(data = lynx_deer, aes(x = Year, y = Roe_deer_population, color = 'Lynx reintroduction')) +
  geom_point(data = lynx_deer, aes(x = Year, y = Roe_deer_population, color = 'Lynx reintroduction', shape = 'Lynx reintroduction')) +
  scale_color_manual(values = c('Baseline' = 'red', 'Wolf reintroduction' = 'blue', 'Lynx reintroduction' = 'green')) +
  scale_shape_manual(values = c('Baseline' = 16, 'Wolf reintroduction' = 17, 'Lynx reintroduction' = 18)) +  # Assign different shapes
  theme_bw() +
  ggtitle("Roe deer population in each scenario") +
  xlab("Year") +
  ylab("Roe deer population") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Scenarios", shape = "Scenarios") +  # Change the legend title here
  scale_y_continuous(breaks = seq(0, 22000, by = 2000), limits = c(0, 22000))  # Extend y-axis to 22000
roe_deer_plot

deer_den_plot <- ggplot() +
  geom_line(data = baseline_deer, aes(x = Year, y = combined_density, color = 'Baseline')) +
  geom_point(data = baseline_deer, aes(x = Year, y = combined_density, color = 'Baseline', shape = 'Baseline')) +
  geom_line(data = wolf_deer, aes(x = Year, y = combined_density, color = 'Wolf reintroduction')) +
  geom_point(data = wolf_deer, aes(x = Year, y = combined_density, color = 'Wolf reintroduction', shape = 'Wolf reintroduction')) +
  geom_line(data = lynx_deer, aes(x = Year, y = combined_density, color = 'Lynx reintroduction')) +
  geom_point(data = lynx_deer, aes(x = Year, y = combined_density, color = 'Lynx reintroduction', shape = 'Lynx reintroduction')) +
  scale_color_manual(values = c('Baseline' = 'red', 'Wolf reintroduction' = 'blue', 'Lynx reintroduction' = 'green')) +
  scale_shape_manual(values = c('Baseline' = 16, 'Wolf reintroduction' = 17, 'Lynx reintroduction' = 18)) +  # Assign different shapes
  theme_bw() +
  ggtitle("Deer density per km2 in each scenario") +
  xlab("Year") +
  ylab("Deer density per km2") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Scenarios", shape = "Scenarios") +  # Change the legend title here
  scale_y_continuous(breaks = seq(0, 60, by = 5), limits = c(0, 60))  # Extend y-axis to 22000
deer_den_plot


wolf_red_plot <- ggplot() +
  geom_line(data = wolf_deer, aes(x = Year, y = Red_deer_population, color = 'Red Deer')) +
  geom_point(data = wolf_deer, aes(x = Year, y = Red_deer_population, color = 'Red Deer')) +
  geom_line(data = wolf_deer, aes(x = Year, y = Wolf_population * 550, color = 'Wolf')) +  # Scale wolf population for visibility
  geom_point(data = wolf_deer, aes(x = Year, y = Wolf_population * 550, color = 'Wolf')) +  # Scale wolf population for visibility
  scale_y_continuous(
    name = "Red Deer Population",
    sec.axis = sec_axis(~./550, name = "Wolf Population")  # Create secondary y-axis
  ) +
  scale_color_manual(values = c('Red Deer' = 'red', 'Wolf' = 'blue')) +
  theme_bw() +
  ggtitle("Population of Red Deer and Wolves") +
  xlab("Year") +
  ylab("Red Deer Population") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Species")
wolf_red_plot

lynx_roe_plot <- ggplot() +
  geom_line(data = lynx_deer, aes(x = Year, y = Roe_deer_population, color = 'Roe Deer')) +
  geom_point(data = lynx_deer, aes(x = Year, y = Roe_deer_population, color = 'Roe Deer')) +
  geom_line(data = lynx_deer, aes(x = Year, y = Lynx_population * 550, color = 'WLynx')) +  # Scale wolf population for visibility
  geom_point(data = lynx_deer, aes(x = Year, y = Lynx_population * 550, color = 'Lynx')) +  # Scale wolf population for visibility
  scale_y_continuous(
    name = "Red Deer Population",
    sec.axis = sec_axis(~./550, name = "Lynx Population")  # Create secondary y-axis
  ) +
  scale_color_manual(values = c('Roe Deer' = 'red', 'Lynx' = 'blue')) +
  theme_bw() +
  ggtitle("Population of Roe Deer and Lynxes") +
  xlab("Year") +
  ylab("Red Deer Population") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Species")
lynx_roe_plot

woodland_plot <- ggplot() +
  geom_line(data = baseline_veg, aes(x = Year, y = Woodland_Coverage, color = 'Baseline')) +
  geom_point(data = baseline_veg, aes(x = Year, y = Woodland_Coverage, color = 'Baseline', shape = 'Baseline')) +
  geom_line(data = wolf_veg, aes(x = Year, y = Woodland_Coverage, color = 'Wolf reintroduction')) +
  geom_point(data = wolf_veg, aes(x = Year, y = Woodland_Coverage, color = 'Wolf reintroduction', shape = 'Wolf reintroduction')) +
  geom_line(data = lynx_veg, aes(x = Year, y = Woodland_Coverage, color = 'Lynx reintroduction')) +
  geom_point(data = lynx_veg, aes(x = Year, y = Woodland_Coverage, color = 'Lynx reintroduction', shape = 'Lynx reintroduction')) +
  scale_color_manual(values = c('Baseline' = 'red', 'Wolf reintroduction' = 'blue', 'Lynx reintroduction' = 'green')) +
  scale_shape_manual(values = c('Baseline' = 16, 'Wolf reintroduction' = 17, 'Lynx reintroduction' = 18)) +  # Assign different shapes
  theme_bw() +
  ggtitle("Forest coverage change in each scenario") +
  xlab("Year") +
  ylab("Forest coverage (Ha)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Scenarios", shape = "Scenarios") +  # Change the legend title here
  scale_y_continuous(breaks = seq(3300, 5300, by = 400), limits = c(3300, 5300))  # Extend y-axis to 16000
woodland_plot

econ_plot <- ggplot() +
  geom_line(data = baseline_econ_sum, aes(x = Year, y = Accumulative_gain_loss, color = 'Baseline')) +
  geom_point(data = baseline_econ_sum, aes(x = Year, y = Accumulative_gain_loss, color = 'Baseline', shape = 'Baseline')) +
  geom_line(data = wolf_econ_sum, aes(x = Year, y = Accumulative_gain_loss, color = 'Wolf reintroduction')) +
  geom_point(data = wolf_econ_sum, aes(x = Year, y = Accumulative_gain_loss, color = 'Wolf reintroduction', shape = 'Wolf reintroduction')) +
  geom_line(data = lynx_econ_sum, aes(x = Year, y = Accumulative_gain_loss, color = 'Lynx reintroduction')) +
  geom_point(data = lynx_econ_sum, aes(x = Year, y = Accumulative_gain_loss, color = 'Lynx reintroduction', shape = 'Lynx reintroduction')) +
  scale_color_manual(values = c('Baseline' = 'red', 'Wolf reintroduction' = 'blue', 'Lynx reintroduction' = 'green')) +
  scale_shape_manual(values = c('Baseline' = 16, 'Wolf reintroduction' = 17, 'Lynx reintroduction' = 18)) +  # Assign different shapes
  theme_bw() +
  ggtitle("Accumulative gain/loss from deers (m) in each scenarios") +
  xlab("Year") +
  ylab("Accumulative gain/loss from deers (m)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Make the title bold
  ) +
  labs(color = "Scenarios", shape = "Scenarios") +  # Change the legend title here
  scale_y_continuous(breaks = seq(-75, 0, by = 15), limits = c(-75, 0))  # Extend y-axis to 16000
econ_plot

lynx_wood_deer_plot <- ggplot() +
  geom_bar(data = lynx_veg, aes(x = Year, y = woodland_change*5), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(data = lynx_deer, aes(x = Year, y = combined_density), color = "red", size = 1) +  # Scale down line data
  geom_point(data = lynx_deer, aes(x = Year, y = combined_density), color = "red", size = 2) +  # Scale down line data
  scale_y_continuous(
    name = "Deer density per Km2",
    sec.axis = sec_axis(~./5, name = "Woodland coverage change")  # Create secondary y-axis
  ) +
  theme_bw() +
  ggtitle("Deer density and forest coverage change") +
  xlab("Year") +
  ylab("Deer density per km2") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
lynx_wood_deer_plot

baseline_red_plot <- ggplot() +
  geom_line(data = baseline_deer, aes(x = Year, y = Red_deer_population, color = 'Red Deer population')) +
  geom_point(data = baseline_deer, aes(x = Year, y = Red_deer_population, color = 'Red Deer population')) +
  geom_line(data = baseline_deer, aes(x = Year, y = Red_deer_density * 645.24, color = 'Red Deer density')) +  # Scale wolf population for visibility
  geom_point(data = baseline_deer, aes(x = Year, y = Red_deer_density * 645.24, color = 'Red Deer density')) +  # Scale wolf population for visibility
  scale_y_continuous(
    name = "Red Deer Population",
    sec.axis = sec_axis(~./645.24, name = "Red Deer density per km2")  # Create secondary y-axis
  ) +
  scale_color_manual(values = c('Red Deer population' = 'red', 'Red Deer density' = 'blue')) +
  theme_bw() +
  ggtitle("Population and Density of Red Deer") +
  xlab("Year") +
  ylab("Red Deer Population") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"  # Remove the legend  # Make the title bold
  ) +
  labs(color = "Species")
baseline_red_plot
