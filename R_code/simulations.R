library(tidyverse)
library(palmerpenguins)

# visualization of the definition
df_test <- data.frame(x = c(2, 3, 4, 5, 6, 10, 11, 12, 13, 14),
                 y = c(7, 6, 5, 4, 3, 15, 14, 13, 12, 11),
                 group = rep(c("A", "B"), each = 5))

df_test |>
  ggplot(aes(x = x, y = y, color = group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", aes(group = 1),
              color = "black", linetype = "dashed", se = FALSE) +
  labs(title = "Visualization of Simpson's paradox",
       x = "X values",
       y = "Y values") +
  expand_limits(x = 1, y = c(2, 16))  +
  theme_minimal()



# (i) example with two numerical variables
df_1 = palmerpenguins::penguins |> na.omit()

df_1 |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = 1), 
              color = "#9932CC", linetype = "dashed", se = FALSE) +  
  labs(x = "Length (mm)", y = "Depth (mm)", title = "Penguins' Bill Depth / Bill Length") +
  theme_minimal()

df_1 |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = species)) +  
  geom_smooth(method = "lm", aes(group = 1), 
              color = "#9932CC", linetype = "dashed", se = FALSE) +  
  labs(x = "Length (mm)", y = "Depth (mm)", title = "Penguins' Bill Depth / Bill Length over different species") +
  theme_minimal()



# (ii) example with two categorical variables
df_2 = data.frame(
  Treatment = rep(c("Treatment A", "Treatment B"), each = 3),
  Stone_Size = c("Small stones", "Large stones", "Both"),
  Success_Count = c(81, 192, 273, 234, 55, 289),
  Total_Count = c(87, 263, 350, 270, 80, 350)
)

df_2 <- df_2 |> mutate(Success_Rate = Success_Count / Total_Count)

df_2 |>
  filter(Stone_Size == "Both") |>
  ggplot(aes(x = Treatment, y = Success_Rate, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.12) +
  labs(title = "Success Rate by Treatment",
       x = "Treatment Type",
       y = "Success Rate") +
  scale_fill_manual(values = c("Treatment A" = "black",
                               "Treatment B" = "red")) +
  theme_minimal()

df_2 |>
  filter(Stone_Size != "Both") |>
  ggplot(aes(x = Stone_Size, y = Success_Rate, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.12) +
  labs(title = "Success Rate by Stone Size and Treatment",
       x = "Stone Size",
       y = "Success Rate") +
  scale_fill_manual(values = c("Treatment A" = "black",
                               "Treatment B" = "red")) +
  theme_minimal()

df_2 |>
  ggplot(aes(x = Stone_Size, y = Success_Rate, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.12) +
  labs(title = "Comparison",
       x = "Stone Size",
       y = "Success Rate") +
  scale_fill_manual(values = c("Treatment A" = "black",
                               "Treatment B" = "red")) +
  theme_minimal()



