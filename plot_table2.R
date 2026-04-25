# install.packages(c("tidyverse"))  # run once if needed
library(tidyverse)

# --- table (typed exactly from your message) ---
df <- tribble(
  ~pi_E, ~Association,
  ~CR_NE_N, ~CR_p1, ~CR_p, ~CR_AEr,
  ~SD_NE_N, ~SD_p1, ~SD_p, ~SD_AEr,
  ~S1_NE_N, ~S1_p1, ~S1_p, ~S1_AEr,
  ~S5_NE_N, ~S5_p1, ~S5_p, ~S5_AEr,
  
  0.5, "VeryWeak",   0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,
  0.5, "Weak",       0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,
  0.5, "Moderate",   0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,  0.50, 0.21, 0.80, 1.28,  0.50, 0.22, 0.80, 1.28,
  0.5, "Strong",     0.50, 0.21, 0.80, 1.27,  0.50, 0.20, 0.78, 1.27,  0.50, 0.21, 0.79, 1.27,  0.50, 0.21, 0.79, 1.27,
  0.5, "VeryStrong", 0.50, 0.21, 0.80, 1.27,  0.50, 0.20, 0.79, 1.27,  0.50, 0.21, 0.79, 1.27,  0.50, 0.21, 0.79, 1.27,
  
  0.6, "VeryWeak",   0.50, 0.21, 0.80, 1.23,  0.59, 0.22, 0.79, 1.22,  0.52, 0.22, 0.81, 1.23,  0.54, 0.22, 0.80, 1.23,
  0.6, "Weak",       0.50, 0.23, 0.80, 1.23,  0.59, 0.22, 0.80, 1.22,  0.53, 0.22, 0.81, 1.23,  0.54, 0.22, 0.82, 1.23,
  0.6, "Moderate",   0.50, 0.23, 0.83, 1.23,  0.59, 0.24, 0.82, 1.22,  0.53, 0.25, 0.84, 1.23,  0.54, 0.25, 0.84, 1.23,
  0.6, "Strong",     0.50, 0.28, 0.87, 1.23,  0.60, 0.28, 0.86, 1.22,  0.53, 0.29, 0.87, 1.23,  0.55, 0.29, 0.87, 1.22,
  0.6, "VeryStrong", 0.50, 0.33, 0.91, 1.23,  0.60, 0.31, 0.90, 1.22,  0.53, 0.33, 0.91, 1.22,  0.55, 0.32, 0.91, 1.22,
  
  0.7, "VeryWeak",   0.50, 0.22, 0.81, 1.16,  0.69, 0.19, 0.75, 1.12,  0.55, 0.22, 0.80, 1.15,  0.58, 0.21, 0.81, 1.14,
  0.7, "Weak",       0.50, 0.25, 0.84, 1.16,  0.69, 0.21, 0.78, 1.12,  0.55, 0.24, 0.84, 1.15,  0.59, 0.24, 0.84, 1.14,
  0.7, "Moderate",   0.50, 0.27, 0.87, 1.16,  0.69, 0.24, 0.81, 1.11,  0.56, 0.27, 0.87, 1.15,  0.59, 0.27, 0.87, 1.14,
  0.7, "Strong",     0.50, 0.41, 0.95, 1.15,  0.69, 0.35, 0.92, 1.11,  0.58, 0.41, 0.95, 1.14,  0.62, 0.39, 0.95, 1.13,
  0.7, "VeryStrong", 0.50, 0.55, 0.98, 1.15,  0.69, 0.49, 0.96, 1.11,  0.60, 0.54, 0.98, 1.13,  0.64, 0.53, 0.99, 1.12,
  
  0.8, "VeryWeak",   0.50, 0.23, 0.81, 1.05,  0.78, 0.16, 0.66, 0.92,  0.57, 0.22, 0.81, 1.01,  0.62, 0.20, 0.81, 0.99,
  0.8, "Weak",       0.50, 0.28, 0.87, 1.05,  0.78, 0.19, 0.73, 0.92,  0.59, 0.27, 0.87, 1.01,  0.64, 0.25, 0.86, 0.98,
  0.8, "Moderate",   0.50, 0.34, 0.92, 1.04,  0.78, 0.24, 0.79, 0.91,  0.60, 0.33, 0.92, 1.00,  0.66, 0.31, 0.91, 0.97,
  0.8, "Strong",     0.50, 0.62, 0.99, 1.03,  0.79, 0.46, 0.95, 0.90,  0.66, 0.59, 0.99, 0.96,  0.71, 0.56, 0.99, 0.94,
  0.8, "VeryStrong", 0.50, 0.82, 1.00, 1.03,  0.79, 0.66, 0.99, 0.89,  0.70, 0.80, 1.00, 0.93,  0.74, 0.76, 1.00, 0.91
)

# --- use ONLY AEr columns; reshape to long ---
aer_long <- df %>%
  select(pi_E, Association, CR_p, SD_p, S1_p, S5_p) %>% #CR_AEr, SD_AEr, S1_AEr, S5_AEr
  pivot_longer(
    cols = c(CR_p, SD_p, S1_p, S5_p),
    names_to = "Method",
    values_to = "Power"#AEr
  ) %>%
  mutate(
    Method = recode(Method,
                    CR_p = "Complete randomization",
                    SD_p = "Safety-driven RAR",
                    S1_p = "SAFER (η=1)",
                    S5_p = "SAFER (η=5)"
    ),
    Method = factor(Method, levels = c("Complete randomization","Safety-driven RAR","SAFER (η=1)","SAFER (η=5)")),
    pi_E = factor(pi_E, levels = c(0.5, 0.6, 0.7, 0.8),
                  labels = c("π_E=0.5","π_E=0.6","π_E=0.7","π_E=0.8"))
  )



  
# --- plot: grouped bars by target allocation, faceted by association (like your attached figure style) ---
aer_long %>% filter(Association == "VeryWeak")  %>% ggplot(aes(x = pi_E, y = Power, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = sprintf("%.2f", Power)),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3
  ) +
  facet_wrap(~ Association, nrow = 1) +
  labs(
    x = "Target Allocation",
    y = "Power", #AE Rate / Patient-Year
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

############################################################


library(tidyverse)

# If pi_E is a factor like "π_E=0.5", extract the number safely
df_vs <- aer_long %>%
  filter(Association == "VeryStrong") %>%
  mutate(
    pi_num = readr::parse_number(as.character(pi_E)),  # works for "0.5" or "π_E=0.5"
    Method = factor(Method,
                    levels = c("Complete randomization",
                               "Safety-driven RAR",
                               "SAFER (η=1)",
                               "SAFER (η=5)"))
  )


# Take darker tones only
blue_dark  <- brewer.pal(7, "Blues")[4:7]
green_dark <- brewer.pal(7, "Greens")[4:7]

ggplot(df_vs, aes(x = pi_num, y = AEr, group = Method, color = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = sprintf("%.2f", AEr)),
            vjust = -0.8, size = 3, show.legend = FALSE) +
  scale_x_continuous(
    breaks = c(0.5, 0.6, 0.7, 0.8),
    labels = function(x) parse(text = paste0("pi[E]==", x))
  ) +  scale_color_manual(
    values = c(
      "Complete randomization" = blue_dark[4],
      "Safety-driven RAR"      = blue_dark[3],
      "SAFER (η=1)"            = green_dark[3],
      "SAFER (η=5)"            = green_dark[4]
    )
  ) +
  #scale_color_brewer(palette = "Blues") +
  labs(
    x = "Target allocation",
    y = "AE rate / patient-year",
    color = NULL,
    title = "Very strong association"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top")


###############################################


library(tidyverse)
library(RColorBrewer)

# Prepare data
df_plot <- aer_long %>%
  filter(Association %in% c("VeryWeak", "VeryStrong")) %>%
  mutate(Association = factor(Association,
                              levels = c("VeryWeak", "VeryStrong")), 
    pi_num = readr::parse_number(as.character(pi_E)),
    Method = factor(Method,
                    levels = c("Complete randomization",
                               "Safety-driven RAR",
                               "SAFER (η=1)",
                               "SAFER (η=5)"))
  )

# Light tones (top of Brewer scales)
blue_light  <- c("#9ecae1", "#6baed6", "#4292c6", "#2171b5")
#green_light <- c("#a1d99b", "#74c476", "#31a354", "#006d2c")

# Assign colors by BOTH Association and Method
df_plot <- df_plot %>%
  group_by(Association) %>%
  mutate(color = if (unique(Association) == "VeryStrong")
    blue_light[as.numeric(Method)]
    else
      blue_light[as.numeric(Method)]) %>%
  ungroup()

ggplot(df_plot,
       aes(x = pi_num,
           y = AEr,
           group = interaction(Method, Association),
           color = color)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  geom_text(aes(label = sprintf("%.2f", AEr)),
            vjust = -0.8, size = 4, show.legend = FALSE) +
  facet_wrap(~Association, nrow = 1) +
  scale_color_identity(
    guide = "legend",
    labels = levels(df_plot$Method),
    breaks = blue_light,  # ensures legend appears
    name = "Design"
  ) +
  scale_x_continuous(
    breaks = c(0.5, 0.6, 0.7, 0.8),
    labels = function(x) parse(text = paste0("pi[E]==", x))
  ) +
  labs(
    x = expression(pi[E]),
    y = "AE rate / patient-year",
    title = "AE rate under Very Weak and Very Strong association"
  ) +
  theme_dark(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
