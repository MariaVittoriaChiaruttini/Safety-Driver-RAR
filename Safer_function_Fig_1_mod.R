library(ggplot2)
library(dplyr)
library(RColorBrewer)


# Load necessary libraries
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# Define the functions
F_linear <- function(fi, rar) {
  return(0.5 + (rar - 0.5) * fi)
}

# DABC_mod <- function(fi, rar, eta) {
#   ifelse(rar < 0.5, 0.5, ifelse(fi <= 0.5, 0.5, ifelse(fi == 1, rar,
#                                 0.5 + (rar - 0.5) * (1 - (1 - (fi - 0.5) / 0.5)^eta))))
# }


DABC_mod <- function(fi, rar, eta) {
  if (any(eta <= 0, na.rm = TRUE)) stop("eta must be > 0.")
  out <- rep(0.5, length.out = max(length(fi), length(rar), length(eta)))
  
  ok <- rar >= 0.5 & fi > 0.5
  out[ok & fi == 1] <- rar[ok & fi == 1]
  
  mid <- ok & fi < 1
  t <- (fi[mid] - 0.5) / 0.5
  out[mid] <- 0.5 + (rar[mid] - 0.5) * (1 - (1 - t)^eta[mid])
  
  out
}

#DABC_mod(0.9, 0.51, 1)

# Set parameters
fi_vals <- seq(0, 1, length.out = 100)  # fi values from 0 to 1
rar_vals <- c(0.5, 0.6, 0.7, 0.8, 0.9)  # Excluded rar = 0.9
eta_vals <- c(1, 2, 5)  # Using eta = 2 and eta = 4

# Step 1: Keep type as character
df_nonlinear <- do.call(rbind, lapply(rar_vals, function(rar) {
  do.call(rbind, lapply(eta_vals, function(eta) {
    data.frame(
      fi = fi_vals,
      F_fi = DABC_mod(fi_vals, rar, eta),
      type = paste0("(π_E=", rar, ", η=", eta, ")"),
      stringsAsFactors = FALSE
    )
  }))
}))

# Expression labels for scale
label_expressions <- c(
  "(π_E=0.7, η=1)" = expression(hat(pi)[E] == 0.7 ~ ", " ~ eta == 1),
  "(π_E=0.7, η=2)" = expression(hat(pi)[E] == 0.7 ~ ", " ~ eta == 2),
  "(π_E=0.7, η=5)" = expression(hat(pi)[E] == 0.7 ~ ", " ~ eta == 5),
  "(π_E=0.9, η=1)" = expression(hat(pi)[E] == 0.9 ~ ", " ~ eta == 1),
  "(π_E=0.9, η=2)" = expression(hat(pi)[E] == 0.9 ~ ", " ~ eta == 2),
  "(π_E=0.9, η=5)" = expression(hat(pi)[E] == 0.9 ~ ", " ~ eta == 5)
)



# Define types
types_07 <- c("(π_E=0.7, η=1)", "(π_E=0.7, η=2)", "(π_E=0.7, η=5)")
types_09 <- c("(π_E=0.9, η=1)", "(π_E=0.9, η=2)", "(π_E=0.9, η=5)")

# Assign colors from two palettes
colors_07 <- brewer.pal(n = 12, name = "Paired")[1:length(types_07)]
colors_09 <- brewer.pal(n = min(9, length(types_09)), name = "Reds")[1:length(types_09)]

# Combine into one named vector
custom_colors <- setNames(c(colors_07, colors_09), c(types_07, types_09))

# Filter relevant data
selected_types <- c(types_07, types_09)

g <- df_nonlinear %>%
  filter(type %in% names(label_expressions)) %>%
  ggplot(aes(x = fi, y = F_fi, color = type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = custom_colors, labels = label_expressions) +
  ylim(0.5, 1) +
  xlim(0, 1) +
  theme_minimal(base_size = 12) +
  labs(
    title = "",
    subtitle = "",
    x = expression(hat(Phi)),
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = ""
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

g


cairo_pdf("Figure1_fixed.pdf", width = 6, height = 5, family = "DejaVu Sans")
plot(g)   # your plotting code
dev.off()
