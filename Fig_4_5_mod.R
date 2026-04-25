#figure 4
### strong association, target allocation 0.8

### LOAD cubo_allocation_long_s4.Rds

months=rep(seq(3, 48, by = 3),5)
d=as.data.frame(months)
d$PFS=as.factor(rep(c(3,9,18,24,30), each=16))
dimnames(cubo_allocation_long_s4)
tre=colMeans(cubo_allocation_long_s4[,,11])
nove=colMeans(cubo_allocation_long_s4[,,12])
diciotto=colMeans(cubo_allocation_long_s4[,,13])
ventiquattro=colMeans(cubo_allocation_long_s4[,,14])
trenta=colMeans(cubo_allocation_long_s4[,,15])
d$avg_alloc=ifelse(d$PFS==3, tre, ifelse(d$PFS==9, nove,
                  ifelse(d$PFS==18, diciotto, ifelse(d$PFS==24, ventiquattro, trenta))))
tre_25=apply(cubo_allocation_long_s4[,,11], 2, function(x) quantile(x, probs = c(0.25)))
tre_50=apply(cubo_allocation_long_s4[,,11], 2, function(x) quantile(x, probs = c(0.5)))
tre_75=apply(cubo_allocation_long_s4[,,11], 2, function(x) quantile(x, probs = c(0.75)))

nove_25=apply(cubo_allocation_long_s4[,,12], 2, function(x) quantile(x, probs = c(0.25)))
nove_50=apply(cubo_allocation_long_s4[,,12], 2, function(x) quantile(x, probs = c(0.5)))
nove_75=apply(cubo_allocation_long_s4[,,12], 2, function(x) quantile(x, probs = c(0.75)))

diciotto_25=apply(cubo_allocation_long_s4[,,13], 2, function(x) quantile(x, probs = c(0.25)))
diciotto_50=apply(cubo_allocation_long_s4[,,13], 2, function(x) quantile(x, probs = c(0.5)))
diciotto_75=apply(cubo_allocation_long_s4[,,13], 2, function(x) quantile(x, probs = c(0.75)))

ventiquattro_25=apply(cubo_allocation_long_s4[,,14], 2, function(x) quantile(x, probs = c(0.25)))
ventiquattro_50=apply(cubo_allocation_long_s4[,,14], 2, function(x) quantile(x, probs = c(0.5)))
ventiquattro_75=apply(cubo_allocation_long_s4[,,14], 2, function(x) quantile(x, probs = c(0.75)))

trenta_25=apply(cubo_allocation_long_s4[,,15], 2, function(x) quantile(x, probs = c(0.25)))
trenta_50=apply(cubo_allocation_long_s4[,,15], 2, function(x) quantile(x, probs = c(0.5)))
trenta_75=apply(cubo_allocation_long_s4[,,15], 2, function(x) quantile(x, probs = c(0.75)))

d$med_alloc=ifelse(d$PFS==3, tre_50, ifelse(d$PFS==9, nove_50,
       ifelse(d$PFS==18, diciotto_50, ifelse(d$PFS==24, ventiquattro_50, trenta_50))))

d$first_alloc=ifelse(d$PFS==3, tre_25, ifelse(d$PFS==9, nove_25,
       ifelse(d$PFS==18, diciotto_25, ifelse(d$PFS==24, ventiquattro_25, trenta_25))))

d$third_alloc=ifelse(d$PFS==3, tre_75, ifelse(d$PFS==9, nove_75,
        ifelse(d$PFS==18, diciotto_75, ifelse(d$PFS==24, ventiquattro_75, trenta_75))))

library(RColorBrewer)
library(ggplot2)

colors <- brewer.pal(n = 5, name = "Paired")[1:length(unique(d$PFS))]

ggplot(d, aes(x = months, y = avg_alloc, color = PFS, fill = PFS)) +
  #geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  #geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  labs(
    title = "",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

library(dplyr)

colors <- brewer.pal(n = 5, name = "Paired")[1]

g1=d%>% filter(PFS==3) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "A",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g1

colors <- brewer.pal(n = 5, name = "Paired")[2]

g2=d%>% filter(PFS==9) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "B",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g2


colors <- brewer.pal(n = 5, name = "Paired")[3]

g3=d%>% filter(PFS==18) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "C",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g3

colors <- brewer.pal(n = 5, name = "Paired")[4]

g4=d%>% filter(PFS==24) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "D",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g4

# Load the patchwork package
library(patchwork)

# Arrange the plots in a 2x2 grid
final_plot <- (g1 | g2) / (g3 | g4)

# Print the arranged plot
print(final_plot)

d=d%>%rename(Q1_alloc=first_alloc, Q3_alloc=third_alloc)
#writexl::write_xlsx(d, "scenario4.xlsx")


cairo_pdf("Figure4_fixed.pdf", width = 10, height = 7, family = "DejaVu Sans")
plot(final_plot)   # your plotting code
dev.off()

#figure 5

### weak association, target allocation 0.8


### LOAD cubo_allocation_long_s4_weak.Rds

months=rep(seq(3, 48, by = 3),5)
d=as.data.frame(months)
d$PFS=as.factor(rep(c(3,9,18,24,30), each=16))
dimnames(cubo_allocation_long_s4_weak)
tre=colMeans(cubo_allocation_long_s4_weak[,,11])
nove=colMeans(cubo_allocation_long_s4_weak[,,12])
diciotto=colMeans(cubo_allocation_long_s4_weak[,,13])
ventiquattro=colMeans(cubo_allocation_long_s4_weak[,,14])
trenta=colMeans(cubo_allocation_long_s4_weak[,,15])
d$avg_alloc=ifelse(d$PFS==3, tre, ifelse(d$PFS==9, nove,
                                         ifelse(d$PFS==18, diciotto, ifelse(d$PFS==24, ventiquattro, trenta))))
tre_25=apply(cubo_allocation_long_s4_weak[,,11], 2, function(x) quantile(x, probs = c(0.25)))
tre_50=apply(cubo_allocation_long_s4_weak[,,11], 2, function(x) quantile(x, probs = c(0.5)))
tre_75=apply(cubo_allocation_long_s4_weak[,,11], 2, function(x) quantile(x, probs = c(0.75)))

nove_25=apply(cubo_allocation_long_s4_weak[,,12], 2, function(x) quantile(x, probs = c(0.25)))
nove_50=apply(cubo_allocation_long_s4_weak[,,12], 2, function(x) quantile(x, probs = c(0.5)))
nove_75=apply(cubo_allocation_long_s4_weak[,,12], 2, function(x) quantile(x, probs = c(0.75)))

diciotto_25=apply(cubo_allocation_long_s4_weak[,,13], 2, function(x) quantile(x, probs = c(0.25)))
diciotto_50=apply(cubo_allocation_long_s4_weak[,,13], 2, function(x) quantile(x, probs = c(0.5)))
diciotto_75=apply(cubo_allocation_long_s4_weak[,,13], 2, function(x) quantile(x, probs = c(0.75)))

ventiquattro_25=apply(cubo_allocation_long_s4_weak[,,14], 2, function(x) quantile(x, probs = c(0.25)))
ventiquattro_50=apply(cubo_allocation_long_s4_weak[,,14], 2, function(x) quantile(x, probs = c(0.5)))
ventiquattro_75=apply(cubo_allocation_long_s4_weak[,,14], 2, function(x) quantile(x, probs = c(0.75)))

trenta_25=apply(cubo_allocation_long_s4_weak[,,15], 2, function(x) quantile(x, probs = c(0.25)))
trenta_50=apply(cubo_allocation_long_s4_weak[,,15], 2, function(x) quantile(x, probs = c(0.5)))
trenta_75=apply(cubo_allocation_long_s4_weak[,,15], 2, function(x) quantile(x, probs = c(0.75)))

d$med_alloc=ifelse(d$PFS==3, tre_50, ifelse(d$PFS==9, nove_50,
                                            ifelse(d$PFS==18, diciotto_50, ifelse(d$PFS==24, ventiquattro_50, trenta_50))))

d$first_alloc=ifelse(d$PFS==3, tre_25, ifelse(d$PFS==9, nove_25,
                                              ifelse(d$PFS==18, diciotto_25, ifelse(d$PFS==24, ventiquattro_25, trenta_25))))

d$third_alloc=ifelse(d$PFS==3, tre_75, ifelse(d$PFS==9, nove_75,
                                              ifelse(d$PFS==18, diciotto_75, ifelse(d$PFS==24, ventiquattro_75, trenta_75))))

library(RColorBrewer)
library(ggplot2)

colors <- brewer.pal(n = 5, name = "Paired")[1:length(unique(d$PFS))]

ggplot(d, aes(x = months, y = avg_alloc, color = PFS, fill = PFS)) +
  #geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  #geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  labs(
    title = "",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

library(dplyr)

colors <- brewer.pal(n = 5, name = "Paired")[1]

g1=d%>% filter(PFS==3) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "A",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g1

colors <- brewer.pal(n = 5, name = "Paired")[2]

g2=d%>% filter(PFS==9) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "B",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g2


colors <- brewer.pal(n = 5, name = "Paired")[3]

g3=d%>% filter(PFS==18) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "C",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g3

colors <- brewer.pal(n = 5, name = "Paired")[4]

g4=d%>% filter(PFS==24) %>% ggplot(aes(x = months, y = med_alloc, color = PFS, fill = PFS)) +
  geom_ribbon(aes(ymin = first_alloc, ymax = med_alloc), alpha = 0.2) +
  geom_errorbar(aes(ymin = first_alloc, ymax = med_alloc), width = 1, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(3, 48, by = 3)) +  # x-ticks da 3 a 48
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05), limits = c(0.5,0.85)) +  # y-lim e step
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "D",
    x = "Months",
    y = expression("SAFER(" * hat(pi)[E] * ")"),
    color = "PFS",
    fill = "PFS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g4

# Load the patchwork package
library(patchwork)

# Arrange the plots in a 2x2 grid
final_plot <- (g1 | g2) / (g3 | g4)

# Print the arranged plot
print(final_plot)

d=d%>%rename(Q1_alloc=first_alloc, Q3_alloc=third_alloc)
#writexl::write_xlsx(d, "scenario4.xlsx")



cairo_pdf("Figure5_fixed.pdf", width = 10, height = 7, family = "DejaVu Sans")
plot(final_plot)   # your plotting code
dev.off()


