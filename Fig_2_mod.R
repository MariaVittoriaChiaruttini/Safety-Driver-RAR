shape_param <- 50  # Example choice, can be adjusted
# Compute scale parameter to get mean = 0.5
scale_params <- c(0, 0.01, 0.03, 0.05) / shape_param

median1_drug = c(1.5, 1.8, 2.25, 2.8, 3.5, 4.5, 6)
median2_drug = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)
lambda1_drug= log(2)/median1_drug
lambda2_drug= log(2)/median2_drug

d=as.data.frame(1/lambda1_drug)
colnames(d)="averaget_red"
d$averagec_red=1/lambda2_drug
d$ney_red=d$averaget_red/(d$averaget_red+d$averagec_red)
d$averaget_red_days=d$averaget_red*30
d$averagec_red_days=d$averagec_red*30
d$cyclest_extra=floor(d$averaget_red_days/21)-3
d$cyclesc_extra=floor(d$averagec_red_days/21)-3
thetac=sapply(d$cyclesc_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[1]) * x))
thetat=sapply(d$cyclest_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[1]) * x))
# d$thetac=exp(6.07+0*d$cyclesc_extra)
# d$thetat=exp(6.07+0*d$cyclest_extra)
d$thetac_avg=colMeans(thetac)
d$thetat_avg=colMeans(thetat)
d$thetac_1q=apply(thetac, 2, quantile, probs = 0.05)
d$thetat_1q=apply(thetat, 2, quantile, probs = 0.05)
d$thetac_3q=apply(thetac, 2, quantile, probs = 0.95)
d$thetat_3q=apply(thetat, 2, quantile, probs = 0.95)
d$ney_pfs_avg=d$thetat_avg/(d$thetat_avg+d$thetac_avg)
d$ney_pfs_1q=d$thetat_1q/(d$thetat_1q+d$thetac_1q)
d$ney_pfs_3q=d$thetat_3q/(d$thetat_3q+d$thetac_3q)
d$case="independent"


g=as.data.frame(1/lambda1_drug)
colnames(g)="averaget_red"
g$averagec_red=1/lambda2_drug
g$ney_red=g$averaget_red/(g$averaget_red+g$averagec_red)
g$averaget_red_days=g$averaget_red*30
g$averagec_red_days=g$averagec_red*30
g$cyclest_extra=floor(g$averaget_red_days/21)-3
g$cyclesc_extra=floor(g$averagec_red_days/21)-3
thetac=sapply(g$cyclesc_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[2]) * x))
thetat=sapply(g$cyclest_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[2]) * x))
g$thetac_avg=colMeans(thetac)
g$thetat_avg=colMeans(thetat)
g$thetac_1q=apply(thetac, 2, quantile, probs = 0.05)
g$thetat_1q=apply(thetat, 2, quantile, probs = 0.05)
g$thetac_3q=apply(thetac, 2, quantile, probs = 0.95)
g$thetat_3q=apply(thetat, 2, quantile, probs = 0.95)
g$ney_pfs_avg=g$thetat_avg/(g$thetat_avg+g$thetac_avg)
g$ney_pfs_1q=g$thetat_1q/(g$thetat_1q+g$thetac_1q)
g$ney_pfs_3q=g$thetat_3q/(g$thetat_3q+g$thetac_3q)
g$case="moderate"




e=as.data.frame(1/lambda1_drug)
colnames(e)="averaget_red"
e$averagec_red=1/lambda2_drug
e$ney_red=e$averaget_red/(e$averaget_red+e$averagec_red)
e$averaget_red_days=e$averaget_red*30
e$averagec_red_days=e$averagec_red*30
e$cyclest_extra=floor(e$averaget_red_days/21)-3
e$cyclesc_extra=floor(e$averagec_red_days/21)-3
thetac=sapply(e$cyclesc_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[3]) * x))
thetat=sapply(e$cyclest_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[3]) * x))
e$thetac_avg=colMeans(thetac)
e$thetat_avg=colMeans(thetat)
e$thetac_1q=apply(thetac, 2, quantile, probs = 0.05)
e$thetat_1q=apply(thetat, 2, quantile, probs = 0.05)
e$thetac_3q=apply(thetac, 2, quantile, probs = 0.95)
e$thetat_3q=apply(thetat, 2, quantile, probs = 0.95)
e$ney_pfs_avg=e$thetat_avg/(e$thetat_avg+e$thetac_avg)
e$ney_pfs_1q=e$thetat_1q/(e$thetat_1q+e$thetac_1q)
e$ney_pfs_3q=e$thetat_3q/(e$thetat_3q+e$thetac_3q)
e$case="strong"

f=as.data.frame(1/lambda1_drug)
colnames(f)="averaget_red"
f$averagec_red=1/lambda2_drug
f$ney_red=f$averaget_red/(f$averaget_red+f$averagec_red)
f$averaget_red_days=f$averaget_red*30
f$averagec_red_days=f$averagec_red*30
f$cyclest_extra=floor(f$averaget_red_days/21)-3
f$cyclesc_extra=floor(f$averagec_red_days/21)-3
thetac=sapply(f$cyclesc_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[4]) * x))
thetat=sapply(f$cyclest_extra, function(x) exp(6.07 + rgamma(10000,shape = shape_param, scale =scale_params[4]) * x))
f$thetac_avg=colMeans(thetac)
f$thetat_avg=colMeans(thetat)
f$thetac_1q=apply(thetac, 2, quantile, probs = 0.05)
f$thetat_1q=apply(thetat, 2, quantile, probs = 0.05)
f$thetac_3q=apply(thetac, 2, quantile, probs = 0.95)
f$thetat_3q=apply(thetat, 2, quantile, probs = 0.95)
f$ney_pfs_avg=f$thetat_avg/(f$thetat_avg+f$thetac_avg)
f$ney_pfs_1q=f$thetat_1q/(f$thetat_1q+f$thetac_1q)
f$ney_pfs_3q=f$thetat_3q/(f$thetat_3q+f$thetac_3q)
f$case="very strong"



dd=rbind(d,g, e, f)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)

colors <- brewer.pal(n = 12, name = "Paired")[1:length(unique(dd$case))]

g=ggplot(dd, aes(x = averaget_red/averagec_red, y = thetat_avg/thetac_avg, color = case, fill = case)) +
  geom_ribbon(aes(ymin = thetat_1q/thetac_1q, ymax = thetat_3q/thetac_3q), alpha = 0.2) +
  geom_line(size = 1) +  # Linea per collegare i punti
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(
    title = "",
    x = "Ratio of average time to dose reduction (E vs C)",
    y = "Ratio of average PFS (E vs C)",
    color = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

g

cairo_pdf("Figure2_fixed.pdf", width = 6, height = 5, family = "DejaVu Sans")
plot(g)   # your plotting code
dev.off()
