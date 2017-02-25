## The effects of exposure misclassification on estimates of relative risk
## Flegal, Katherine M., Brownie, C., Haas, Jere D.
## Am. J. Epidemiol. (1986) 123(4) 736-751

## R* = [(URP(E) + (1-V)P(E_bar)) * ((1-U)P(E) + VP(E_bar))] / [(UP(E)+ (1-V)P(E_bar)) * ((1-U)RP(E)+ VP(E_bar))]
## where U=Se, V=Sp, P(E)=prevalence of exposure in population, P(E_bar)=the complement of P(E), R=true relative risk

library(ggplot2)
library(ggthemes)
library(directlabels)

## Unit contours of R* for different levels of R and P(E)
## Figure 1
dat <- expand.grid(sp = seq(0, 1, 0.01),
                    se = seq(0, 1, 0.01),
                    R = c(5, 10),
                    pE = c(0.1, 0.24, 0.5))
dat$levels <- with(dat, ifelse(R == 10 & pE == 0.5, "R=10; Pr=0.5",
                        ifelse(R == 10 & pE == 0.24, "R=10; Pr=0.24",
                        ifelse(R == 10 & pE == 0.1, "R=10; Pr=0.1",
                        ifelse(R == 5 & pE == 0.5, "R=5; Pr=0.5",
                        ifelse(R == 5 & pE == 0.24, "R=5; Pr=0.24",
                               "R=5; Pr=0.1"))))))

dat$R_star <- with(dat,
((se*R*pE + (1 - sp)*(1-pE))*((1 - se)*pE + sp*(1-pE))) /
((se*pE + (1 - sp)*(1-pE))*((1-se)*R*pE + sp*(1-pE)))
                  )
p <- ggplot(data=dat, aes(x=sp, y=se, z=R_star)) + 
    geom_raster(data=dat, aes(fill=R_star), show.legend = TRUE) +
    scale_fill_gradient(limits=range(dat$R_star), high = 'red', low = 'green') +
    scale_x_continuous(breaks = seq(0, 1, .2), expand=c(0,0)) +
    scale_y_continuous(breaks = seq(0, 1, .2), expand=c(0,0)) +
    coord_fixed() +
    geom_contour(aes(colour = ..level..)) +
    scale_colour_gradient(guide = 'none') +
    theme(axis.title = element_text(face="bold"),
          axis.text.y = element_text(colour="grey42"),
          axis.text.x = element_text(colour="grey42"),
          legend.position = "none") +
    xlab("Specificity") +
    ylab("Sensitivity") +
    ggtitle("Unit contours of apparent relative risk for different levels of true relative risk and prevalence") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
p1 <- direct.label(p, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
      hjust=1, vjust=-.5, box.color = NA, fill = "transparent", "draw.rects"))
p2 <- p1 + facet_wrap(~ levels, nrow=2)
p2
ggsave(filename = "graphics/pdf/copeland_77_1.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/copeland_77_1.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 2
## R* as a function of U when U + V = 1.70, for selected values of P(E)
dat <- expand.grid(sp = seq(0, 1, 0.01),
                   se = seq(0, 1, 0.01),
                   R = 10,
                   pE = c(0.05, 0.24, 0.55))
dat$R_star <- with(dat,
((se*R*pE + (1 - sp)*(1-pE))*((1 - se)*pE + sp*(1-pE))) /
((se*pE + (1 - sp)*(1-pE))*((1-se)*R*pE + sp*(1-pE)))
)
elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
dat <- subset(dat, elementwise.all.equal(se+sp, 1.7))
dat$pE <- as.factor(dat$pE)

ggplot(dat, aes(x = se, y = R_star, group = pE, colour = pE)) +
    stat_smooth(method="auto", se=FALSE) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "PREVALENCE")) +
    scale_x_continuous(breaks = seq(.5, 1, .1)) + 
    scale_y_continuous(breaks = seq(1, 10, 1)) +
    expand_limits(#x=c(0.5, 1),
        y=c(1, 10)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.position = c(.5, .75),
          legend.direction="vertical",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.title=element_text(colour="grey41", size=10),
          legend.key.width = unit(2, "cm"),
          legend.justification = "centre",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
    xlab("Sensitivity") + 
    ylab("Apparent relative risk") +
    ggtitle("Apparent relative risk as a function of sensitivity",
            subtitle="True relative risk =10; Se + Sp = 1.70") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_2.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_2.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 3
## R* as a function of U when U + V = 1.50, for selected values of P(E)
dat <- expand.grid(sp = seq(0, 1, 0.01),
                   se = seq(0, 1, 0.01),
                   R = 10,
                   pE = c(0.15, 0.25, 0.35))
dat$R_star <- with(dat,
((se*R*pE + (1 - sp)*(1-pE))*((1 - se)*pE + sp*(1-pE))) /
((se*pE + (1 - sp)*(1-pE))*((1-se)*R*pE + sp*(1-pE)))
)
dat <- subset(dat, elementwise.all.equal(se+sp, 1.5))
dat$pE <- as.factor(dat$pE)

ggplot(dat, aes(x = se, y = R_star, group = pE, colour = pE)) +
    stat_smooth(method="auto", se=FALSE) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "PREVALENCE")) +
    scale_x_continuous(breaks = seq(.5, 1, .1)) + 
    scale_y_continuous(breaks = seq(1, 10, 1)) +
    expand_limits(#x=c(0.5, 1),
        y=c(1, 10)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.position = c(.5, .75),
          legend.direction="vertical",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.title=element_text(colour="grey41", size=10),
          legend.key.width = unit(2, "cm"),
          legend.justification = "centre",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
    xlab("Sensitivity") + 
    ylab("Apparent relative risk") +
    ggtitle("Apparent relative risk as a function of sensitivity",
            subtitle="True relative risk = 10; Se + Sp = 1.50") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_3.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_3.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 4
## R* as a function of P(E), for selected values of U and V
## where U=Se, V=Sp, P(E)=prevalence of exposure in population, P(E_bar)=the complement of P(E), R=true relative risk
dat <- expand.grid(sp = c(0.8, 0.85, 0.9, 0.95, 0.99),
                   se = c(0.8, 0.85, 0.9, 0.95, 0.99),
                   R = 10,
                   pE = seq(0, 1, 0.001))
dat$R_star <- with(dat,
((se*R*pE + (1 - sp)*(1-pE))*((1 - se)*pE + sp*(1-pE))) /
((se*pE + (1 - sp)*(1-pE))*((1-se)*R*pE + sp*(1-pE)))
)
dat <- subset(dat, (se==0.99 & sp==0.8) | (se==0.95 & sp==0.85) | (se==0.9 & sp==0.9) |
              (se==0.85 & sp==0.95) | (se==0.8 & sp==0.99))
dat$levels <- with(dat, ifelse(se==0.99 & sp==0.8, "Se=0.99, Sp==0.80",
                        ifelse(se==0.95 & sp==0.85, "Se=0.95, Sp=0.85",
                        ifelse(se==0.9 & sp==0.9, "Se=0.9, Sp=0.9",
                        ifelse(se==0.85 & sp==0.95, "Se=0.85, Sp=0.95",
                               "Se=0.80, Sp=0.99")))))

palette538 <- c("#FF2700", "#008FD5", "#77AB43", "#8B008B", "#FFA500")
                                "blue", "green", "magenta4", "orange"))
ggplot(dat, aes(x = pE, y = R_star, group = levels, colour = levels)) +
    geom_line(size=1) +
    scale_color_manual(values=palette538) +
    scale_x_continuous(breaks = seq(0, 1, .1)) + 
    scale_y_continuous(breaks = seq(1, 10, 1)) +
    expand_limits(y=c(1, 10)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.title=element_blank(),
          legend.position = "bottom",
          legend.direction="horizontal",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.key.width = unit(2, "cm"),
          legend.justification = "center",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
    guides(col=guide_legend(ncol=2)) +
    xlab("Prevelance of exposure") + 
    ylab("Apparent relative risk") +
    ggtitle("Apparent relative risk as a function of prevalence",
            subtitle="True relative risk = 10") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_4.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_4.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 5
## P(E)_max_R* as a function of R, for selected values of U and V
## P(E)_max_R* = 1 / (1 + sqrt((R*se*(1-se)) / (sp*(1-sp))))
## where U=Se, V=Sp, P(E)=prevalence of exposure in population, P(E_bar)=the complement of P(E), R=true relative risk
dat <- expand.grid(sp = c(0.7, 0.9, 0.99),
                   se = c(0.7, 0.9, 0.99),
                   R = seq(1, 15, .1),
                   pE = seq(0, 1, 0.01))
dat$pE_max <- with(dat, 1 / (1 + sqrt((R*se*(1-se)) / (sp*(1-sp))))
                   )
dat <- subset(dat, (se==0.99 & sp==0.7) | (se==0.99 & sp==0.9) | (se==0.99 & sp==0.99) |
              (se==0.9 & sp==0.99) | (se==0.7 & sp==0.99))
dat$levels <- with(dat, ifelse(se==0.99 & sp==0.7, "Se=0.99, Sp==0.70",
                        ifelse(se==0.99 & sp==0.9, "Se=0.99, Sp=0.90",
                        ifelse(se==0.99 & sp==0.99, "Se=0.99, Sp=0.99",
                        ifelse(se==0.9 & sp==0.99, "Se=0.90, Sp=0.99",
                               "Se=0.70, Sp=0.99")))))

ggplot(dat, aes(x = R, y = pE_max, group = levels, colour = levels)) +
    geom_line(size=1) +
    scale_color_manual(values=palette538) +
    scale_x_continuous(breaks = seq(0, 15, 1)) + 
    scale_y_continuous(breaks = seq(0, 1, .2)) +
    expand_limits(y=c(0, 1)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.title=element_blank(),
          legend.position = "bottom",
          legend.direction="vertical",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.key.width = unit(2, "cm"),
          legend.justification = "center",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
    guides(col=guide_legend(ncol=2)) +
    xlab("True relative risk") + 
    ylab("Prevalence for maximum\napparent relative risk") +
    ggtitle("Prevalence for maximum apparent relative risk\nas a function of true relative risk") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_5.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_5.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 6
## R* as a function of R, for selected values of U and V when P(E)=P(E)_max_R*
## P(E)_max_R* = 1 / (1 + sqrt((R*se*(1-se)) / (sp*(1-sp))))
## where U=Se, V=Sp, P(E)=prevalence of exposure in population, P(E_bar)=the complement of P(E), R=true relative risk
dat <- expand.grid(sp = c(0.8, 0.85, 0.9, 0.95, 0.99, 1),
                   se = c(0.8, 0.85, 0.9, 0.95, 0.99, 1),
                   R = seq(1, 15, .01)
                   )

dat$pE_max <- with(dat, 1 / (1 + sqrt((R*se*(1-se)) / (sp*(1-sp))))
                   )
dat$pE <- dat$pE_max

dat$R_star <- with(dat,
((se*R*pE + (1 - sp)*(1-pE))*((1 - se)*pE + sp*(1-pE))) /
((se*pE + (1 - sp)*(1-pE))*((1-se)*R*pE + sp*(1-pE)))
)


dat <- subset(dat, (se==1 & sp==1) | (se==0.99 & sp==0.99) | (se==0.95 & sp==0.95) |
              (se==0.9 & sp==0.9) | (se==0.85 & sp==0.85) | (se==0.8 & sp==0.8))
dat$levels <- with(dat, ifelse(se==1 & sp==1, "Se=1.00, Sp==1.00",
                        ifelse(se==0.99 & sp==0.99, "Se=0.99, Sp=0.99",
                        ifelse(se==0.95 & sp==0.95, "Se=0.95, Sp=0.95",
                        ifelse(se==0.9 & sp==0.9, "Se=0.90, Sp=0.90",
                        ifelse(se==0.85 & sp==0.85, "Se=0.85, Sp=0.85",
                               "Se=0.80, Sp=0.80"))))))
dat$R_star <- with(dat, ifelse(se==1 & sp==1, R, R_star))

palette538 <- c(palette538, "#3C3C3C")
ggplot(dat, aes(x = R, y = R_star, group = levels, colour = levels)) +
    geom_line() +
    scale_color_manual(values=palette538) +
    scale_x_continuous(breaks = seq(1, 15, 1)) + 
    scale_y_continuous(breaks = seq(1, 15, 1)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.title=element_blank(),
          legend.position = "bottom",
          legend.direction="vertical",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.key.width = unit(2, "cm"),
          legend.justification = "center",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
    guides(col=guide_legend(ncol=2)) +
    xlab("True relative risk") + 
    ylab("Apparent relative risk") +
    ggtitle("Apparent relative risk as a function of true relative risk",
            subtitle="When prevalence maximizes apparent relative risk") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_6.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_6.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 7
## Bias (R - R*) as a function of R, for selected values of U and V when P(E)=P(E)_max_R*
## P(E)_max_R* = 1 / (1 + sqrt((R*se*(1-se)) / (sp*(1-sp))))
## where U=Se, V=Sp, P(E)=prevalence of exposure in population, P(E_bar)=the complement of P(E), R=true relative risk
dat <- subset(dat, se != 1)

dat$bias <- dat$R - dat$R_star

ggplot(dat, aes(x = R, y = bias, group = levels, colour = levels)) +
    geom_line() +
    scale_color_manual(values=palette538[1:5]) +
    scale_x_continuous(breaks = seq(1, 15, 1)) + 
    scale_y_continuous(breaks = seq(0, 11, 1)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.title=element_blank(),
          legend.position = c(.25, .75),
          legend.direction="vertical",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.key.width = unit(2, "cm"),
          legend.justification = "center",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
#    guides(col=guide_legend(ncol=2)) +
    xlab("True relative risk") + 
    ylab("Bias") +
    ggtitle("Bias (true relative risk - apparent relative risk)",
            subtitle="When prevalence maximizes apparent relative risk") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_7.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_7.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 8
## (R/R*) as a function of R, for selected values of U and V when P(E)=P(E)_max_R*
## P(E)_max_R* = 1 / (1 + sqrt((R*se*(1-se)) / (sp*(1-sp))))
## where U=Se, V=Sp, P(E)=prevalence of exposure in population, P(E_bar)=the complement of P(E), R=true relative risk
dat$bias_ratio <- dat$R_star/dat$R

ggplot(dat, aes(x = R, y = bias_ratio, group = levels, colour = levels)) +
    geom_line() +
    scale_color_manual(values=palette538[1:5]) +
    scale_x_continuous(breaks = seq(1, 15, 1)) + 
    scale_y_continuous(breaks = seq(0, 1, .2)) +
    expand_limits(y=c(0,1)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.title=element_blank(),
          legend.position = "bottom",
          legend.direction="vertical",
          legend.key=element_rect(colour="transparent"),
          legend.background=element_rect(linetype="solid", colour="grey42", size=.2),
          legend.text=element_text(colour="grey41"),
          legend.key.width = unit(2, "cm"),
          legend.justification = "center",
          axis.text.y = element_text(colour = "grey42"),
          axis.text.x = element_text(colour="grey42"),
          plot.caption = element_text(colour="grey41", size=8)) +
    guides(col=guide_legend(ncol=2)) +
    xlab("True relative risk") + 
    ylab("Ratio apparent/true relative risk") +
    ggtitle("Apparent relative risk/true relative risk ratio",
            subtitle="When prevalence maximizes apparent relative risk") +
    labs(caption = "Flegal et al. (1986), Am. J. Epidemiol. 123(4), 736-751")
ggsave(filename = "graphics/pdf/flegal_86_8.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/flegal_86_8.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
