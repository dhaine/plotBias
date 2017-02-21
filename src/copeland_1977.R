## Bias due to misclassification in the estimation of relative risk
## Copeland, Karen T., Checkoway, Harvey, McMichael, Anthony J., and Holbrook, Robert H.
## Am. J. Epidemiol. (1977) 105(5) 488-495

## Bias due to misclassification: a function of Se and Sp, disease frequency, and
## exposure frequency.

## Non-differential misclassification
## Figure 1
dat <- expand.grid(sp = seq(0.5, 1, 0.01),
                   se = seq(.5, .9, 0.2))

dat$nA <- 100
dat$nB <- 100
dat$mA_1 <- 10
dat$mA_0 <- 90
dat$mB_1 <- 5
dat$mB_0 <- 95

dat$aA <- dat$se * dat$mA_1
dat$dA <- dat$sp * dat$mA_0
dat$aB <- dat$se * dat$mB_1
dat$dB <- dat$sp * dat$mB_0

dat$nA_1 <- dat$aA + (dat$mA_0 - dat$dA)
dat$nB_1 <- dat$aB + (dat$mB_0 - dat$dB)

dat$TRR <- (dat$mA_1/dat$nA) / (dat$mB_1/dat$nB)
dat$ARR <- (dat$nA_1/dat$nA) / (dat$nB_1/dat$nB)

dat$se <- as.factor(dat$se)

library(ggplot2)
library(ggthemes)
ColourSchemes <- c(rgb(119, 17, 34, max = 255),  # red --- management
                   rgb(68, 170, 170, max = 255),  # teal --- reprod
                   rgb(17, 119, 68, max = 255),  # green --- production
                   rgb(119, 170, 221, max = 255))  ## light blue --- health

ggplot(dat, aes(x = sp, y = ARR, group = se, colour = se)) +
    geom_line(size = 1) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "SENSITIVITY OF TEST")) +
    scale_x_continuous(breaks = seq(.5, 1, .05)) + 
    scale_y_continuous(breaks = seq(1, 2, .2)) +
    geom_segment(aes(x=.96, y=1, xend=.96, yend=1.518072), colour="grey42") +
    geom_segment(aes(x=.5, y=1.518072, xend=.96, yend=1.518072), colour="grey42") +
    geom_text(x=.985, y=1, label="(0.96)", colour="grey42", size=3) +
    geom_text(x=.5, y=1.47, label="(1.51)", colour="grey42", size=3) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.position = c(.25, .75),
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
    xlab("Specificity of test") + 
    ylab("Apparent relative risk") +
    ggtitle("Bias as a function of sensitivity and specificity",
            subtitle="Cohort study") +
    labs(caption = "Risk in population A = .10; Risk in population B = .05; True relative risk = 2.0\nCopeland et al. (1977), Am. J. Epidemiol. 105(5), 488-495")
ggsave(filename = "graphics/pdf/copeland_77_1.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/copeland_77_1.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 2
dat <- expand.grid(sp = seq(0.5, 1, 0.01),
                   se = .9,
                   nA = 100,
                   nB = 100,
                   mA_1 = seq(5, 20, 5),
                   mB_1 = seq(2.5, 10, 2.5))
dat <- subset(dat, mA_1 != 15)
dat <- subset(dat, mB_1 != 7.5)
dat <- subset(dat, (mA_1==20 & mB_1==10) | (mA_1==10 & mB_1==5) | (mA_1==5 & mB_1==2.5))

dat$mA_0 <- dat$nA - dat$mA_1
dat$mB_0 <- dat$nB - dat$mB_1
dat$aA <- dat$se * dat$mA_1
dat$dA <- dat$sp * dat$mA_0
dat$aB <- dat$se * dat$mB_1
dat$dB <- dat$sp * dat$mB_0

dat$nA_1 <- dat$aA + (dat$mA_0 - dat$dA)
dat$nB_1 <- dat$aB + (dat$mB_0 - dat$dB)

dat$TRR <- (dat$mA_1/dat$nA) / (dat$mB_1/dat$nB)
dat$ARR <- (dat$nA_1/dat$nA) / (dat$nB_1/dat$nB)

dat$risk <- with(dat, ifelse(mA_1==20 & mB_1==10, ".20 in pop. A; .10 in pop. B",
                      ifelse(mA_1==10 & mB_1==5, ".10 in pop. A; .05 in pop. B",
                             ".05 in pop. A; .025 in pop. B")))

ggplot(dat, aes(x = sp, y = ARR, group = risk, colour = risk)) +
    geom_line(size = 1) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "DISEASE RISK")) +
    scale_x_continuous(breaks = seq(.5, 1, .05)) + 
    scale_y_continuous(breaks = seq(1, 2, .2)) +
    theme_fivethirtyeight() + 
    theme(axis.title=element_text(face="bold"),
          plot.title=element_text(size=14),
          legend.position = c(.35, .75),
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
    xlab("Specificity of test") + 
    ylab("Apparent relative risk") +
    ggtitle("Bias as a function of specificity and disease risk",
            subtitle="Cohort study") +
    labs(caption = "Sensitivity = 0.90; True relative risk = 2.0\nCopeland et al. (1977), Am. J. Epidemiol. 105(5), 488-495")
ggsave(filename = "graphics/pdf/copeland_77_2.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/copeland_77_2.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
