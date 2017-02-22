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

ggplot(dat, aes(x = sp, y = ARR, group = se, colour = se)) +
    geom_line(size = 1) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "SENSIBILITÉ DU TEST")) +
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
    xlab("Spécificité du test") + 
    ylab("Risque relatif apparent") +
    ggtitle("Biais en fonction de la sensibilité et de la spécificité",
            subtitle="Étude cohorte") +
    labs(caption = "Risque dans la population A = .10; Risque dans la population B = .05; Risque relatif réel = 2.0\nCopeland et al. (1977), Am. J. Epidemiol. 105(5), 488-495")
ggsave(filename = "graphics/pdf/copeland_77_1fr.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/copeland_77_1fr.jpg", width = 14, height = 10.5,
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

## Figure 3
dat <- expand.grid(sp = seq(0.5, 1, 0.01),
                   se = seq(.5, .9, 0.2),
                   a_true = 40,
                   b_true = 20,
                   c_true = 60,
                   d_true = 80)

dat$a_observed <- (dat$se*dat$a_true) + (dat$c_true - (dat$sp*dat$c_true))
dat$c_observed <- (dat$a_true - (dat$se*dat$a_true)) + (dat$sp*dat$c_true)
dat$b_observed <- (dat$se*dat$b_true) + (dat$d_true - (dat$sp*dat$d_true))
dat$d_observed <- (dat$b_true - (dat$se*dat$b_true)) + (dat$sp*dat$d_true)

dat$TOR <- (dat$a_true*dat$d_true) / (dat$c_true*dat$b_true)
dat$AOR <- (dat$a_observed*dat$d_observed) / (dat$c_observed*dat$b_observed)

dat$se <- as.factor(dat$se)

ggplot(dat, aes(x = sp, y = AOR, group = se, colour = se)) +
    geom_line(size = 1) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "SENSITIVITY OF TEST")) +
    scale_x_continuous(breaks = seq(.5, 1, .05)) + 
    scale_y_continuous(breaks = seq(1, 2.8, .3)) +
    expand_limits(y=2.8) +
    geom_text(x=1, y=2.67, label="X", colour="grey42", size=3) +
    geom_text(x=.97, y=2.67, label="(2.67)", colour="grey42", size=3) +
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
    ylab("Apparent odds ratio") +
    ggtitle("Bias as a function of sensitivity and specificity",
            subtitle="Case-control study") +
    labs(caption = "Exposure rates: cases = .4, controls = .2. True odds ratio = 2.67\nCopeland et al. (1977), Am. J. Epidemiol. 105(5), 488-495")
ggsave(filename = "graphics/pdf/copeland_77_3.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/copeland_77_3.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)

## Figure 4
dat1 <- expand.grid(sp = seq(0.5, 1, 0.01),
                    se = 0.7,
                    a_true = 40,
                    b_true = 20,
                    c_true = 60,
                    d_true = 80,
                    group = "cases=.4, controls=.2; OR=2.67")
dat2 <- expand.grid(sp = seq(0.5, 1, 0.01),
                    se = 0.7,
                    a_true = 50,
                    b_true = 30,
                    c_true = 50,
                    d_true = 70,
                    group = "cases=.5, controls=.3; OR=2.33")
dat3 <- expand.grid(sp = seq(0.5, 1, 0.01),
                    se = 0.7,
                    a_true = 20,
                    b_true = 10,
                    c_true = 80,
                    d_true = 90,group = "cases=.2, controls=.1; OR=2.25")
dat <- rbind(dat1, dat2, dat3)

dat$a_observed <- (dat$se*dat$a_true) + (dat$c_true - (dat$sp*dat$c_true))
dat$c_observed <- (dat$a_true - (dat$se*dat$a_true)) + (dat$sp*dat$c_true)
dat$b_observed <- (dat$se*dat$b_true) + (dat$d_true - (dat$sp*dat$d_true))
dat$d_observed <- (dat$b_true - (dat$se*dat$b_true)) + (dat$sp*dat$d_true)

dat$TOR <- (dat$a_true*dat$d_true) / (dat$c_true*dat$b_true)
dat$AOR <- (dat$a_observed*dat$d_observed) / (dat$c_observed*dat$b_observed)

ggplot(dat, aes(x = sp, y = AOR, group = group, colour = group)) +
    geom_line(size = 1) +
    scale_colour_fivethirtyeight(guide = guide_legend(title = "EXPOSURE RATE & TRUE ODDS RATIO")) +
    scale_x_continuous(breaks = seq(.5, 1, .05)) + 
    scale_y_continuous(breaks = seq(1, 2.8, .3)) +
    expand_limits(y=2.8) +
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
    ylab("Apparent odds ratio") +
    ggtitle("Bias as a function of specificity and exposure rate",
            subtitle="Case-control study") +
    labs(caption = "Sensitivity = 0.7\nCopeland et al. (1977), Am. J. Epidemiol. 105(5), 488-495")
ggsave(filename = "graphics/pdf/copeland_77_4.pdf", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
ggsave(filename = "graphics/jpg/copeland_77_4.jpg", width = 14, height = 10.5,
       units = "cm", dpi = 1200)
