## CODE FOR:
##### Linking humans, dogs, and patterns of dispersal to stress physiology of California ground squirrels in a semi-urban park ######


# load libraries
library(brms)
library(car)
library(dplyr)
library(emmeans)
library(ggpubr)
library(officer)
library(flextable)
library(ggplot2)
library(patchwork)
library(performance)
library(posterior)
library(tibble)




# 1) full model including 2013-2024 data ------------------------------------------------------------

# read full data set 
CORT_full <- read.csv("Data/Data_for_2013_2024_model.csv")


# 1.1) VIFs ---------------------------------------------------------------


# make sure site is a factor
CORT_full$site <- as.factor(CORT_full$site)

# simple linear model with the same fixed structure as the full model and scaled continuous variables
m_lm <- lm(
  lnCort ~ scale(day.s) + site + stage + sex + scale(mass_div_100),
  data = CORT_full
)



# compute VIFs
vif_vals <- vif(m_lm)
vif_vals

# scale(day.s)                site               stage                 sex scale(mass_div_100) 
# 1.114507            1.055193            3.157393            1.042288            3.062204

vif_df <- as.data.frame(round(vif_vals, 2))

vif_df <- data.frame(Predictor = rownames(vif_df), vif_df, row.names = NULL)


# make flextable
ft <- flextable(vif_df)


# export to Word
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "output tables/vif_table_2013-2024.docx")


# 1.2) Run model --------------------------------------------------------------------


# we run the full model as a Bayesian regression
# we scale continuous variables for better model convergence and interpretability

# instead of running the model, you can also directly load the output below
m_full_area_stage <- brm(
  formula = lnCort ~ scale(day.s) + site*stage + site*sex + sex*scale(mass_div_100) + stage*sex +
    (1 | uid) + (1 | area) + (1 | year),
  family = gaussian(),
  data = CORT_full,
  cores = 4, chains = 4, iter = 4000
)

#save(m_full_area_stage, file="model output/m_full_area_stage.RDA")
load("model output/m_full_area_stage.RDA")

# summary output - considered as having an effect if CIs do not span 0
summary(m_full_area_stage)

# extract fixed-effect summary from the model
sum_df <- as.data.frame(summary(m_full_area_stage)$fixed)
sum_df$term <- rownames(sum_df)


# save the summary as a word table:
out_table <- sum_df %>%
  transmute(
    Term = term,
    Estimate = round(Estimate, 2),
    CI_low = round(`l-95% CI`, 2),
    CI_high = round(`u-95% CI`, 2),
    Rhat = round(Rhat, 2)
  )

# make flextable
ft <- flextable(out_table)

# save to Word
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "output tables/model_summary_2013-2024.docx")

# get contrasts for interactions of categorical predictors

# Get estimated marginal medians
emm_site_stage <- emmeans(m_full_area_stage, ~ site * stage)
emm_stage_sex <- emmeans(m_full_area_stage, ~ stage * sex)
emm_site_sex <- emmeans(m_full_area_stage, ~ site * sex)

emm_site_stage
# site stage emmean lower.HPD upper.HPD
# 0    A       3.59      3.26      3.93
# 1    A       3.97      3.65      4.31
# 0    P       3.55      3.21      3.89
# 1    P       3.69      3.38      4.04
# 
# Results are averaged over the levels of: sex 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

emm_stage_sex
# stage sex emmean lower.HPD upper.HPD
# A     F     3.92      3.57      4.23
# P     F     3.57      3.24      3.91
# A     M     3.64      3.32      3.99
# P     M     3.68      3.34      4.01
# 
# Results are averaged over the levels of: site 
# Point estimate displayed: median 
# HPD interval probability: 0.95

emm_site_sex

# site sex emmean lower.HPD upper.HPD
# 0    F     3.63      3.29      3.95
# 1    F     3.87      3.53      4.19
# 0    M     3.52      3.19      3.85
# 1    M     3.80      3.47      4.13
# 
# Results are averaged over the levels of: stage 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

# Simple pairwise comparisons
contrast(emm_site_stage, method = "pairwise", by = "site")
# site = 0:
#   contrast estimate lower.HPD upper.HPD
# A - P      0.0311   -0.0719     0.131
# 
# site = 1:
#   contrast estimate lower.HPD upper.HPD
# A - P      0.2760    0.2050     0.356
# 
# Results are averaged over the levels of: sex 
# Point estimate displayed: median 
# HPD interval probability: 0.95 


contrast(emm_stage_sex, method = "pairwise", by = "sex")
# sex = F:
#   contrast estimate lower.HPD upper.HPD
# A - P      0.3440     0.254    0.4392
# 
# sex = M:
#   contrast estimate lower.HPD upper.HPD
# A - P     -0.0359    -0.144    0.0753
# 
# Results are averaged over the levels of: site 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

contrast(emm_site_sex, method = "pairwise", by = "site")

# site = 0:
#   contrast estimate lower.HPD upper.HPD
# F - M      0.1082    0.0242     0.191
# 
# site = 1:
#   contrast estimate lower.HPD upper.HPD
# F - M      0.0626    0.0165     0.111
# 
# Results are averaged over the levels of: stage 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

# 1.3) Model checks -------------------------------------------------------

# look for stationarity and mixture
plot(m_full_area_stage)
# check if draws fit the data (dark blue and light blue lines should overlap if the model is appropriate)
pp_check(m_full_area_stage)
# both look good

# 1.4) Plot conditional effects -------------------------------------------

plot_2013_2024 <- plot(conditional_effects(m_full_area_stage))

fig <- ggarrange(
  plot_2013_2024$`site:stage`+
    theme_bw()+
    scale_color_manual(values = c("#4bc490", "#9b5d7d"), labels = c("A" = "Adult", "P" = "Juvenile"), name="Age category")+
    scale_x_discrete(labels = c("0" = "Less disturbed", "1" = "More disturbed")) +
    ylab("Ln fecal glucocorticoid\n metabolites (ng/g feces)")+
    guides(fill = "none")+
    xlab("Site"),
  

  plot_2013_2024$`mass_div_100:sex`+
    theme_bw()+
    scale_color_manual(values = c("#d67627", "#83633e"), labels = c("M" = "Male", "F" = "Female"), name="Sex")+
    scale_fill_manual(values = c("#d67627", "#83633e"), labels = c("M" = "Male", "F" = "Female"), name="Sex")+
    ylab("")+
    xlab("Body mass [g]")+
    scale_x_continuous(breaks = c(3, 6, 9), 
                       labels = c("300", "600", "900")),
  
  plot_2013_2024$`stage:sex`+
    theme_bw()+
    scale_color_manual(values = c("#d67627", "#83633e"), labels=c("M"="Male", "F"="Female"), name="Sex")+
    ylab("")+
    scale_x_discrete(labels=c("Adult", "Juvenile"), name="Age category")+
    guides(fill = "none"),
  
  labels = c("a", "b", "c"),
  ncol = 3, nrow = 1,
  widths = c(1, 1),   
  heights = c(1, 1),
  common.legend = FALSE
  
  
  
)


# display figure
fig

# save to file
ggsave("figures/model 2013-2024.png", fig, width = 12, height = 4, dpi = 300)


# supplementary figure with the rest of the panels
fig_si <- ggarrange(
  plot_2013_2024$day.s+
    theme_bw()  
  +ylab("Ln fecal glucocorticoid metabolites (ng/g feces)")+
    xlab("Day"),

    plot_2013_2024$site+
    theme_bw()+
    ylab("")+
    xlab("Site")+
    scale_x_discrete(labels = c("0" = "Less disturbed", "1" = "More disturbed")),
  
  plot_2013_2024$stage+
    theme_bw()+
    ylab("")+
    xlab("Age category")+
    scale_x_discrete(labels=c("Adult", "Juvenile"), name="Age category")+
    guides(fill = "none"),
  
  
  plot_2013_2024$sex+
    theme_bw()+
    ylab("Ln fecal glucocorticoid metabolites (ng/g feces)")+
    scale_x_discrete(labels=c("Female", "Male"), name="Sex"),
  
  
  plot_2013_2024$mass_div_100+
    theme_bw()+
    ylab("")+
    xlab("Body mass [g]")+
    scale_x_continuous(breaks = c(3, 6, 9), 
                       labels = c("300", "600", "900")),
  
  
  plot_2013_2024$`site:sex`+
    theme_bw()+
    scale_color_manual(values = c("#d67627", "#83633e"), labels=c("M"="Male", "F"="Female"), name="Sex")+
    ylab("")+
    scale_x_discrete(labels = c("0" = "Less disturbed", "1" = "More disturbed"),name="Site")+
    guides(fill = "none")+
  theme(
    legend.position = c(0.75, 0.15),   # move legend inside
    legend.background = element_rect(fill = alpha("white", 0.7))),
  
  
  labels = c("a", "b", "c", "d", "e", "f"),
  ncol = 3, nrow = 2,
  widths = c(1, 1),   
  heights = c(1, 1),
  common.legend = FALSE
  
    
)

fig_si

ggsave("figures/model 2013-2024_SI.png", fig_si, width = 12, height = 8, dpi = 300)



# 1.5) Calculate repeatability --------------------------------------------

# to calculate repeatability, we more year to be a fixed effect, then extract from the posterior and calculate the intra-class correlation coefficient (icc) for uid and area
# we do this separately for the two sites

# some samples are duplicated per day per sample, we take the mean since otherwise we cannot estimate auto-correlation among samples within individuals
CORT_full_site0 <- CORT_full %>%
  filter(site == 0) %>%
  group_by(uid, day.s) %>%
  summarize(
    lnCort = mean(lnCort),
    stage = first(stage),
    sex = first(sex),
    mass_div_100 = first(mass_div_100),
    year = first(year),
    area = first(area)
  ) %>%
  ungroup()


 # Site 0 = less disturbed
m_full_area_stage_0 <- brm(
  formula = lnCort ~ scale(day.s) + stage + sex*scale(mass_div_100) + stage*sex + year +
    (1 | uid) + (1 | area)+
    ar(time = day.s, gr = uid), # this controls for auto-correlation for unequal time between sample collection
  family = gaussian(),
  data = CORT_full_site0,
  cores = 4, chains = 4, iter = 4000
)

# Extract posterior draws
post_0 <- as_draws_df(m_full_area_stage_0)

# Variance components
V_uid  <- post_0$sd_uid__Intercept^2
V_area <- post_0$sd_area__Intercept^2
V_res  <- post_0$sigma^2

# Total variance
V_total <- V_uid + V_area  + V_res

# Adjusted repeatabilities
R_uid  <- V_uid  / V_total
R_area <- V_area / V_total

# Summarize
summarize_icc <- function(x) {
  c(mean = mean(x),
    median = median(x),
    l95 = quantile(x, 0.025),
    u95 = quantile(x, 0.975))
}

site_0 <- list(
  R_uid  = summarize_icc(R_uid),
  R_area = summarize_icc(R_area)
)


# $R_uid
# mean     median   l95.2.5%  u95.97.5% 
#   0.17534301 0.17916269 0.03402815 0.29264967 
# 
# $R_area
# mean       median     l95.2.5%    u95.97.5% 
#   0.0104255126 0.0066781736 0.0000303506 0.0427367600 


# Site 1 = more disturbed

CORT_full_site1 <- CORT_full %>%
  filter(site == 1) %>%
  group_by(uid, day.s) %>%
  summarize(
    lnCort = mean(lnCort),
    stage = first(stage),
    sex = first(sex),
    mass_div_100 = first(mass_div_100),
    year = first(year),
    area = first(area)
  ) %>%
  ungroup()



m_full_area_stage_1 <- brm(
  formula = lnCort ~ scale(day.s) + stage + sex*scale(mass_div_100) + stage*sex + year +
    (1 | uid) + (1 | area)+
    ar(time = day.s, gr = uid), # this controls for auto-correlation for unequal time between sample collection
  family = gaussian(),
  data = CORT_full_site1,
  cores = 4, chains = 4, iter = 4000
)

# Extract posterior draws
post_1 <- as_draws_df(m_full_area_stage_1)

# Variance components
V_uid  <- post_1$sd_uid__Intercept^2
V_area <- post_1$sd_area__Intercept^2
V_res  <- post_1$sigma^2

# Total variance
V_total <- V_uid + V_area +  V_res

# Adjusted repeatabilities
R_uid  <- V_uid  / V_total
R_area <- V_area / V_total

# Summarize
summarize_icc <- function(x) {
  c(mean = mean(x),
    median = median(x),
    l95 = quantile(x, 0.025),
    u95 = quantile(x, 0.975))
}

site_1 <- list(
  R_uid  = summarize_icc(R_uid),
  R_area = summarize_icc(R_area)
)

# $R_uid
# mean     median   l95.2.5%  u95.97.5% 
#   0.05995037 0.05931702 0.02743090 0.09598078 
# 
# $R_area
# mean     median   l95.2.5%  u95.97.5% 
#   0.03496694 0.03259797 0.01385301 0.07055099


# plots


icc_site0 <- tibble(
  effect = c("uid", "area"),
  mean = c(site_0$R_uid[1], site_0$R_area[1]),
  l95 = c(site_0$R_uid[3], site_0$R_area[3]),
  u95 = c(site_0$R_uid[4], site_0$R_area[4]),
  site = "Site 0"
)

icc_site1 <- tibble(
  effect = c("uid", "area"),
  mean = c(site_1$R_uid[1], site_1$R_area[1]),
  l95 = c(site_1$R_uid[3], site_1$R_area[3]),
  u95 = c(site_1$R_uid[4], site_1$R_area[4]),
  site = "Site 1"
)

icc_all <- bind_rows(icc_site0, icc_site1)


repeatability <- ggplot(icc_all, aes(x = effect, y = mean, col = site)) +
  geom_pointrange(aes(ymin = l95, ymax = u95), position = position_dodge(width = 0.5), size = 1) +
  ylab("Adjusted repeatability (ICC)") +
  xlab("Random effect") +
  theme_bw() +
  scale_color_manual(values = c("Site 0" = "#404788FF", "Site 1" = "#57B6D8FF"), labels =c("Site 0"= "Less disturbed", "Site 1"="More disturbed"), name="Site")+
  scale_x_discrete(labels = c("Trapping area", "Squirrel identity")) 

repeatability

ggsave("figures/repeatability.png", repeatability, width = 5, height = 4, dpi = 300)


# how does unequal sampling among indidiauls affect repeatabilties? Mixed models partially account for this via partial pooling, but we can test whetehr indivdiuals with more samplse have systematically different cort levels:

n_samples <- table(CORT_full$uid)

mean_cort <- CORT_full %>%
  group_by(uid) %>%
  summarize(mean_cort = mean(lnCort),
            n = n())

cor.test(mean_cort$mean_cort, mean_cort$n)

# Pearson's product-moment correlation
# 
# data:  mean_cort$mean_cort and mean_cort$n
# t = 2.3409, df = 1243, p-value = 0.0194
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.01073251 0.12136060
# sample estimates:
#        cor 
# 0.06625015 

# a minimally positive effect, but likely driven by really large sample size. 


# 2) 2018-2024 data within disturbed site including area-specific disturbance through humans and dogs ----------------------------------------------------------------------

CORT_2018_2024_disturbance <- read.csv("Data/Data_for_2018_2024_model_disturbed_site.csv")


# 2.1) VIFs ---------------------------------------------------------------


# calculate vifs

# note that we scale continuous variables. For dog and human disturbance, we just center them around 0 since they are already scaled between 0 and 1

lm_crow18_24 <- lm(
  lnCort ~ scale(day.s) +
    sex +
    scale(mass_div_100) +
    stage +
    scale(human_rate_scaled, scale=F) +
    scale(dog_rate_scaled, scale=F),
  data = CORT_2018_2024_disturbance
)

# compute VIFs
vif_vals <- vif(lm_crow18_24)
vif_vals
# scale(day.s)                                 sex 
# 1.073919                            1.046828 
# scale(mass_div_100)                               stage 
# 2.843287                            2.915682 
# scale(human_rate_scaled, scale = F)   scale(dog_rate_scaled, scale = F) 
# 2.272882                            2.292473               7.287381 


# convert to data frame
vif_df <- data.frame(
  Term = names(vif_vals),
  VIF = round(as.numeric(vif_vals), 2),
  row.names = NULL
)

# make flextable
ft <- flextable(vif_df)

# export to Word
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "output tables/vif_table_2018-2024_crow.docx")


# 2.2) Run Bayesian regression-------------------------------------------------------------------


# scale variables as above
m_crow18_24 <- brm(
  formula = lnCort ~ scale(day.s) +
    sex * scale(mass_div_100) +
    sex * stage +
    scale(human_rate_scaled, scale=FALSE) * sex +
    scale(dog_rate_scaled, scale=FALSE) * sex +
    scale(mass_div_100) * stage +
    scale(mass_div_100) * scale(human_rate_scaled, scale=FALSE) +
    scale(mass_div_100) * scale(dog_rate_scaled, scale=FALSE) +
    scale(human_rate_scaled, scale=FALSE) * stage +
    scale(dog_rate_scaled, scale=FALSE) * stage +
    scale(dog_rate_scaled, scale=FALSE) * scale(human_rate_scaled, scale=FALSE) +
    (1 | uid) +
    (1 | area) +
    (1 | year),
  family = gaussian(),
  data = CORT_2018_2024_disturbance,
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95, max_treedepth = 15) # helps with complex models
)


#save(m_crow18_24, file="model output/m_disturbance_Crow.RDA")
load("model output/m_disturbance_Crow.RDA")

# summary output - considered as having an effect if CIs do not span 0
summary(m_crow18_24)

# extract fixed-effect summary from the model
sum_df <- as.data.frame(summary(m_crow18_24)$fixed)
sum_df$term <- rownames(sum_df)


# save the summary as a word table:
out_table <- sum_df %>%
  transmute(
    Term = term,
    Estimate = round(Estimate, 2),
    CI_low = round(`l-95% CI`, 2),
    CI_high = round(`u-95% CI`, 2),
    Rhat = round(Rhat, 2)
  )

# make flextable
ft <- flextable(out_table)

# save to Word
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "output tables/model_summary_2018-2024_Crow.docx")

# For human rate by stage
emm_human <- emtrends(m_crow18_24, ~ stage, var = "human_rate_scaled")
# stage human_rate_scaled.trend lower.HPD upper.HPD
# A                      -0.662    -0.947    -0.393
# P                       0.865     0.500     1.228
# 
# Results are averaged over the levels of: sex 
# Point estimate displayed: median 
# HPD interval probability: 0.95 
contrast(emm_human, "pairwise")
# contrast estimate lower.HPD upper.HPD
# A - P       -1.53     -1.99     -1.03
# 
# Results are averaged over the levels of: sex 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

emm_dog <- emtrends(m_crow18_24, ~ stage, var = "dog_rate_scaled")

# stage dog_rate_scaled.trend lower.HPD upper.HPD
# A                     0.557     0.284    0.8158
# P                    -0.268    -0.614    0.0532
# 
# Results are averaged over the levels of: sex 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

contrast(emm_dog, "pairwise")
# stage dog_rate_scaled.trend lower.HPD upper.HPD
# A                     0.557     0.284    0.8158
# P                    -0.268    -0.614    0.0532
# 
# Results are averaged over the levels of: sex 
# Point estimate displayed: median 
# HPD interval probability: 0.95  


# 2.3) Model checks -------------------------------------------------------


plot(m_crow18_24)
pp_check(m_crow18_24)
# both look okay


# 2.4) Plot conditional effects -------------------------------------------

# we want to plot for disturbance values 0.1, 0.5 and 0.9
ce_disturbance <- conditional_effects(
  m_crow18_24,
  effects = c(
    "human_rate_scaled",
    "dog_rate_scaled",
    "mass_div_100:human_rate_scaled",
    "mass_div_100:dog_rate_scaled",
    "human_rate_scaled:stage",
    "dog_rate_scaled:stage",
    "human_rate_scaled:dog_rate_scaled"
  ),
  int_conditions = list(
    human_rate_scaled = setNames(c(0.1, 0.5, 0.9), c("0.1","0.5","0.9")),
    dog_rate_scaled   = setNames(c(0.1, 0.5, 0.9), c("0.1","0.5","0.9"))
  )
)

plot_2018_2024_disturbance <- plot(ce_disturbance)



ab <- ggarrange(
  plot_2018_2024_disturbance$human_rate_scaled+
    theme_bw()+
    ylab("Ln fecal glucocorticoid \nmetabolites (ng/g feces)")+
    xlab("Human presence\n [standardized]")+
    ylim(c(2.7,5.5)), 
  plot_2018_2024_disturbance$dog_rate_scaled+
    theme_bw()+
    ylab("")+
    xlab("Dog presence\n [standardized]")+
    ylim(c(2.7,5.5)),
  ncol = 2, nrow = 1,
  labels = c("a", "b")
)

fig <- ggarrange(
  ab, 
  plot_2018_2024_disturbance$`human_rate_scaled:stage`+
    theme_bw()+
    scale_color_manual(values = c("#4bc490", "#9b5d7d" ), labels = c("A" = "Adult", "P" = "Juvenile"), name="Age category")+
    scale_fill_manual(values = c("#4bc490", "#9b5d7d" ), labels = c("A" = "Adult", "P" = "Juvenile"), name="Age category")+
    ylab("")+
    xlab("Human presence\n [standardized]")+
    theme(
      legend.position = c(0.25, 0.79),   # move legend inside
      legend.background = element_rect(fill = alpha("white", 0.7))
    )+
    ylim(c(2.7,5.5)), 
  plot_2018_2024_disturbance$`dog_rate_scaled:stage`+
    theme_bw()+
    scale_color_manual(values = c("#4bc490", "#9b5d7d" ), labels = c("A" = "Adult", "P" = "Juvenile"), name="Age category")+
    scale_fill_manual(values = c("#4bc490", "#9b5d7d" ), labels = c("A" = "Adult", "P" = "Juvenile"), name="Age category")+
    ylab("")+
    xlab("Dog presence\n [standardized]")+
    theme(
      legend.position = c(0.25, 0.79),   # move legend inside
      legend.background = element_rect(fill = alpha("white", 0.7))
    )+
    ylim(c(2.7,5.5)),
  plot_2018_2024_disturbance$`mass_div_100:human_rate_scaled`+
    theme_bw()+
    scale_x_continuous(breaks = c(3, 6, 9), 
                       labels = c("300", "600", "900"))+
    xlab("Mass [g]")+
    ylab("Ln fecal glucocorticoid\n metabolites (ng/g feces)")+
    scale_color_manual(values = c("#a64d6c", "#a09446", "#7f64b9"), name="Human presence\n [standardized]")+
    scale_fill_manual(values = c("#a64d6c", "#a09446", "#7f64b9"), name="Human presence\n [standardized]")+
    theme(
      legend.position = c(0.8, 0.75),   # move legend inside
      legend.background = element_rect(fill = alpha("white", 0.7))
    )+
    ylim(c(2.7,5.5)), 
  plot_2018_2024_disturbance$`mass_div_100:dog_rate_scaled`+
    theme_bw()+
    scale_x_continuous(breaks = c(3, 6, 9), 
                       labels = c("300", "600", "900"))+
    xlab("Mass [g]")+
    ylab("")+
    scale_color_manual(values = c("#a64d6c", "#a09446", "#7f64b9"), name="Dog presence\n [standardized]")+
    scale_fill_manual(values = c("#a64d6c", "#a09446", "#7f64b9"), name="Dog presence\n [standardized]")+
    theme(
      legend.position = c(0.8, 0.75),   # move legend inside
      legend.background = element_rect(fill = alpha("white", 0.7))
    )+
    ylim(c(2.7,5.5)),
  plot_2018_2024_disturbance$`human_rate_scaled:dog_rate_scaled`+
    theme_bw()+
    ylab("")+
    xlab("Human presence\n [standardized]")+
    scale_color_manual(values = c("#a64d6c", "#a09446", "#7f64b9"), name="Dog presence\n [standardized]")+
    scale_fill_manual(values = c("#a64d6c", "#a09446", "#7f64b9"), name="Dog presence\n [standardized]")+
    theme(
      legend.position = c(0.8, 0.75),   # move legend inside
      legend.background = element_rect(fill = alpha("white", 0.7))
    )+
    ylim(c(2.7,5.5)),
  ncol = 3, nrow = 2,
  widths = c(1.1, 1, 1),
  labels=c("","c", "d", "e", "f", "g")
)

fig

# save to file
ggsave("figures/model 2018-2024_crow.png", fig, width = 10.8, height = 6.5, dpi = 300)


# 3) Dispersal ------------------------------------------------------------


CORT_dispersal_move <- read.csv("Data/Data_for_dispersal_model.csv", h=T)


length(unique(CORT_dispersal_move$uid[CORT_dispersal_move$natal_site=="low disturbance"]))
# 17
length(unique(CORT_dispersal_move$uid[CORT_dispersal_move$natal_site=="moderate disturbance"]))
# 19


# 3.1) VIFs ---------------------------------------------------------------


# simple linear model with the same fixed structure as the full model and scaled continuous variables
m_lm <- lm(
  lnCort ~ scale(day.s) + sex + scale(mass_div_100) + stage + 
    dispersal_status +  natal_site,
  data = CORT_dispersal_move
)


# compute VIFs
vif_vals <- vif(m_lm)

vif_vals

# scale(day.s)                 sex scale(mass_div_100)               stage    dispersal_status          natal_site 
# 1.133580            1.400278            2.099603            1.821720            1.245193            1.309120 

vif_df <- as.data.frame(round(vif_vals, 2))

vif_df <- data.frame(Predictor = rownames(vif_df), vif_df, row.names = NULL)


# make flextable
ft <- flextable(vif_df)


# export to Word
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "output tables/vif_table_dispersal.docx")



# 3.2) Run model ----------------------------------------------------------


m_disperse_move <- brm(
  formula = lnCort ~ scale(day.s) + sex + scale(mass_div_100) + stage + 
    dispersal_status * natal_site +
    (1 | uid) + (1 | area) + (1 | year),
  family = student(), # switching from gaussian to student, since the model struggled to deal with tails. 
  data = CORT_dispersal_move,
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95) # helps with convergence
)

# check model with minimum age instead of dispersal (in response to reviewer question)
m_disperse_move2 <- brm(
  formula = lnCort ~ scale(day.s) + sex + scale(mass_div_100) + stage + scale(min_age_master)* natal_site +
    (1 | uid) + (1 | area) + (1 | year),
  family = student(), # switching from gaussian to student, since the model struggled to deal with tails. 
  data = CORT_dispersal_move,
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95) # helps with convergence
)

# and compare with loo
loo(m_disperse_move, m_disperse_move2)

# Model comparisons:
#   elpd_diff se_diff
# m_disperse_move   0.0       0.0   
# m_disperse_move2 -4.2       3.1
# Model with dispersal performs better than that with age, although relatively uncertain


#save(m_disperse_move, file="model output/m_dispersal.RDA")
load("model output/m_dispersal.RDA")

# summary output - considered as having an effect if CIs do not span 0
summary(m_disperse_move)

# extract fixed-effect summary from the model
sum_df <- as.data.frame(summary(m_disperse_move)$fixed)
sum_df$term <- rownames(sum_df)


# save the summary as a word table:
out_table <- sum_df %>%
  transmute(
    Term = term,
    Estimate = round(Estimate, 2),
    CI_low = round(`l-95% CI`, 2),
    CI_high = round(`u-95% CI`, 2),
    Rhat = round(Rhat, 2)
  )

# make flextable
ft <- flextable(out_table)

# save to Word
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "output tables/model_dispersal.docx")


# for categorical interactions, extract posterior means to calculate contrasts


# Get estimated marginal means
emm <- emmeans(m_disperse_move, ~ dispersal_status * natal_site)

emm

# dispersal_status natal_site           emmean lower.HPD upper.HPD
# before           low disturbance        3.66      3.27      4.08
# post             low disturbance        3.89      3.52      4.29
# before           moderate disturbance   3.81      3.45      4.20
# post             moderate disturbance   3.49      3.10      3.86
# 
# Results are averaged over the levels of: sex, stage 
# Point estimate displayed: median 
# HPD interval probability: 0.95 

# Simple pairwise comparisons within each natal site
contrast(emm, method = "pairwise", by = "natal_site")

# NOTE: this is before minus post, so a negative value means an increase from before to post.

# natal_site = low disturbance:
#   contrast      estimate lower.HPD upper.HPD
# before - post   -0.231   -0.5194    0.0667
# 
# natal_site = moderate disturbance:
#   contrast      estimate lower.HPD upper.HPD
# before - post    0.322    0.0524    0.5897
# 
# Results are averaged over the levels of: sex, stage 
# Point estimate displayed: median 
# HPD interval probability: 0.95
# 3.3) Model checks -------------------------------------------------------

plot(m_disperse_move)
pp_check(m_disperse_move)
# looks okay

# 3.4) Plot conditional effects -------------------------------------------

plot_dispersal <- plot(conditional_effects(m_disperse_move))



fig <- ggarrange(#plot_dispersal$natal_site+
                   # theme_bw()+
                   # xlab("Natal site")+
                   # ylab("Ln fecal glucocorticoid metabolites (ng/g feces)")+
                   # scale_x_discrete(labels=c("Less disturbed", "More disturbed")),
                   #
                   #
                 plot_dispersal$dispersal_status+
                   theme_bw()+
                   xlab("Dispersal status")+
                   ylab("Ln fecal glucocorticoid\n metabolites (ng/g feces)")+
                   scale_x_discrete(labels=c("before", "after")),

                 plot_dispersal$`dispersal_status:natal_site`+
                   theme_bw()+
                   scale_color_manual(values = c("#404788FF", "#57B6D8FF"), labels=c("low disturbance"= "Less disturbed", "moderate disturbance"="More disturbed"), name="Site")+
                   guides(fill="none")+
                   ylab("")+
                   scale_x_discrete(labels=c("before", "after"))+
                   xlab("Dispersal status") +
                   theme(
                     legend.position = c(1.25, 0.5),   # move legend inside
                     legend.background = element_rect(fill = alpha("white", 0.7)),
                     plot.margin = margin(5, 100, 5, 5)
                   ),
                 labels = c("a", "b"),
                 ncol = 2, nrow = 1,
                 widths = c(0.9, 1.3),
                 heights = c(1, 1),
                 common.legend = F
)


fig


# save to file
ggsave("figures/model dispersal.png", fig, width = 8, height = 4, dpi = 300)

