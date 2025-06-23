#sex differences across reefs sites of Panama
setwd("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024")

#for plots
library(ggplot2)
library(ggh4x)
library(tidyr)
#for checking model/ANOVA assumptions
library(performance) 
#for mapping of sites
library(ggmap)
#For temp comparisons across sites
library(dplyr)
library(zoo)
library(lubridate)
#### Temperature across sites ####
# Load packages
library(dplyr)
library(ggplot2)
library(lubridate)
#for making figures
library(cowplot)
library(grid)
library(gridExtra)

#### Temperature analysis across sites ####
Temp <- read.csv("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/Temperature_BocasSites_long_2024_2025.csv")
#Raw Temp data, only for the months of Sept 2024 - January 2025

# Convert datetime to POSIXct if not already so dates can be read
Temp$Dates_Time <- parse_date_time2(Temp$Dates_Time, c("%m/%d/%y %H:%M"), exact = TRUE)
Temp$Dates_Time <- as.POSIXct(Temp$Dates_Time)

unique(Temp$Site)
#reorder Sites variable for figures
f=c('Sid Ciudad','Punta Caracol', 'Cristobal Island', 'Hospital Point', 'Punta Donato', 'STRI Point')
Temp <- within(Temp, Site<- factor(Site, levels=f))


Temp <- Temp %>%
  mutate(Site = recode(Site,
                       "Cristobal Island" = "CI",
                       "Hospital Point" = "HP",
                       "Punta Caracol" = "PC",
                       "Punta Donato" = "PD",
                       "Sid Ciudad" = "SC",
                       "STRI Point" = "SP"))

#site colors
SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96","#AABAEE", "#AD961D" )#)"#CFD8F5","#768EA1

#Box plots
ggplot(Temp, aes(x=Site, y=Temperature, color = Site, fill = Site)) +
  scale_fill_manual(values=SiteColors)+
  scale_color_manual(values=SiteColors)+
  geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.35, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, linewidth = 1, color ="black", alpha = 0.8)+
  theme_classic() +
  ylab("Temperature (°C)")+
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#line graph
#show temperatures together by month and day so the HP data is with other temp data
Temp$month_day <- as.Date(format(Temp$Dates_Time, "2020-%m-%d"))
Temp$plot_date <- as.Date(ifelse(format(Temp$month_day, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08"),
                                        format(Temp$month_day, "2021-%m-%d"),
                                        format(Temp$month_day, "2020-%m-%d")))
ggplot(Temp, aes(x=plot_date, y=Temperature, color=Site)) +
  scale_color_manual(values=SiteColors)+
  geom_line(size = 1) +
  labs(x = "Date", y = "Raw Temperature (°C)", color = "Site") +
  theme_classic()+
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))



#### Calculate daily means per site
daily_means <- Temp %>%
  mutate(date = as.Date(Dates_Time)) %>%
  group_by(Site, date) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE), .groups = "drop")

# Calculate overall mean and standard error per site
site_summary <- daily_means %>%
  group_by(Site) %>%
  summarise(
    site_mean_temp = mean(mean_temp, na.rm = TRUE),
    site_se_temp = sd(mean_temp, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# View site-wise means and SEs
print(site_summary)

# A tibble: 6 × 3
# Site  site_mean_temp  site_se_temp
# <fct>          <dbl>        <dbl>
# 1 SC              29.9       0.107 
# 2 PC              29.7       0.0934
# 3 CI              29.8       0.108 
# 4 HP              30.2       0.0829
# 5 PD              29.8       0.103 
# 6 SP              29.4       0.0971

# Plot daily mean temperature
#show temperatures together by month and day so the HP data is with other temp data
daily_means$month_day <- as.Date(format(daily_means$date, "2020-%m-%d"))
daily_means$plot_date <- as.Date(ifelse(format(daily_means$month_day, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08"),
                                        format(daily_means$month_day, "2021-%m-%d"),
                                        format(daily_means$month_day, "2020-%m-%d")))
#line graph
ggplot(daily_means, aes(x = plot_date, y = mean_temp, color = Site)) +
  geom_line(size = 1) +
  scale_color_manual(values=SiteColors)+
  labs(x = "Date", y = "Mean Daily Temperature (°C)", color = "Site") +
  theme_classic()+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 16, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#boxplot
ggplot(daily_means, aes(x = Site, y = mean_temp, color = Site, fill = Site)) +
  scale_fill_manual(values=SiteColors)+
  scale_color_manual(values=SiteColors)+
  geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color = "black", linewidth = 1.5, alpha = 0.6)+
  labs(x = "Date", y = "Mean Daily Temperature (°C)", color = "Site") +
  theme_classic()+
  theme(legend.position="none", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 16, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

###Significance
#assumption checks for ANOVA
library("performance")
m <- lm(mean_temp ~ Site, data = daily_means)
check_model(m)#bimodal with bad residuals

hist(daily_means$mean_temp)#looks bi-modal
shapiro.test(daily_means$mean_temp)#p-value = 2.164e-13, not normal

#try transforming
trans_daily_means = log(daily_means$mean_temp)#log
trans_daily_means = exp(daily_means$mean_temp)#antilog
#trans_daily_means = 1/(daily_means$mean_temp)#reciprocal
#assumption checks
m <- lm(trans_daily_means ~ Site, data = daily_means)
check_model(m)#log-better, reciprocal-worse, antilog-right skew - less bimodal

hist(trans_daily_means)#log looks much better, more normal; recip - way worse, antilog - right skew
shapiro.test(trans_daily_means)#log -  p-value = 1.185e-13, not normal, antilog - p-value < 2.2e-16

#Anova - tried initially, but didn't pass assumptions 
#aov1 = aov(trans_daily_means ~ Site, data = daily_means)
#summary(aov1) #same trends of significance whether untransformed or log/antilog


#not normal distributions, use non-parametric

#Kruskal-Wallis
kruskal.test(daily_means$mean_temp~daily_means$Site)
# Kruskal-Wallis rank sum test
# data:  daily_means$mean_temp by daily_means$Site
# Kruskal-Wallis chi-squared = 37.426, df = 5, p-value = 4.92e-07

library("FSA")
dunnTest(mean_temp~Site, data=daily_means, method="bonferroni")
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Bonferroni method.
# 
# Comparison          Z      P.unadj        P.adj
# 1     CI - HP -2.2060450 2.738085e-02 4.107128e-01
# 2     CI - PC  0.7841309 4.329633e-01 1.000000e+00
# 3     HP - PC  2.9784986 2.896643e-03 4.344965e-02
# 4     CI - PD  0.3380480 7.353270e-01 1.000000e+00
# 5     HP - PD  2.5313087 1.136378e-02 1.704567e-01
# 6     PC - PD -0.4434514 6.574393e-01 1.000000e+00
# 7     CI - SC -0.4167110 6.768898e-01 1.000000e+00
# 8     HP - SC  1.7841388 7.440111e-02 1.000000e+00
# 9     PC - SC -1.1969217 2.313371e-01 1.000000e+00
# 10    PD - SC -0.7516375 4.522691e-01 1.000000e+00
# 11    CI - SP  3.7081710 2.087616e-04 3.131424e-03
# 12    HP - SP  5.9382978 2.879966e-09 4.319949e-08
# 13    PC - SP  2.8970401 3.767016e-03 5.650524e-02
# 14    PD - SP  3.3459939 8.198820e-04 1.229823e-02
# 15    SC - SP  4.1209618 3.772940e-05 5.659410e-04

# #Do non-parametric instead 
# #Aligned Rank Transform (ART) --- same as Kruskal-Wallis results -- used Kruskal instead do to concerns about robustness
# library(ARTool)
# 
# # Apply the ART procedure
# art_model <- art(mean_temp ~ Site, data = daily_means)
# # Conduct ANOVA on the ART model
# anova_art <- anova(art_model)
# print(anova_art)


#### Calculate daily temp range (max - min) by site ####
daily_range <- Temp %>%
  mutate(date = as.Date(Dates_Time)) %>%
  group_by(Site, date) %>%
  filter(!all(is.na(Temperature))) %>%  # remove groups where all temperatures are NA
  summarise(temp_range = max(Temperature, na.rm = TRUE) - min(Temperature, na.rm = TRUE),
            .groups = "drop")

# Calculate overall mean and SE of daily temperature range per site
range_summary <- daily_range %>%
  group_by(Site) %>%
  summarise(
    mean_temp_range = mean(temp_range, na.rm = TRUE),
    se_temp_range = sd(temp_range, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# View summary
print(range_summary)

# A tibble: 6 × 3
# Site  mean_temp_range se_temp_range
# <fct>           <dbl>         <dbl>
# 1 SC              1.45         0.0497
# 2 PC              1.29         0.0396
# 3 CI              1.05         0.0291
# 4 HP              1.08         0.0337
# 5 PD              0.767        0.0203
# 6 SP              0.733        0.0238

# Plot the daily temperature range
#boxplot
ggplot(daily_range, aes(x = Site, y = temp_range, color = Site, fill = Site)) +
  scale_fill_manual(values=SiteColors)+
  scale_color_manual(values=SiteColors)+
  geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.35, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1.5, color ="black", linewidth = 1.5, alpha = 0.6)+
  labs(x = "Date", y = "Daily Temperature Range (°C)", color = "Site") +
  theme_classic()+
  theme(legend.position="none", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 16, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#show temperatures together by month and day so the HP data is with other temp data in comparisons
daily_range$month_day <- as.Date(format(daily_range$date, "2020-%m-%d"))
daily_range$plot_date <- as.Date(ifelse(format(daily_range$month_day, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08"),
                                        format(daily_range$month_day, "2021-%m-%d"),
                                        format(daily_range$month_day, "2020-%m-%d")))
#line - check to make sure nothing weird
ggplot(daily_range, aes(x = plot_date, y = temp_range, color = Site)) +
  scale_color_manual(values = SiteColors) +
  geom_line(size = 1) +
  labs(x = "Month-Day", y = "Daily Temperature Range (°C)", color = "Site") +
  scale_x_date(date_labels = "%b %d", breaks = "1 month") +
  theme_classic() +
  theme(legend.position = "top",
    legend.title = element_text(size = 14, face = "bold", color = "black"), 
    legend.text = element_text(size = 14, face = "bold", color = "black"),
    axis.title.x = element_blank(),
    axis.text = element_text(color = "black", size = 17, face = "bold"),
    axis.title.y = element_text(size = 16, color = "black", face = "bold"),
    axis.ticks = element_line(color = "black"))


####Significance
#check anova assumptions
m <- lm(temp_range ~ Site, data = daily_range)
check_model(m)#says decent

hist(daily_range$temp_range)#looks a lil right skewed
shapiro.test(daily_range$temp_range)#p-value < 2.2e-16, not normal

#try transforming
trans_daily_range = log(daily_range$temp_range)
#check anova assumptions
m <- lm(trans_daily_range ~ Site, data = daily_range)
check_model(m)#looks better
hist(trans_daily_range)#looks normal
shapiro.test(trans_daily_range)#p-value = 0.09152, normal

#by logger/Site and daily temperature range
#one-way ANOVA
aov2 = aov(trans_daily_range ~ Site, data = daily_range)
summary(aov2)
#               Df Sum Sq Mean Sq F value Pr(>F)    
# Site          5  47.57   9.513   70.92 <2e-16 ***
# Residuals   803 107.71   0.134                 

TukeyHSD(aov2)
#almost all sites sign diff from each other except:Sid Ciudad-Punta Caracol, Hospital Point-Cristobal Island, & STRI Point-Punta Donato
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = trans_daily_range ~ Site, data = daily_range)
# 
# $Site
# diff        lwr         upr     p adj
# PC-SC -0.09597296 -0.2240529  0.03210701 0.2675209
# CI-SC -0.30043373 -0.4273088 -0.17355868 0.0000000
# HP-SC -0.28773921 -0.4130520 -0.16242644 0.0000000
# PD-SC -0.60613209 -0.7339647 -0.47829947 0.0000000
# SP-SC -0.67026166 -0.7983416 -0.54218168 0.0000000
# CI-PC -0.20446077 -0.3325407 -0.07638080 0.0000863
# HP-PC -0.19176624 -0.3182988 -0.06523368 0.0002431
# PD-PC -0.51015912 -0.6391877 -0.38113053 0.0000000
# SP-PC -0.57428869 -0.7035624 -0.44501503 0.0000000
# HP-CI  0.01269453 -0.1126182  0.13800729 0.9997275
# PD-CI -0.30569835 -0.4335310 -0.17786574 0.0000000
# SP-CI -0.36982792 -0.4979079 -0.24174795 0.0000000
# PD-HP -0.31839288 -0.4446751 -0.19211070 0.0000000
# SP-HP -0.38252245 -0.5090550 -0.25598989 0.0000000
# SP-PD -0.06412957 -0.1931582  0.06489902 0.7150035

#all sites daily temp ranges are significantly different except SC & PC, CI & HP, and SP & PD. 



#### Calculate Degree heating weeks for all Temp data we have for sites ####

#Import temp file with complete Temp data for all sites (file above for site characterization only include Sept 2024 -Jan 2025 to be consistent across sites)
TempAll <- read.csv("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/TempAll_BocasSites.csv")
#this TempAll file includes all available Temp data from our sites from May 2022- Jan 2025
# additional temp data for HP was included from Lucey et al. 2024 from May 2022 - Oct 2023 to allow for better comparisons of sites through time

# Convert datetime to POSIXct if not already so dates can be read
TempAll$Date_Time <- parse_date_time2(TempAll$Date_Time, c("%m/%d/%y %H:%M"), exact = TRUE)
TempAll$Date_Time <- as.POSIXct(TempAll$Date_Time)

#reorder Sites variable for figures
f=c('Sid Ciudad','Punta Caracol', 'Cristobal Island', 'Hospital Point21-23', 'Hospital Point','Punta Donato', 'STRI Point')
TempAll <- within(TempAll, Site<- factor(Site, levels=f))

unique(TempAll$Site) #sanity check, makes sure all sites are showing up as they should
#there should be 2 hospital point groups: 
#one for the Lucey et al. data (Hospital Point 2021-2023), includes only data from May 2022-Oct2023 from this dataset
#and one for the data collected by our loggers (Hospital Point).
#these data are kept separate for DHW analysis to due to differences in exact locations of loggers across site characteristics.

#Earliest dates for each site -- sanity check
earliest_dates <- TempAll %>%
  group_by(Site) %>%
  summarise(first_date = min(Date_Time, na.rm = TRUE), .groups = "drop")
earliest_dates 

### estimate local MMM using multi-year local logger data ###
### MMMs By Site ###
#Extract date components
MMM <- TempAll %>%
  mutate(date = as.Date(Date_Time),
         year = year(date),
         month = month(date))

#Calculate monthly mean temperature per site and year
monthly_means <- MMM %>%
  group_by(Site, year, month) %>%
  summarise(monthly_mean_temp = mean(Temperature, na.rm = TRUE), .groups = "drop")

#Identify the warmest month each year per site
max_months_per_site_year <- monthly_means %>%
  group_by(Site, year) %>%
  filter(monthly_mean_temp == max(monthly_mean_temp)) %>%
  ungroup()

#Compute the MMM per site
local_MMMs <- max_months_per_site_year %>%
  group_by(Site) %>%
  summarise(MMM = mean(monthly_mean_temp, na.rm = TRUE), .groups = "drop")
local_MMMs
# A tibble: 7 × 2
# Site                  MMM
# <fct>               <dbl>
# 1 Sid Ciudad           29.9
# 2 Punta Caracol        29.6
# 3 Cristobal Island     30.4
# 4 Hospital Point21-23  30.8
# 5 Hospital Point       31.0
# 6 Punta Donato         30.4
# 7 STRI Point           29.9

### Now calculate DHW with Site MMMs ###
#Calculate daily means per site
daily_means <- TempAll %>%
  mutate(date = as.Date(Date_Time)) %>%
  group_by(Site, date) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE), .groups = "drop")

#Set your Maximum Monthly Mean (MMM)
daily_means <- left_join(daily_means, local_MMMs, by = "Site") #this calculates from our site's specific MMM calculated from logger data above


#Calculates hotspots from our site's specific MMM calculated from logger data above
daily_means <- daily_means %>%
  mutate(hotspot = ifelse(mean_temp > MMM, mean_temp - MMM, 0),
         hotspot_excess = ifelse(hotspot > 1, hotspot, 0))

#Calculate DHW using a rolling sum over 84 days, per site
DHW_calc <- daily_means %>%
  arrange(Site, date) %>%
  group_by(Site) %>%
  mutate(DHW = rollapply(hotspot_excess, width = 84, FUN = sum, align = "right", fill = NA) / 7) %>%
  ungroup()

#plot DHW
# Desired site order
f <- c('Sid Ciudad','Punta Caracol', 'Cristobal Island', 'Hospital Point21-23', 'Hospital Point','Punta Donato', 'STRI Point')

# Defined Site as factor for designated order above
DHW_calc$Site <- factor(DHW_calc$Site, levels = f)

#Plot as line graph
#site colors
SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96", "#3C6E96","#AABAEE", "#AD961D" )

# Create a data frame for year labels - allows for year and months to be on x-axis
year_labels <- DHW_calc %>%
  mutate(year = format(date, "%Y")) %>%
  filter(format(date, "%m") == "01" & format(date, "%d") == "01") %>%  # Jan 1 only, years at this date of each year only
  group_by(year) %>%
  summarise(label_date = min(date), .groups = "drop")

# Plot
p <- ggplot(DHW_calc, aes(x = date, y = DHW, color = Site)) +
  geom_line(size = 2) +
  # Bleaching thresholds
  geom_hline(yintercept = 4, linetype = "dashed", color = "orange", size = 1) +
  geom_hline(yintercept = 8, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = min(DHW_calc$date), y = 4.6, label = "Significant Bleaching",
           color = "orange", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = min(DHW_calc$date), y = 8.6, label = "Severe Bleaching & Mortality",
           color = "red", hjust = 0, size = 5, fontface = "bold") +
  # Manual year label layer
  geom_text(data = year_labels,
            aes(x = label_date, y = -1, label = year),
            inherit.aes = FALSE,
            size = 5, fontface = "bold") +
  # Axis and theme
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b",  # Jan, Apr, Jul...
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1))+
  scale_color_manual(values = SiteColors) +
  labs(y = "Degree Heating Weeks (°C-weeks)") +
  theme_classic() +
  theme(
    panel.grid.major = element_line(color = "grey75"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    axis.ticks = element_line(color = "black")
  )
p

#You may receive the following warning:
# "Warning message:Removed 581 rows containing missing values or values outside the scale range (`geom_line()`). "

#The plot isn't breaking—it’s just a heads-up that the early part of each 
#time series (first ~83 days per site) doesn’t yet have enough data to calculate DHW.
#so isn't included in the plot.
#save as PDF vector for minor tweaks and cleaning up in Adobe Illustrator (like making Lucey et al. data dashed)

#ggsave("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/Figures/DHW_Bocas_2022-2023.pdf", plot = last_plot(), device = cairo_pdf, width = 10, height = 8)

# #The code below was used to check the data for calculating DHW for any weirdness or untrimmed dates before/after deployment
# # Plot daily mean temperatures for all raw data
# #show temperatures together by month and day so the HP data is with other temp data
# daily_means$month_day <- as.Date(format(daily_means$date, "2020-%m-%d"))
# daily_means$plot_date <- as.Date(ifelse(format(daily_means$month_day, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08"),
#                                            format(daily_means$month_day, "2021-%m-%d"),
#                                            format(daily_means$month_day, "2020-%m-%d")))
# #line graph daily means
# ggplot(daily_means, aes(x = date, y = mean_temp, color = Site)) +
#   geom_line(size = 1) +
#   scale_color_manual(values=SiteColors)+
#   labs(x = "Date", y = "Mean Daily Temperature (°C)", color = "Site") +
#   theme_classic()+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 16, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))
# 
# #line graph -raw
# ggplot(TempAll, aes(x = Date_Time, y = Temperature, color = Site)) +
#   geom_line(size = 1) +
#   scale_color_manual(values=SiteColors)+
#   labs(x = "Date", y = "Mean Daily Temperature (°C)", color = "Site") +
#   theme_classic()+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 16, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))
##Straight semi-horizontal lines are when there were gaps in data collection across sites.


#### Import Coral Metadata ####
#import data used for depth, sex ratio, fecundity, and spatial analyses
data <- read.csv("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/Panama_SSID_2024-2025_Metadata.csv")
head(data)


#### Differences in Depth across Sites? ####
#pull depths (m) and sites for corals
Depths <- data %>% select(Depth_m, Site)

#reorder Sites variable for figures
f=c('Sid Ciudad','Punta Caracol', 'Cristobal Island', 'Hospital Point', 'Punta Donato', 'STRI Point')
Depths <- within(Depths, Site <- factor(Site, levels=f))

#site colors
SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96","#AABAEE", "#AD961D" )#)"#CFD8F5","#768EA1

#boxplot
ggplot(Depths, aes(x = Site, y = Depth_m, color = Site, fill = Site)) +
  scale_fill_manual(values=SiteColors)+
  scale_color_manual(values=SiteColors)+
  geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.35, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1.5, color ="black", linewidth = 1.5, alpha = 0.6)+
  labs(x = "Date", y = "Depth (m)", color = "Site") +
  theme_classic()+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#check anova assumptions
m <- lm(Depth_m ~ Site, data = Depths)
check_model(m)#says decent

hist(Depths$Depth_m)#looks right skewed
shapiro.test(Depths$Depth_m)#p-value = 1.1e-10, not normal

#try transforming
#transDepth = log(Depths$Depth_m)
transDepth = 1/Depths$Depth_m #reciprocal 
#check anova assumptions
m <- lm(transDepth ~ Site, data = Depths)
check_model(m)#log looks worse, reciprocal is iffy
hist(transDepth)#log looks bimodal, recip - bimodal
shapiro.test(transDepth) # p > 0.05 is not normally distributed
#log p-value = 1.865e-10 not normal, recip p-value = 4.088e-11

#Do non-parametric instead
kruskal.test(Depths$Depth_m~Depths$Site)
# Kruskal-Wallis rank sum test
# data:  Depths$Depth_m by Depths$Site
# Kruskal-Wallis chi-squared = 107.51, df = 5, p-value < 2.2e-16

library("FSA")
dunnTest(Depth_m~Site, data=Depths, method="bonferroni")
# Dunn (1964) Kruskal-Wallis multiple comparison
# p-values adjusted with the Bonferroni method.
# 
# Comparison                                 Z      P.unadj        P.adj
# 1  Cristobal Island - Hospital Point -3.96064919 7.474627e-05 1.121194e-03*
# 2   Cristobal Island - Punta Caracol -0.01934926 9.845625e-01 1.000000e+00
# 3     Hospital Point - Punta Caracol  3.94154962 8.095688e-05 1.214353e-03*
# 4    Cristobal Island - Punta Donato -6.29818270 3.011554e-10 4.517332e-09*
# 5      Hospital Point - Punta Donato -2.25626323 2.405415e-02 3.608123e-01*
# 6       Punta Caracol - Punta Donato -6.27883344 3.411228e-10 5.116842e-09*
# 7      Cristobal Island - Sid Ciudad -0.14753807 8.827073e-01 1.000000e+00
# 8        Hospital Point - Sid Ciudad  3.81501492 1.361747e-04 2.042621e-03*
# 9         Punta Caracol - Sid Ciudad -0.12818882 8.979995e-01 1.000000e+00
# 10         Punta Donato - Sid Ciudad  6.15064463 7.716865e-10 1.157530e-08*
# 11     Cristobal Island - STRI Point -6.99233724 2.703441e-12 4.055161e-11*
# 12       Hospital Point - STRI Point -2.94146056 3.266684e-03 4.900027e-02*
# 13        Punta Caracol - STRI Point -6.97298799 3.102791e-12 4.654187e-11*
# 14         Punta Donato - STRI Point -0.69415454 4.875853e-01 1.000000e+00
# 15           Sid Ciudad - STRI Point -6.84479917 7.658323e-12 1.148748e-10*

#Every site significantly different from others except: CI-PC, CI-SC, PC-SC, PD-SP


#### Import Coral Metadata ####
#same as line 531 above but also code here in case skipped importing above
data <- read.csv("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/Panama_SSID_2024-2025_Metadata.csv")
head(data)

#reorder Sites variable for figures
f=c('Sid Ciudad','Punta Caracol', 'Cristobal Island', 'Hospital Point', 'Punta Donato', 'STRI Point')
data <- within(data, Site <- factor(Site, levels=f))

data <- data %>%
  mutate(Site = recode(Site,
                          "Cristobal Island" = "CI",
                          "Hospital Point" = "HP",
                          "Punta Caracol" = "PC",
                          "Punta Donato" = "PD",
                          "Sid Ciudad" = "SC",
                          "STRI Point" = "SP"))

data <- data %>%
  mutate(Sex_Sept2024 = recode(Sex_Sept2024,
                          "F" = "Oocytes",
                          "M" = "Spermatocytes",
                          "H" = "Both"))


#### Distribution of Gamete Production #####
# convert dataframe to table 
Gams = table(data$Sex_Sept2024,data$Site)

# display  
Gams #table of counts by Sex Type produced and site
rownames(Gams)[rownames(Gams) == ""] = "Unknown" #if no name for row, make "Unknown" - colony without sex determinations
Gams #check it did what it should

# convert dataframe to table with site and lineage characteristics included
Gams <- as.data.frame(Gams)
Gams <- rename(Gams, SexType = Var1, Site = Var2) #rename columns by what they are
Gams$Site <- factor(Gams$Site) #make sure right type identifiers
Gams$SexType <- factor(Gams$SexType)
Gams$Freq <- as.numeric(Gams$Freq)

#filter to non-zero frequencies and unknowns
Gams <- subset(Gams, SexType != "Unknown")#filter out unknown for comparisons and stats

#only for figure, remove zeroes so it looks cleaner
Gametes1 <- subset(Gams, Freq != "0") # filter out zeroes for figure

#### plot bar graphs of sex distributions by site, with n
#order the Sex Type factor for figures
f=c('Spermatocytes','Oocytes','Both')
Gametes1 <- within(Gametes1, SexType <- factor(SexType, levels=f))

#All corals across lineages --- not used for MS
ggplot(Gametes1, aes(x = Site, y= Freq, fill = SexType)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("#50164A","#E59EDD","#A02B93"))+
  geom_text(label = Gametes1$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  theme_classic() +
  ylab("Frequency")+
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#Now lineage facets
# convert dataframe to table 
GametesLin = table(data$Sex_Sept2024,data$Site,data$SuspectedLineage)

# display table
GametesLin #table of counts by size and site
rownames(GametesLin)[rownames(GametesLin) == ""] = "Unknown"
GametesLin #check that it worked

# convert dataframe to tablewith site and lineage characteristics included
GametesLin <- as.data.frame(GametesLin)
GametesLin <- rename(GametesLin, SexType = Var1, Site = Var2, Lineage = Var3)
GametesLin$SexType <- factor(GametesLin$SexType)
GametesLin$Site <- factor(GametesLin$Site)
GametesLin$Lineage <- factor(GametesLin$Lineage)
GametesLin$Freq <- as.numeric(GametesLin$Freq)


#filter to non-zero frequencies and unknowns
GametesLin <- subset(GametesLin, SexType != "Unknown") #remove unknown, for site comparisons for fig and stats

#only for figure
GametesLin1 <- subset(GametesLin, Freq != "0")# to remove zero freq from figure

#### Bar graphs of sex distributions by lineage, with n
f=c('Spermatocytes','Oocytes','Both')
GametesLin1 <- within(GametesLin1, SexType <- factor(SexType, levels=f))

#Lineage colors
LinCol = c("#3f007d", "#807dba" )

#comparison of Sex Types faceted by lineage 
# Fig 2D
ggplot(GametesLin1, aes(x = Site, y= Freq, fill = SexType)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c( "#50164A","#E59EDD","#A02B93"))+
  geom_text(label = GametesLin1$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  theme_classic() +
  ylab("Sex Type Frequency")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))


#### Stats Gamete Distributions ####

### Fisher's Exact Test --- used since do not meet Chi-Square assumptions of counts >1 and most >5 due to small n

# #Make GametesCollapsed column with gametes collapsed into "HasEggs" or "No Eggs" to improve power
# #tried with collapsed versions but had more muddled results, so stuck with the three sex type levels
# GametesLin$GametesCollapsed <- as.character(GametesLin$SexType)
# GametesLin$GametesCollapsed[GametesLin$GametesCollapsed %in% c("Oocytes", "Both")] <- "HasEggs"
# GametesLin$GametesCollapsed[GametesLin$GametesCollapsed == "Spermatocytes"] <- "NoEggs"
# GametesLin$GametesCollapsed <- factor(GametesLin$GametesCollapsed, levels = c("HasEggs", "NoEggs"))

# Differ by Site?
GametesLin$SexType <- droplevels(GametesLin$SexType)## drop "Unknown" zero row for analysis
Gametes_by_Site <- xtabs(Freq ~ SexType + Site, data = GametesLin)
#chisq.test(Gametes_by_Site) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test # Sperm/eggs/both
# data:  Gametes_by_Site
# X-squared = 19.472, df = 10, p-value = 0.03466 *
fisher.test(Gametes_by_Site, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Site
# p-value = 0.0106
# alternative hypothesis: two.sided


# Gametes_by_Site <- xtabs(Freq ~ GametesCollapsed + Site, data = GametesLin)
# #chisq.test(Gametes_by_Site)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test #Has Eggs/No Eggs
# # data:  Gametes_by_Site
# # X-squared = 13.806, df = 5, p-value = 0.01689 *
# fisher.test(Gametes_by_Site, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Site
# # p-value = 0.01031
# # alternative hypothesis: two.sided

#Differ by Lineage?
GametesLin$SexType <- droplevels(GametesLin$SexType) ## drop "Unknown" zero row for analysis
Gametes_by_Lineage <- xtabs(Freq ~ SexType + Lineage, data = GametesLin)
#chisq.test(Gametes_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test # Sperm/eggs/both
# data:  Gametes_by_Lineage
# X-squared = 7.5229, df = 2, p-value = 0.02325 *
fisher.test(Gametes_by_Lineage, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Lineage
# p-value = 0.01951
# alternative hypothesis: two.sided

# Gametes_by_Lineage <- xtabs(Freq ~ GametesCollapsed + Lineage, data = GametesLin)
# #chisq.test(Gametes_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test with Yates' continuity correction #Has Eggs/No Eggs
# # data:  Gametes_by_Lineage
# # X-squared = 2.677, df = 1, p-value = 0.1018
# fisher.test(Gametes_by_Lineage, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Lineage
# # p-value = 0.0885
# # alternative hypothesis: true odds ratio is not equal to 1

### Now within lineage across sites
#Lineage 1 comparisons across sites
Lin1 <- subset(GametesLin, Lineage == "1")

Gametes_by_Site1 <- xtabs(Freq ~ SexType  + Site, data = Lin1)
# Drop empty rows and columns from the contingency table (Sites without L1)
Gametes_by_Site1 <- Gametes_by_Site1[rowSums(Gametes_by_Site1) > 0, colSums(Gametes_by_Site1) > 0]
#chisq.test(Gametes_by_Site1)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test # Sperm/eggs/both
# data:  Gametes_by_Site1
# X-squared = 7.268, df = 6, p-value = 0.2968
fisher.test(Gametes_by_Site1, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Site1
# p-value = 0.4374
# alternative hypothesis: two.sided

# Gametes_by_Site1 <- xtabs(Freq ~ GametesCollapsed  + Site, data = Lin1)
# # Drop empty rows and columns from the contingency table (Sites without L1)
# Gametes_by_Site1 <- Gametes_by_Site1[rowSums(Gametes_by_Site1) > 0, colSums(Gametes_by_Site1) > 0]
# #chisq.test(Gametes_by_Site1)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test  #Has Eggs/No Eggs
# # data:  Gametes_by_Site1
# # X-squared = 6.3198, df = 3, p-value = 0.09705
# fisher.test(Gametes_by_Site1, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Site1
# # p-value = 0.1059
# # alternative hypothesis: two.sided


#Lineage 2 comparisons across sites
Lin2<- subset(GametesLin, Lineage == "2")

Gametes_by_Site2 <- xtabs(Freq ~ SexType + Site, data = Lin2)
# Drop empty rows and columns from the contingency table (Sites without L2)
Gametes_by_Site2 <- Gametes_by_Site2[rowSums(Gametes_by_Site2) > 0, colSums(Gametes_by_Site2) > 0]
#chisq.test(Gametes_by_Site2)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test # Sperm/eggs/both
# data:  Gametes_by_Site2
# X-squared = 5.3059, df = 6, p-value = 0.5052
fisher.test(Gametes_by_Site2, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Site2
# p-value = 0.4513
# alternative hypothesis: two.sided

# Gametes_by_Site2 <- xtabs(Freq ~ GametesCollapsed + Site, data = Lin2)
# # Drop empty rows and columns from the contingency table (Sites without L2)
# Gametes_by_Site2 <- Gametes_by_Site2[rowSums(Gametes_by_Site2) > 0, colSums(Gametes_by_Site2) > 0]
# #chisq.test(Gametes_by_Site2)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test #Has Eggs/No Eggs
# # data:  Gametes_by_Site2
# # X-squared = 4.4259, df = 3, p-value = 0.219
# fisher.test(Gametes_by_Site2, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Site2
# # p-value = 0.2388
# # alternative hypothesis: two.sided



###Logistic Regression --- tried this but ran horribly with my dataset, maybe due to low replication, stick with chi-square
# glm_full <- glm(GametesCollapsed ~ Site + Lineage, data = GametesLin, family = binomial)
# summary(glm_full)
# exp(coef(glm_full))
# 
# Lin1 <- droplevels(Lin1)
# glm_lin1 <- glm(GametesCollapsed ~ Site, data = Lin1, family = binomial)
# summary(glm_lin1)
# 
# Lin1 <- droplevels(Lin2)
# glm_lin1 <- glm(GametesCollapsed ~ Site, data = Lin2, family = binomial)
# summary(glm_lin1)


### Log linear model --- same results as Fisher's Exact Test ---use Fisher's Exact Test
# # Create a contingency table from the data frame
# gamete_table <- xtabs(Freq ~ GametesCollapsed + Site, data = Lin1)
# # Remove columns (sites) that have only zeros across all gamete categories
# gamete_table_trimmed <- gamete_table[, colSums(gamete_table) > 0]
# # Fit log-linear models
# loglin(gamete_table_trimmed, margin = list("GametesCollapsed", "Site"))
# 
# # Create a contingency table from the data frame
# gamete_table <- xtabs(Freq ~ GametesCollapsed + Site, data = Lin2)
# # Remove columns (sites) that have only zeros across all gamete categories
# gamete_table_trimmed <- gamete_table[, colSums(gamete_table) > 0]
# # Fit log-linear models
# loglin(gamete_table_trimmed, margin = list("GametesCollapsed", "Site"))



#### Gametogenic Stage Comparisons ####

### Just Eggs or Spermatocytes analyses
#using coral metadata imported above as "data"

# convert dataframe to table 
Oocytes = table(data$OocyteStage,data$Site,data$SuspectedLineage)
#SuspectedLineage column includes the known 2bRAD IDs and the IDs from morphological characterization of colonies without 2bRAD data
#KnownLineages column has just the confirmed lineage IDs from 2bRAD data

# display  
Oocytes #table of counts by gametes produced and site
rownames(Oocytes)[rownames(Oocytes) == ""] = "Unknown" #make empty rows "Unknown", since colonies were not able to be sexed
Oocytes #check

# convert dataframe to tablewith site and lineage characteristics included
dataStagesOo <- as.data.frame(Oocytes)
dataStagesOo <- rename(dataStagesOo, Stages = Var1, Site = Var2, Lineage = Var3, Frequency = Freq)
dataStagesOo$Site <- factor(dataStagesOo$Site)
dataStagesOo$Stages <- factor(dataStagesOo$Stages)
dataStagesOo$Lineage <- factor(dataStagesOo$Lineage)
dataStagesOo$Frequency <- as.numeric(dataStagesOo$Frequency)

#filter to non-zero frequencies and unknowns
dataStagesOo <- subset(dataStagesOo, Stages != "Unknown")#filter out unknown for comparisons and stats

#For figure without zeroes and unrepresented sites 
# I omit this line now to include all sites (even those that don't have eggs) in graph
#dataStagesOo1 <- subset(dataStagesOo, Frequency != "0") # filter out zeroes for figure

##oocyte stages by sites -- faceted by lineage
colorsOo = c("#930516","#F06F3C", "#FFA875")

#Lineage colors
LinCol = c("#3f007d", "#807dba" )

#dataStagesOo = subset(dataStagesOo, Counts != "0")

#compare oocyte stages across all sites for lineage facets
#Fig 2 F
ggplot(dataStagesOo, aes(x = Site, y= Frequency, fill = Stages)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,8)) +
  geom_bar(stat="identity", color = "black")+
  #scale_fill_brewer(palette="Reds") +
  scale_fill_manual(values = colorsOo)+
  geom_text(label = dataStagesOo$Frequency,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  ylab("Oocyte Stage Frequency")+
  theme_classic() +
  labs(fill = "Stages")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))
#Clean up in Adobe Illustrator

##oocyte stages by sites - All -- no zeroes, no unrepresented sites --- not used in MS
colorsOo = c("#930516","#F06F3C", "#FFA875")
dataStagesOo = subset(dataStagesOo, Frequency != "0")
ggplot(dataStagesOo, aes(x = Site, y= Frequency, fill = Stages)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,8)) +
  geom_bar(stat="identity", color = "black")+
  #scale_fill_brewer(palette="Reds") +
  scale_fill_manual(values = colorsOo)+
  geom_text(label = dataStagesOo$Frequency,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  ylab("Oocyte Stage Frequency")+
  theme_classic() +
  labs(fill = "Stages")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#### Stats Oocyte stages ####
### Fisher's Exact Test --- can't meet Chi-Square assumptions of counts >1 and most >5 due to small n

# Differ by Site?
OoStages_by_Site <- xtabs(Frequency ~ Stages + Site, data = dataStagesOo)
# Drop empty rows and columns from the contingency table (Sites without L1)
OoStages_by_Site <- OoStages_by_Site[rowSums(OoStages_by_Site) > 0, colSums(OoStages_by_Site) > 0]
#chisq.test(OoStages_by_Site)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  Stages_by_Site
# X-squared = 5.3571, df = 6, p-value = 0.4989
fisher.test(OoStages_by_Site, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  OoStages_by_Site
# p-value = 0.6084
# alternative hypothesis: two.sided

#Differ by Lineage?
OoStages_by_Lineage <- xtabs(Frequency ~ Stages + Lineage, data = dataStagesOo)
# Drop empty rows and columns from the contingency table (Sites without L1)
OoStages_by_Lineage <- OoStages_by_Lineage[rowSums(OoStages_by_Lineage) > 0, colSums(OoStages_by_Lineage) > 0]
#chisq.test(OoStages_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  OoStages_by_Lineage
# X-squared = 3.4286, df = 2, p-value = 0.1801
fisher.test(OoStages_by_Lineage, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  OoStages_by_Lineage
# p-value = 0.3636
# alternative hypothesis: two.sided


### Now within lineage across sites
#Lineage 1 comparisons across sites --- no good egg comparisons across sites so did not use
#Lin1 <- subset(dataStagesOo, Lineage == "1") #Compares CI and HP, CI only has n=1 so not a good comparison
# 
# OoStages_by_Site1 <- xtabs(Frequency ~ Stages  + Site, data = Lin1)
# # Drop empty rows and columns from the contingency table (Sites without L1)
# OoStages_by_Site1 <- OoStages_by_Site1[rowSums(OoStages_by_Site1) > 0, colSums(OoStages_by_Site1) > 0]
# #chisq.test(OoStages_by_Site1)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test
# # data:  OoStages_by_Site1
# # X-squared = 0.83333, df = 2, p-value = 0.6592
# #Compares CI and HP, CI only has n=1 so not a good comparison
# fisher.test(OoStages_by_Site1, workspace = 2e7)


#Lineage 2 comparisons across sites
Lin2<- subset(dataStagesOo, Lineage == "2")

OoStages_by_Site2 <- xtabs(Frequency ~ Stages  + Site, data = Lin2)
# Drop empty rows and columns from the contingency table (Sites without L1)
OoStages_by_Site2 <- OoStages_by_Site2[rowSums(OoStages_by_Site2) > 0, colSums(OoStages_by_Site2) > 0]
#chisq.test(OoStages_by_Site2)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test with Yates' continuity correction
# data:  OoStages_by_Site2
# X-squared = 0.017361, df = 1, p-value = 0.8952
fisher.test(OoStages_by_Site2, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  OoStages_by_Site2
# p-value = 0.5714
# alternative hypothesis: true odds ratio is not equal to 1


#### Now for spermatocyte stages ####
#uses coral metadata imported above (line 531)

### Fig 2 H

# convert dataframe to table 
Sperms = table(data$SpermaryStage,data$Site,data$SuspectedLineage)

# display  
Sperms #table of counts by gametes produced and site
rownames(Sperms)[rownames(Sperms) == ""] = "Unknown" #make empty rown "Unknown" sex
Sperms #check

# convert dataframe to tablewith site and lineage characteristics included
dataStagesSp <- as.data.frame(Sperms)
dataStagesSp <- rename(dataStagesSp, Stages = Var1, Site = Var2, Lineage = Var3, Frequency = Freq)
dataStagesSp$Site <- factor(dataStagesSp$Site)
dataStagesSp$Stages <- factor(dataStagesSp$Stages)
dataStagesSp$Lineage <- factor(dataStagesSp$Lineage)
dataStagesSp$Frequency <- as.numeric(dataStagesSp$Frequency)

#filter out unknown
dataStagesSp <- subset(dataStagesSp, Stages != "Unknown")#filter out unknown for comparisons and stats

#only for figure, remove zeroes to make cleaner
dataStagesSp1 <- subset(dataStagesSp, Frequency != "0") # filter out zeroes for figure

#Spermarrye stage colors
colorsSp = c("#3ad2ff", "#67a9cf","#2166ac","#01304C")

#Lineage colors
LinCol = c("#3f007d", "#807dba" )

###Spermatocytes stages by site ---- faceted by Lineage
ggplot(dataStagesSp1, aes(x = Site, y= Frequency, fill = Stages)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  geom_bar(stat="identity", color = "black")+
  #scale_fill_brewer(palette="Blues") +
  scale_fill_manual(values = colorsSp)+
  geom_text(label = dataStagesSp1$Frequency,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  ylab("Spermatocyte Stage Frequency")+
  theme_classic() +
  labs(fill = "Stages")+
  #scale_y_continuous(labels = scales::percent)+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

# #Spermary stages by site -- All together --- not used for MS, used to get an idea of site specific stages
# ggplot(dataStagesSp1, aes(x = Site, y= Frequency, fill = Stages)) + 
#   scale_y_continuous(expand = c(0,0), limits = c(0,30)) +
#   geom_bar(stat="identity", color = "black")+
#   #scale_fill_brewer(palette="Blues") +
#   scale_fill_manual(values = colorsSp)+
#   geom_text(label = dataStagesSp1$Frequency,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
#   ylab("Spermatocyte Stage Frequency")+
#   theme_classic() +
#   #scale_y_continuous(labels = scales::percent)+
#   labs(fill = "Stages")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

#### Stats Spermatocyte stages ####
### Fisher's Exact Test --- can't meet Chi-Square assumptions of counts >1 and most >5 due to small n

# Differ by Site?
SpStages_by_Site <- xtabs(Frequency ~ Stages + Site, data = dataStagesSp)
# Drop empty rows and columns from the contingency table (Sites without L1)
SpStages_by_Site <- SpStages_by_Site[rowSums(SpStages_by_Site) > 0, colSums(SpStages_by_Site) > 0]
#chisq.test(SpStages_by_Site)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStages_by_Site
# X-squared = 42.665, df = 15, p-value = 0.0001774
fisher.test(SpStages_by_Site, workspace = 1e8) #needed to increase workspace for the heavy computation of a large contingency table
# Fisher's Exact Test for Count Data
# data:  SpStages_by_Site
# p-value = 0.0001191
# alternative hypothesis: two.sided

#Differ by Lineage?
SpStages_by_Lineage <- xtabs(Frequency ~ Stages + Lineage, data = dataStagesSp)
# Drop empty rows and columns from the contingency table (Sites without L1)
SpStages_by_Lineage <- SpStages_by_Lineage[rowSums(SpStages_by_Lineage) > 0, colSums(SpStages_by_Lineage) > 0]
#chisq.test(SpStages_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStages_by_Lineage
# X-squared = 8.8391, df = 3, p-value = 0.03151
fisher.test(SpStages_by_Lineage, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  SpStages_by_Lineage
# p-value = 0.03177
# alternative hypothesis: two.sided


### Now within lineage across sites
#Lineage 1 comparisons across sites
Lin1 <- subset(dataStagesSp, Lineage == "1")

SpStages_by_Site1 <- xtabs(Frequency ~ Stages  + Site, data = Lin1)
# Drop empty rows and columns from the contingency table (Sites without L1)
SpStages_by_Site1 <- SpStages_by_Site1[rowSums(SpStages_by_Site1) > 0, colSums(SpStages_by_Site1) > 0]
#chisq.test(SpStages_by_Site1)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStages_by_Site1
# X-squared = 10.827, df = 9, p-value = 0.2877
fisher.test(SpStages_by_Site1, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  SpStages_by_Site1
# p-value = 0.2937
# alternative hypothesis: two.sided


#Lineage 2 comparisons across sites
Lin2<- subset(dataStagesSp, Lineage == "2")

SpStages_by_Site2 <- xtabs(Frequency ~ Stages  + Site, data = Lin2)
# Drop empty rows and columns from the contingency table (Sites without L1)
SpStages_by_Site2 <- SpStages_by_Site2[rowSums(SpStages_by_Site2) > 0, colSums(SpStages_by_Site2) > 0]
#chisq.test(SpStages_by_Site2)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStages_by_Site2
# X-squared = 26.507, df = 9, p-value = 0.001687
fisher.test(SpStages_by_Site2, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  SpStages_by_Site2
# p-value = 0.0007402
# alternative hypothesis: two.sided



#### Colony Fecundity Comparisons ####
#import metadata again if not already imported, reorder and rename (line 531)

# Getting only individuals with oocyte comparisons 
dataEggs = subset(data, Sex_Sept2024 != "Spermatocytes") #remove only spermatocyte producers from dataset

dataEggs$SuspectedLineage <- factor(dataEggs$SuspectedLineage)

#### Fecundity Eggs per Polyp ####

# #visualization of fecundity by site, jitter/points Lineage --- not used for MS
# ggplot(dataEggs, aes(x=Site, y=AverageEggs,shape = SuspectedLineage, color = SuspectedLineage, fill=Site)) +
#   scale_fill_manual(values = SiteColors)+
#   scale_color_manual(values = LinCol)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
#   geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   # facet_grid2(SuspectedLineage~.,
#   #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#   #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Oocytes per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

# #visualization of fecundity by site, faceted by lineage --- not used in MS
# ggplot(dataEggs, aes(x=Site, y=AverageEggs,color = Site, fill=Site)) +
#   scale_fill_manual(values = SiteColors)+
#   scale_color_manual(values = SiteColors)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Oocytes per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))


### Fig 3A

#SexType colors
eggColors = c( "#50164A","#A02B93","#E59EDD")

#visualization of fecundity by site*SexType produced jitter/points lineage
ggplot(dataEggs, aes(x=Site, y=AverageEggs, shape = SuspectedLineage, color = SuspectedLineage, fill=Sex_Sept2024)) +
  scale_fill_manual(values=eggColors)+
  scale_color_manual(values=LinCol)+
  scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
  geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
  #labs(color = "Gametes:", fill = "Gametes:") + 
  # facet_grid2(SuspectedLineage~.,
  #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
  #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  theme_classic() +
  ylab("Average Oocytes per Polyp")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#SexType colors
eggColors = c("#A02B93","#E59EDD")

#visualization of fecundity by SexType produced, jitter/points lineage
ggplot(dataEggs1, aes(x=Sex_Sept2024, y=AverageEggs, shape = SuspectedLineage,color = SuspectedLineage, fill=Sex_Sept2024)) +
  scale_fill_manual(values=eggColors)+
  scale_color_manual(values=LinCol)+
  scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
  geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
  #labs(color = "Gametes:", fill = "Gametes:") + 
  # facet_grid2(SuspectedLineage~.,
  #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
  #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  theme_classic() +
  ylab("Average Oocytes per Polyp")+
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))


# #visualization of fecundity by gametes produced faceted by lineage --- not used in MS
# ggplot(dataEggs1, aes(x=Sex_Sept2024, y=AverageEggs, color = Sex_Sept2024, fill=Sex_Sept2024)) +
#   scale_fill_manual(values=eggColors)+
#   scale_color_manual(values=eggColors)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   labs(color = "Gametes:", fill = "Gametes:") + 
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Oocytes per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))


# #visualization of fecundity by site*gametes produced faceted by lineage --- not used for MS
# ggplot(dataEggs, aes(x=Site, y=AverageEggs, color = Sex_Sept2024, fill=Sex_Sept2024)) +
#   scale_fill_manual(values=eggColors)+
#   scale_color_manual(values=eggColors)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   labs(color = "Gametes:", fill = "Gametes:") + 
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Oocytes per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))


#### Oocyte per polyp Stats ####
#assumption checks for ANOVA 
library("performance")
m <- lm(AverageEggs~SuspectedLineage+Site*Sex_Sept2024, data = dataEggs)
check_model(m) #looks pretty good

hist(dataEggs$AverageEggs) #right skewed, with possible outlier
shapiro.test(dataEggs$AverageEggs)#p-value = 0.08882, barely normal

#try transformation
transEggs = sqrt(dataEggs$AverageEggs)

m <- lm(transEggs~SuspectedLineage+Site*Sex_Sept2024, data = dataEggs)
check_model(m)#looks pretty good, depending on robustness of anova

hist(transEggs) #mostly normal with lil right skew
shapiro.test(transEggs)#p-value = 0.966, normal

#ANOVA
ANOVAmodel <- lm(transEggs ~ SuspectedLineage+Site*Sex_Sept2024, data = dataEggs)
anova(ANOVAmodel)
# Analysis of Variance Table
# 
# Response: transEggs
# Df Sum Sq Mean Sq F value   Pr(>F)   
# SuspectedLineage   1 1.3305  1.3305  3.6537 0.092322 . 
# Site               2 3.3560  1.6780  4.6081 0.046624 * 
# Sex_Sept2024       1 7.5647  7.5647 20.7741 0.001855 ** # SexType
# Site:Sex_Sept2024  1 1.2646  1.2646  3.4729 0.099384 . 
# Residuals          8 2.9131  0.3641                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Tukey test
SBPaov <- aov(ANOVAmodel)
TukeyHSD(SBPaov)
#w/ SuspectedLineage
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = ANOVAmodel)
# 
# $SuspectedLineage
# diff        lwr     upr     p adj
# 2-1 0.6823959 -0.1408487 1.50564 0.0923217
# 
# $Site
# diff         lwr       upr     p adj
# HP-CI -0.3707559 -2.60213419 1.8606225 0.9487828
# PD-CI -0.7437498 -2.83101308 1.3435135 0.6766084
# SP-CI  0.4204574 -1.74006534 2.5809802 0.9217557
# PD-HP -0.3729939 -1.73942850 0.9934407 0.8180744
# SP-HP  0.7912133 -0.68470473 2.2671313 0.3755381
# SP-PD  1.1642072 -0.08317119 2.4115856 0.0675721
# 
# $Sex_Sept2024
# diff       lwr      upr     p adj
# Oocytes-Both 1.17899 0.3557453 2.002234 0.0108186
# 
# $`Site:Sex_Sept2024`
# diff         lwr      upr     p adj
# HP:Both-CI:Both                NA          NA       NA        NA
# PD:Both-CI:Both                NA          NA       NA        NA
# SP:Both-CI:Both                NA          NA       NA        NA
# CI:Oocytes-CI:Both             NA          NA       NA        NA
# HP:Oocytes-CI:Both             NA          NA       NA        NA
# PD:Oocytes-CI:Both             NA          NA       NA        NA
# SP:Oocytes-CI:Both             NA          NA       NA        NA
# PD:Both-HP:Both       -0.07140535 -2.68719066 2.544380 1.0000000
# SP:Both-HP:Both        1.60396085 -1.06576385 4.273686 0.3586146
# CI:Oocytes-HP:Both     1.18350342 -2.19346090 4.560468 0.8412927
# HP:Oocytes-HP:Both     1.21912133 -1.70541556 4.143658 0.7155631
# PD:Oocytes-HP:Both     2.99554855 -0.38141576 6.372513 0.0884500
# SP:Oocytes-HP:Both             NA          NA       NA        NA
# SP:Both-PD:Both        1.67536620  0.07353138 3.277201 0.0397615 *
# CI:Oocytes-PD:Both     1.25490877 -1.36087654 3.870694 0.5847781
# HP:Oocytes-PD:Both     1.29052668 -0.70731235 3.288366 0.2918250
# PD:Oocytes-PD:Both     3.06695391  0.45116860 5.682739 0.0214189 *
# SP:Oocytes-PD:Both             NA          NA       NA        NA
# CI:Oocytes-SP:Both    -0.42045743 -3.09018213 2.249267 0.9972244
# HP:Oocytes-SP:Both    -0.38483952 -2.45279938 1.683120 0.9925671
# PD:Oocytes-SP:Both     1.39158771 -1.27813700 4.061312 0.5008685
# SP:Oocytes-SP:Both             NA          NA       NA        NA
# HP:Oocytes-CI:Oocytes  0.03561791 -2.88891898 2.960155 1.0000000
# PD:Oocytes-CI:Oocytes  1.81204513 -1.56491918 5.189009 0.4712439
# SP:Oocytes-CI:Oocytes          NA          NA       NA        NA
# PD:Oocytes-HP:Oocytes  1.77642723 -1.14810966 4.700964 0.3481533
# SP:Oocytes-HP:Oocytes          NA          NA       NA        NA
# SP:Oocytes-PD:Oocytes          NA          NA       NA        NA


#### Oocyte Diameters ####
### Fig 3B

#Sex Type colors
eggColors = c("#50164A","#A02B93", "#E59EDD")

#visualization of Egg diameters by SexType*site produced, jitter/points Lineage
ggplot(dataEggs, aes(x=Site, y=AvgEggDiameter_um, shape=SuspectedLineage,color=SuspectedLineage, fill=Sex_Sept2024)) +
  scale_fill_manual(values=eggColors)+
  scale_color_manual(values=LinCol)+
  #scale_y_continuous(expand = c(0,0), limits = c(0,0.35)) +
  geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
  #labs(color = "Gametes:", fill = "Gametes:") + 
  # facet_grid2(SuspectedLineage~.,
  #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
  #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  theme_classic() +
  ylab("Average Oocyte Diameter (µm)")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#Receive warnings: 
# 1: Removed 20 rows containing non-finite outside the scale range (`stat_boxplot()`). 
# 2: Removed 20 rows containing missing values or values outside the scale range (`geom_point()`). 
#This is because we did not remove rows without sexed individuals (N=20) from the dataset, but they also do not show up

#gametes colors
eggColors = c("#A02B93","#E59EDD")

dataEggs1 = subset(dataEggs, Sex_Sept2024 != "") #remove empty rows from dataset

#visualization of Egg diameters by gametes produced
ggplot(dataEggs1, aes(x=Sex_Sept2024, y=AvgEggDiameter_um, shape = SuspectedLineage, color = SuspectedLineage, fill=Sex_Sept2024)) +
  scale_fill_manual(values=eggColors)+
  scale_color_manual(values=LinCol)+
  #scale_y_continuous(expand = c(0,0), limits = c(0,0.35)) +
  geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
  geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
  #labs(color = "Gametes:", fill = "Gametes:") + 
  theme_classic() +
  # facet_grid2(SuspectedLineage~.,
  #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
  #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  ylab("Average Oocyte Diameter (µm)")+
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))


# #site colors ---- not used for MS
# SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96","#AABAEE", "#AD961D")
# #Lineage colors
# LinCol = c("#3f007d", "#807dba" )
# 
# #visualization of egg diameters by site, jitter points by lineage
# ggplot(dataEggs, aes(x=Site, y=AvgEggDiameter_um, color = SuspectedLineage, fill=Site, shape = SuspectedLineage)) +
#   scale_fill_manual(values = SiteColors)+
#   scale_color_manual(values = LinCol)+
#   #scale_y_continuous(expand = c(0,0), limits = c(0,0.6)) +
#   geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   # facet_grid2(SuspectedLineage~., 
#   #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
#   #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Egg Diameter (µm)")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

# #visualization of egg diameters by site, facet by lineage --- not used in MS
# ggplot(dataEggs, aes(x=Site, y=AvgEggDiameter_um, color = Site, fill=Site)) +
#   scale_fill_manual(values = SiteColors)+
#   scale_color_manual(values = SiteColors)+
#   #scale_y_continuous(expand = c(0,0), limits = c(0,0.6)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   facet_grid2(SuspectedLineage~., 
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Egg Diameter (µm)")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

# #visualization of Egg diameters by gametes produced --- not used in MS
# ggplot(dataEggs1, aes(x=Sex_Sept2024, y=AvgEggDiameter_um, color = Sex_Sept2024, fill=Sex_Sept2024)) +
#   scale_fill_manual(values=eggColors)+
#   scale_color_manual(values=eggColors)+
#   #scale_y_continuous(expand = c(0,0), limits = c(0,0.35)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   labs(color = "Gametes:", fill = "Gametes:") + 
#   theme_classic() +
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   ylab("Average Oocyte Diameter (µm)")+
#   theme(legend.position="top", 
#       legend.title=element_text(size=17, face = "bold", color = "black"), 
#       legend.text=element_text(size=17, face = "bold", color="black"),
#       axis.title.x = element_blank(),
#       axis.text = element_text(color = "black", size = 17, face = "bold"),
#       axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#       axis.ticks = element_line(color = "black"))

# #visualization of Egg diameters by gametes*site produced --- not used in MS
# ggplot(dataEggs, aes(x=Site, y=AvgEggDiameter_um, color=Sex_Sept2024, fill=Sex_Sept2024)) +
#   scale_fill_manual(values=eggColors)+
#   scale_color_manual(values=eggColors)+
#   #scale_y_continuous(expand = c(0,0), limits = c(0,0.35)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.8 ))+
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   labs(color = "Gametes:", fill = "Gametes:") + 
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Oocyte Diameter (mm)")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))


#### Oocyte Diameter Stats - had to remove site with only one egg producer for ANOVA comparisons ####
dataEggs = subset(dataEggs, Site != "Cristobal Island") 
#remove CI, which is one individual crazy small early stage and uncomparable oocytes

#make lineage a factor for stats
dataEggs$SuspectedLineage = factor(dataEggs$SuspectedLineage)

#assumption checks for ANOVA 
library("performance")
m <- lm(AvgEggDiameter_um~SuspectedLineage+Site*Sex_Sept2024, data = dataEggs)
check_model(m) #little wiggle in variance but we will rely on the robustness of Anova

# m <- lm(AvgEggDiameter_um~Site*Sex_Sept2024, data = dataEggs) #--- did not use for MS
# check_model(m) #better than with lineage, will rely on robustness

hist(dataEggs$AvgEggDiameter_um)#relatively normal
shapiro.test(dataEggs$AvgEggDiameter_um)#p-value = 0.8007, normal

#ANOVA
ANOVAmodel <- lm(AvgEggDiameter_um ~ SuspectedLineage+Site*Sex_Sept2024, data = dataEggs)
anova(ANOVAmodel)
# Analysis of Variance Table
# 
# Response: AvgEggDiameter_um
# Df    Sum Sq   Mean Sq F value   Pr(>F)   
# SuspectedLineage   1 0.0004417 0.0004417  0.4220 0.534158   
# Site               1 0.0023226 0.0023226  2.2187 0.174673   
# Sex_Sept2024       1 0.0121084 0.0121084 11.5668 0.009347 **
# Site:Sex_Sept2024  1 0.0016897 0.0016897  1.6141 0.239619   
# Residuals          8 0.0083746 0.0010468  


#Tukey test
SBPaov <- aov(ANOVAmodel)#
TukeyHSD(SBPaov)
#w/ Lineage
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = ANOVAmodel)
# 
# $SuspectedLineage
# diff         lwr        upr     p adj
# 2-1 -0.001676647 -0.04581657 0.04246328 0.9323529
# 
# $Site
# diff         lwr        upr     p adj
# HP-CI  0.048634762 -0.07100511 0.16827463 0.5867415
# PD-CI  0.024032504 -0.08788035 0.13594535 0.8989635
# SP-CI  0.055141424 -0.06069938 0.17098223 0.4675734
# PD-HP -0.024602259 -0.09786642 0.04866190 0.7130368
# SP-HP  0.006506662 -0.07262767 0.08564100 0.9930992
# SP-PD  0.031108921 -0.03577180 0.09798964 0.4854608
# 
# $Sex_Sept2024
# diff         lwr        upr     p adj
# Oocytes-Both 0.04716912 0.003029192 0.09130904 0.0390585
# 
# $`Site:Sex_Sept2024`
# diff         lwr        upr     p adj
# HP:Both-CI:Both                NA          NA         NA        NA
# PD:Both-CI:Both                NA          NA         NA        NA
# SP:Both-CI:Both                NA          NA         NA        NA
# CI:Oocytes-CI:Both             NA          NA         NA        NA
# HP:Oocytes-CI:Both             NA          NA         NA        NA
# PD:Oocytes-CI:Both             NA          NA         NA        NA
# SP:Oocytes-CI:Both             NA          NA         NA        NA
# PD:Both-HP:Both        0.05033996 -0.08991067 0.19059059 0.8267370
# SP:Both-HP:Both        0.09142000 -0.05172270 0.23456269 0.3018990
# CI:Oocytes-HP:Both     0.03627857 -0.14478421 0.21734135 0.9887143
# HP:Oocytes-HP:Both     0.12737000 -0.02943497 0.28417497 0.1291585
# PD:Oocytes-HP:Both     0.11016665 -0.07089613 0.29122943 0.3465586
# SP:Oocytes-HP:Both             NA          NA         NA        NA
# SP:Both-PD:Both        0.04108004 -0.04480558 0.12696565 0.5877479
# CI:Oocytes-PD:Both    -0.01406139 -0.15431202 0.12618924 0.9998427
# HP:Oocytes-PD:Both     0.07703004 -0.03008815 0.18414823 0.2055091
# PD:Oocytes-PD:Both     0.05982669 -0.08042394 0.20007731 0.6956076
# SP:Oocytes-PD:Both             NA          NA         NA        NA
# CI:Oocytes-SP:Both    -0.05514142 -0.19828412 0.08800127 0.7784113
# HP:Oocytes-SP:Both     0.03595000 -0.07492785 0.14682786 0.8826698
# PD:Oocytes-SP:Both     0.01874665 -0.12439604 0.16188935 0.9991148
# SP:Oocytes-SP:Both             NA          NA         NA        NA
# HP:Oocytes-CI:Oocytes  0.09109143 -0.06571354 0.24789640 0.3913453
# PD:Oocytes-CI:Oocytes  0.07388808 -0.10717470 0.25495086 0.7333859
# SP:Oocytes-CI:Oocytes          NA          NA         NA        NA
# PD:Oocytes-HP:Oocytes -0.01720335 -0.17400832 0.13960162 0.9997162
# SP:Oocytes-HP:Oocytes          NA          NA         NA        NA
# SP:Oocytes-PD:Oocytes          NA          NA         NA        NA



#### Fecundity comparisons Avg Spermatocytes per polyp ####
dataSperm = subset(data, Sex_Sept2024 != "Oocytes") 
dataSperm = subset(dataSperm, AverageSpermaries != "") 

dataSperm$SuspectedLineage <- factor(dataSperm$SuspectedLineage) 

### Fig 3C

##Colors SexType
spermColors = c("#A02B93","#50164A")

#visualization of fecundity by site*SexType produced
ggplot(dataSperm, aes(x=Site, y=AverageSpermaries, shape=SuspectedLineage,color=SuspectedLineage, fill=Sex_Sept2024)) +
  scale_fill_manual(values=spermColors)+
  scale_color_manual(values=LinCol)+
  scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
  geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
  geom_boxplot( outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
  #labs(color = "Gametes:", fill = "Gametes:") + 
  # facet_grid2(SuspectedLineage~.,
  #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
  #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  theme_classic() +
  ylab("Average Spermaries per Polyp")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#visualization of fecundity by gametes produced jitter/pointby lineage
ggplot(dataSperm, aes(x=Sex_Sept2024, y=AverageSpermaries, shape=SuspectedLineage,color=SuspectedLineage, fill=Sex_Sept2024)) +
  scale_fill_manual(values=spermColors)+
  scale_color_manual(values=LinCol)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 40)) +
  geom_jitter(size = 4.5, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
  geom_boxplot(width = 0.6,position = position_dodge(width = 0.8),outlier.shape=1, outlier.size=3, outlier.stroke = 2, color ="black", alpha=0.6)+
  #labs(color = "Gametes:", fill = "Gametes:") + 
  # facet_grid2(SuspectedLineage~.,
  #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
  #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  theme_classic() +
  ylab("Average Spermaries per Polyp")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

# #site colors ---- not used for MS
# SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96","#AABAEE", "#AD961D" )#)"#CFD8F5","#768EA1
# 
# ##AvgSpermaries per polyp by site jitter/points lineage
# ggplot(dataSperm, aes(x=Site, y=AverageSpermaries, shape = SuspectedLineage, color = SuspectedLineage, fill = Site)) +
#   scale_fill_manual(values = SiteColors)+
#   #scale_shape_manual(values = c(19,1))+
#   scale_color_manual(values = LinCol)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,35)) +
#   geom_jitter(size = 4.5,  position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   # facet_grid2(SuspectedLineage~.,
#   #             strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#   #                                  text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Spermaries per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

# ##AvgSpermaries per polyp by site faceted by lineage --- not used for MS
# ggplot(dataSperm, aes(x=Site, y=AverageSpermaries, color = Site, fill = Site)) +
#   scale_fill_manual(values = SiteColors)+
#   scale_shape_manual(values = c(19,1))+
#   scale_color_manual(values = SiteColors)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,35)) +
#   geom_jitter(size = 3,  position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Spermaries per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

# #visualization of fecundity by gametes produced faceted by lineage ---- Not used for MS
# ggplot(dataSperm, aes(x=Sex_Sept2024, y=AverageSpermaries, color=Sex_Sept2024, fill=Sex_Sept2024)) +
#   scale_fill_manual(values=spermColors)+
#   scale_color_manual(values=spermColors)+
#   scale_y_continuous(expand = c(0,0), limits = c(0, 35)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha=0.6)+
#   labs(color = "Gametes:", fill = "Gametes:") + 
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Spermaries per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

# #visualization of fecundity by site*gametes produced faceted by lineage --- Not used in MS
# ggplot(dataSperm, aes(x=Site, y=AverageSpermaries, color=Sex_Sept2024, fill=Sex_Sept2024)) +
#   scale_fill_manual(values=spermColors)+
#   scale_color_manual(values=spermColors)+
#   scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
#   geom_jitter(size = 3, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8 )) +
#   geom_boxplot(outlier.shape=1, outlier.size=3, outlier.stroke = 1, color ="black", alpha = 0.6)+
#   labs(color = "Gametes:", fill = "Gametes:") + 
#   facet_grid2(SuspectedLineage~.,
#               strip = strip_themed(background_y = elem_list_rect(fill = LinCol),
#                                    text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
#   theme_classic() +
#   ylab("Average Spermaries per Polyp")+
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

#### Stats Average Spermatocytes ####
#assumption checks ANOVA
m <- lm(AverageSpermaries~SuspectedLineage+Site*Sex_Sept2024, data = dataSperm)
check_model(m)#decent

hist(dataSperm$AverageSpermaries) #very normal
shapiro.test(dataSperm$AverageSpermaries)#p-value = 0.3427, normal

#ANOVA
ANOVAmodel <- lm(AverageSpermaries ~ SuspectedLineage+Site*Sex_Sept2024, data = dataSperm)
anova(ANOVAmodel)
# Analysis of Variance Table
# 
# Response: AverageSpermaries
#                   Df Sum Sq Mean Sq F value  Pr(>F)  
# SuspectedLineage   1  65.19  65.185  4.2211 0.04408 *
# Site               5  22.84   4.567  0.2958 0.91351  
# Sex_Sept2024       1  89.23  89.228  5.7781 0.01918 *
# Site:Sex_Sept2024  1  82.89  82.893  5.3678 0.02378 *
# Residuals         63 972.88  15.443     


#Tukey test
SBPaov <- aov(ANOVAmodel)
TukeyHSD(SBPaov)
#w/ Lineage
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = ANOVAmodel)
# 
# $SuspectedLineage
# diff       lwr      upr    p adj
# 2-1 1.903732 0.0520729 3.755392 0.044077
# 
# $Site
# diff       lwr      upr     p adj
# PC-SC  0.55838267 -4.157023 5.273788 0.9992947
# CI-SC -0.37927499 -5.666300 4.907750 0.9999398
# HP-SC  0.62418821 -3.960812 5.209189 0.9986158
# PD-SC  0.23036267 -4.146434 4.607160 0.9999870
# SP-SC -0.59431682 -4.745481 3.556847 0.9982372
# CI-PC -0.93765766 -6.629726 4.754411 0.9965630
# HP-PC  0.06580555 -4.980900 5.112511 1.0000000
# PD-PC -0.32802000 -5.186349 4.530309 0.9999554
# SP-PC -1.15269949 -5.808790 3.503391 0.9777834
# HP-CI  1.00346320 -4.581053 6.587980 0.9948289
# PD-CI  0.60963766 -4.805244 6.024520 0.9994483
# SP-CI -0.21504183 -5.449233 5.019149 0.9999962
# PD-HP -0.39382554 -5.125690 4.338039 0.9998745
# SP-HP -1.21850503 -5.742480 3.305470 0.9679546
# SP-PD -0.82467949 -5.137506 3.488147 0.9930934
# 
# $Sex_Sept2024
# diff       lwr      upr     p adj
# Spermaries-Both 2.956939 0.1585737 5.755305 0.0386952
# 
# $`Site:Sex_Sept2024`
# diff         lwr       upr     p adj
# PC:Both-SC:Both                      NA          NA        NA        NA
# CI:Both-SC:Both                      NA          NA        NA        NA
# HP:Both-SC:Both                      NA          NA        NA        NA
# PD:Both-SC:Both                      NA          NA        NA        NA
# SP:Both-SC:Both                      NA          NA        NA        NA
# SC:Spermaries-SC:Both                NA          NA        NA        NA
# PC:Spermaries-SC:Both                NA          NA        NA        NA
# CI:Spermaries-SC:Both                NA          NA        NA        NA
# HP:Spermaries-SC:Both                NA          NA        NA        NA
# PD:Spermaries-SC:Both                NA          NA        NA        NA
# SP:Spermaries-SC:Both                NA          NA        NA        NA
# CI:Both-PC:Both                      NA          NA        NA        NA
# HP:Both-PC:Both                      NA          NA        NA        NA
# PD:Both-PC:Both                      NA          NA        NA        NA
# SP:Both-PC:Both                      NA          NA        NA        NA
# SC:Spermaries-PC:Both                NA          NA        NA        NA
# PC:Spermaries-PC:Both                NA          NA        NA        NA
# CI:Spermaries-PC:Both                NA          NA        NA        NA
# HP:Spermaries-PC:Both                NA          NA        NA        NA
# PD:Spermaries-PC:Both                NA          NA        NA        NA
# SP:Spermaries-PC:Both                NA          NA        NA        NA
# HP:Both-CI:Both                      NA          NA        NA        NA
# PD:Both-CI:Both                      NA          NA        NA        NA
# SP:Both-CI:Both                      NA          NA        NA        NA
# SC:Spermaries-CI:Both                NA          NA        NA        NA
# PC:Spermaries-CI:Both                NA          NA        NA        NA
# CI:Spermaries-CI:Both                NA          NA        NA        NA
# HP:Spermaries-CI:Both                NA          NA        NA        NA
# PD:Spermaries-CI:Both                NA          NA        NA        NA
# SP:Spermaries-CI:Both                NA          NA        NA        NA
# PD:Both-HP:Both                      NA          NA        NA        NA
# SP:Both-HP:Both                      NA          NA        NA        NA
# SC:Spermaries-HP:Both                NA          NA        NA        NA
# PC:Spermaries-HP:Both                NA          NA        NA        NA
# CI:Spermaries-HP:Both                NA          NA        NA        NA
# HP:Spermaries-HP:Both                NA          NA        NA        NA
# PD:Spermaries-HP:Both                NA          NA        NA        NA
# SP:Spermaries-HP:Both                NA          NA        NA        NA
# SP:Both-PD:Both             -6.39000000 -15.3363052  2.556305 0.4059522
# SC:Spermaries-PD:Both       -0.11651652  -7.0033855  6.770352 1.0000000
# PC:Spermaries-PD:Both        0.44186615  -6.8627615  7.746494 1.0000000
# CI:Spermaries-PD:Both       -0.49579151  -8.3047669  7.313184 1.0000000
# HP:Spermaries-PD:Both        0.50767170  -6.6854284  7.700772 1.0000000
# PD:Spermaries-PD:Both        0.18500000  -7.4178975  7.787897 1.0000000
# SP:Spermaries-PD:Both        1.18222222  -5.9165998  8.281044 0.9999882
# SC:Spermaries-SP:Both        6.27348348  -1.2313080 13.778275 0.1900980
# PC:Spermaries-SP:Both        6.83186615  -1.0580334 14.721766 0.1526561
# CI:Spermaries-SP:Both        5.89420849  -2.4648003 14.253217 0.4258000
# HP:Spermaries-SP:Both        6.89767170  -0.8890875 14.684431 0.1310926
# PD:Spermaries-SP:Both        6.57500000  -1.5918220 14.741822 0.2352140
# SP:Spermaries-SP:Both        7.57222222  -0.1275314 15.271976 0.0580211 * ~ almost
# PC:Spermaries-SC:Spermaries  0.55838267  -4.8861653  6.002931 0.9999999
# CI:Spermaries-SC:Spermaries -0.37927499  -6.4838319  5.725282 1.0000000
# HP:Spermaries-SC:Spermaries  0.62418821  -4.6697901  5.918167 0.9999997
# PD:Spermaries-SC:Spermaries  0.30151652  -5.5371107  6.140144 1.0000000
# SP:Spermaries-SC:Spermaries  1.29873874  -3.8664130  6.463890 0.9993272
# CI:Spermaries-PC:Spermaries -0.93765766  -7.5098898  5.634574 0.9999976
# HP:Spermaries-PC:Spermaries  0.06580555  -5.7612715  5.892883 1.0000000
# PD:Spermaries-PC:Spermaries -0.25686615  -6.5828593  6.069127 1.0000000
# SP:Spermaries-PC:Spermaries  0.74035607  -4.9699340  6.450646 0.9999991
# HP:Spermaries-CI:Spermaries  1.00346320  -5.4445860  7.451512 0.9999941
# PD:Spermaries-CI:Spermaries  0.68079151  -6.2214328  7.583016 1.0000000
# SP:Spermaries-CI:Spermaries  1.67801373  -4.6646927  8.020720 0.9989244
# PD:Spermaries-HP:Spermaries -0.32267170  -6.5195493  5.874206 1.0000000
# SP:Spermaries-HP:Spermaries  0.67455052  -4.8923621  6.241463 0.9999996
# SP:Spermaries-PD:Spermaries  0.99722222  -5.0899675  7.084412 0.9999901



#### Comparison of SexType by size of colony ####
#import metadata again if not already imported, reorder and rename (line 531)

#### Colony Sex Type by Size ####
# table of frequencies sex and size
GamSizeData = table(data$Sex_Sept2024,data$ColonySize_cm) 

# display  
GamSizeData #table of counts by size and site
rownames(GamSizeData)[rownames(GamSizeData) == ""] = "Unknown"

# #Preliminary graph of all corals - only works with comparisons of two variables
# barplot(GamSizeData, main='Size by SexType', legend.text = rownames(GamSizeData))
# mosaicplot(GamSizeData)

# convert dataframe to tablewith site and lineage characteristics included
GamSizeFrame <- as.data.frame(GamSizeData)
GamSizeFrame <- rename(GamSizeFrame, SexType = Var1, Size = Var2)
GamSizeFrame$SexType <- factor(GamSizeFrame$SexType)
GamSizeFrame$Freq <- as.numeric(GamSizeFrame$Freq)

#filter to non-zero frequencies
#only run for figure
GamSizeFrame2 <- subset(GamSizeFrame, Freq != "0")

#Pretty plot
#reorder variables for figures
f=c('S','M', 'L', 'XL')
GamSizeFrame2 <- within(GamSizeFrame2, Size<- factor(Size, levels=f))

f=c('Spermatocytes','Oocytes','Both', 'Unknown')
GamSizeFrame2 <- within(GamSizeFrame2, SexType<- factor(SexType, levels=f))


# ###Coral sizes by sex type --- All --- Not used in MS
# ggplot(GamSizeFrame2, aes(x = Size, y= Freq, fill = SexType)) + 
#   scale_y_continuous(expand = c(0,0), limits = c(0,65)) +
#   geom_bar(stat="identity", color = "white")+
#   scale_fill_manual(values = c("#50164A","#E59EDD","#A02B93","grey50"))+
#   geom_text(label = GamSizeFrame2$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
#   theme_classic() +
#   theme(legend.position="top", 
#         legend.title=element_text(size=14, face = "bold", color = "black"), 
#         legend.text=element_text(size=14, face = "bold", color="black"),
#         axis.title.x = element_blank(),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))

#now for stats and lineage/site comparisons
# table of frequencies
GamSizeData = table(data$Sex_Sept2024,data$ColonySize,
                    #data$Site,
                    data$SuspectedLineage
                    ) 
# display  
GamSizeData #table of counts by size and site
rownames(GamSizeData)[rownames(GamSizeData) == ""] = "Unknown"

GamSizeFrame <- as.data.frame(GamSizeData)
GamSizeFrame <- rename(GamSizeFrame, SexType = Var1, Size = Var2, 
                       #Site = Var3, 
                       Lineage = Var3
                       )
#GamSizeFrame$Site <- factor(GamSizeFrame$Site)
GamSizeFrame$SexType <- factor(GamSizeFrame$SexType)
GamSizeFrame$Freq <- as.numeric(GamSizeFrame$Freq)
GamSizeFrame$Lineage <- as.numeric(GamSizeFrame$Lineage)

#filter unknowns out
GamSizeFrame1 <- subset(GamSizeFrame, SexType != "Unknown")

#filter to non-zero frequencies
#only run for figure
GamSizeFrame2 <- subset(GamSizeFrame1, Freq != "0")

#reorder variables
f=c('S','M', 'L', 'XL')
GamSizeFrame2 <- within(GamSizeFrame2, Size<- factor(Size, levels=f))

f=c('Spermatocytes', 'Oocytes', 'Both','Unknown')
GamSizeFrame2 <- within(GamSizeFrame2, SexType<- factor(SexType, levels=f))

#Lineage colors
LinCol = c("#3f007d", "#807dba" )
#Pretty plot with size by SexType --- faceted by lineages
ggplot(GamSizeFrame2, aes(x = Size, y= Freq, fill = SexType)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,30)) +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("#50164A","#E59EDD","#A02B93","grey50"))+
  geom_text(label = GamSizeFrame2$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  
  theme_classic() +
  ylab("Sex Type Frequency")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))


#### Stats SexType by Size ####
### Chi Square --- kept invalidating assumption of >5 counts in most groups, so used Fisher's exact instead (very conservative)

#did not use collapsed version of data for MS
# #Make SexTypeCollapsed column with SexType collapsed into "HasEggs" or "No Eggs" to improve power
# GamSizeFrame1$SexTypeCollapsed <- as.character(GamSizeFrame1$SexType)
# GamSizeFrame1$SexTypeCollapsed[GamSizeFrame1$SexTypeCollapsed %in% c("Oocytes", "Both")] <- "HasEggs"
# GamSizeFrame1$SexTypeCollapsed[GamSizeFrame1$SexTypeCollapsed == "Spermaries"] <- "NoEggs"
# GamSizeFrame1$SexTypeCollapsed <- factor(GamSizeFrame1$SexTypeCollapsed, levels = c("HasEggs", "NoEggs"))

# Sex Type Differ by Size?
GamSizeFrame1$SexType <- droplevels(GamSizeFrame1$SexType)## drop "Unknown" zero row for analysis
Gametes_by_Size <- xtabs(Freq ~ SexType + Size, data = GamSizeFrame1)
#chisq.test(Gametes_by_Size)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  Gametes_by_Size
# X-squared = 10.845, df = 6, p-value = 0.09328
fisher.test(Gametes_by_Size, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Size
# p-value = 0.1861
# alternative hypothesis: two.sided

# Sex Type Differ by Lineage?
GamSizeFrame1$SexType <- droplevels(GamSizeFrame1$SexType) ## drop "Unknown" zero row for analysis
Gametes_by_Lineage <- xtabs(Freq ~ SexType + Lineage, data = GamSizeFrame1)
#chisq.test(Gametes_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test # Sperm/eggs/both
# data:  Gametes_by_Lineage
# X-squared = 7.5229, df = 2, p-value = 0.02325 *
fisher.test(Gametes_by_Lineage, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Lineage
# p-value = 0.01951
# alternative hypothesis: two.sided

# Lineage 1 to Lineage 2,  differ by Size?
GamSizeFrame1$SexType <- droplevels(GamSizeFrame1$SexType)## drop "Unknown" zero row for analysis
Gametes_by_Size <- xtabs(Freq ~ Lineage + Size, data = GamSizeFrame1)
#chisq.test(Gametes_by_Size)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  Gametes_by_Size
# X-squared = 10.845, df = 6, p-value = 0.09328
fisher.test(Gametes_by_Size, workspace = 2e7)
# Fisher's Exact Test for Count Data
# 
# data:  Gametes_by_Size
# p-value = 1.96e-05
# alternative hypothesis: two.sided

# Sex Type Differ by Size?
# Gametes_by_Size <- xtabs(Freq ~ GametesCollapsed + Size, data = GamSizeFrame1)
# #chisq.test(Gametes_by_Size)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test
# # data:  Gametes_by_Size
# # X-squared = 4.986, df = 3, p-value = 0.1728
# fisher.test(Gametes_by_Size, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Size
# # p-value = 0.2001
# # alternative hypothesis: two.sided

# Sex Type Differ by Lineage?
# Gametes_by_Lineage <- xtabs(Freq ~ GametesCollapsed + Lineage, data = GamSizeFrame1)
# #chisq.test(Gametes_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test with Yates' continuity correction #Has Eggs/No Eggs
# # data:  Gametes_by_Lineage
# # X-squared = 2.677, df = 1, p-value = 0.1018
# fisher.test(Gametes_by_Lineage, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Lineage
# # p-value = 0.0885
# # alternative hypothesis: true odds ratio is not equal to 1

### Now within lineage across sites
#Lineage 1 comparisons across sites
Lin1 <- subset(GamSizeFrame1, Lineage == "1")

Gametes_by_Size1 <- xtabs(Freq ~ SexType  + Size, data = Lin1)
# Drop empty rows and columns from the contingency table (Sites without L1)
#chisq.test(Gametes_by_Size1) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  Gametes_by_Size1
# X-squared = 7.3203, df = 6, p-value = 0.2922
fisher.test(Gametes_by_Size1, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Size1
# p-value = 0.489
# alternative hypothesis: two.sided

# Gametes_by_Size1 <- xtabs(Freq ~ GametesCollapsed  + Size, data = Lin1)
# #chisq.test(Gametes_by_Size1) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test
# # data:  Gametes_by_Size1
# # X-squared = 7.3203, df = 3, p-value = 0.06236
# fisher.test(Gametes_by_Size1, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Size1
# # p-value = 0.1118
# # alternative hypothesis: two.sided


#Lineage 2 comparisons across sites
Lin2<- subset(GamSizeFrame1, Lineage == "2")

Gametes_by_Size2 <- xtabs(Freq ~ SexType + Size, data = Lin2)
#chisq.test(Gametes_by_Size2) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  Gametes_by_Size2
# X-squared = 5.2285, df = 6, p-value = 0.5149
fisher.test(Gametes_by_Size2, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_Size2
# p-value = 0.6081
# alternative hypothesis: two.sided

# Gametes_by_Size2 <- xtabs(Freq ~ GametesCollapsed + Size, data = Lin2)
# chisq.test(Gametes_by_Size2)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# # Pearson's Chi-squared test
# # data:  Gametes_by_Size2
# # X-squared = 1.5407, df = 3, p-value = 0.6729
# fisher.test(Gametes_by_Size2, workspace = 2e7)
# # Fisher's Exact Test for Count Data
# # data:  Gametes_by_Size2
# # p-value = 0.6683
# # alternative hypothesis: two.sided



#### Colony Size by Site comparisons ####
### Fig S2

# table of frequencies
GamSizeSiteData = table(data$ColonySize,
                        data$Site,
                        data$SuspectedLineage
) 
# display  
GamSizeSiteData #table of counts by size and site
#rownames(GamSizeSiteData)[rownames(GamSizeSiteData) == ""] = "Unknown"

GamSizeSiteFrame <- as.data.frame(GamSizeSiteData)
GamSizeSiteFrame <- rename(GamSizeSiteFrame, Size = Var1, Site = Var2, 
                           Lineage = Var3
)
GamSizeSiteFrame$Site <- factor(GamSizeSiteFrame$Site)
GamSizeSiteFrame$Freq <- as.numeric(GamSizeSiteFrame$Freq)
GamSizeSiteFrame$Lineage <- as.factor(GamSizeSiteFrame$Lineage)

#filter unknowns out
#GamSizeSiteFrame1 <- subset(GamSizeSiteFrame, Gametes != "Unknown")

#filter to non-zero frequencies
#only run for figure
GamSizeSiteFrame2 <- subset(GamSizeSiteFrame, Freq != "0")

#reorder variables
f=c('S','M', 'L', 'XL')
GamSizeSiteFrame2 <- within(GamSizeSiteFrame2, Size<- factor(Size, levels=f))

f=c('Spermatocytes', 'Oocytes', 'Both','Unknown')
GamSizeSiteFrame2 <- within(GamSizeSiteFrame2, SexType<- factor(SexType, levels=f))

#Lineage colors
LinCol = c("#3f007d", "#807dba" )
#site colors
SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96","#AABAEE", "#AD961D" )#)"#CFD8F5","#768EA1

#Pretty plot with size by gametes --- faceted by lineages
ggplot(GamSizeSiteFrame2, aes(x = Size, y= Freq, fill = Site)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,30)) +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = SiteColors)+
  geom_text(label = GamSizeSiteFrame2$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  
  theme_classic() +
  ylab("Colony Size Frequency")+
  theme(legend.position="top", 
        legend.title=element_text(size=14, face = "bold", color = "black"), 
        legend.text=element_text(size=14, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#### Stats for size by sites ####

# Does size differ by site?
#GamSizeSiteFrame$Gametes <- droplevels(GamSizeSiteFrame$Gametes)## drop "Unknown" zero row for analysis
Gametes_by_SizeSite <- xtabs(Freq ~ Size + Site, data = GamSizeSiteFrame)
fisher.test(Gametes_by_SizeSite, workspace = 2e8) #had to increase workspace for large contigency 4x6 table
# Fisher's Exact Test for Count Data
# data:  Gametes_by_SizeSite
# p-value = 0.008754
# alternative hypothesis: two.sided

### Now within lineage across sites
#Lineage 1 comparisons across sites
Lin1 <- subset(GamSizeSiteFrame, Lineage == "1")

Gametes_by_SizeSite1 <- xtabs(Freq ~ Size + Site, data = Lin1)
# Drop empty rows and columns from the contingency table (Sizes without Oocytes)
Gametes_by_SizeSite1 <- Gametes_by_SizeSite1[rowSums(Gametes_by_SizeSite1) > 0, colSums(Gametes_by_SizeSite1) > 0]
fisher.test(Gametes_by_SizeSite1, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_SizeSite1
# p-value = 0.1576
# alternative hypothesis: two.sided

#Lineage 2 comparisons across sites
Lin2 <- subset(GamSizeSiteFrame, Lineage == "2")

Gametes_by_SizeSite2 <- xtabs(Freq ~ Size + Site, data = Lin2)
# Drop empty rows and columns from the contingency table (Sizes without Oocytes)
Gametes_by_SizeSite2 <- Gametes_by_SizeSite2[rowSums(Gametes_by_SizeSite2) > 0, colSums(Gametes_by_SizeSite2) > 0]
fisher.test(Gametes_by_SizeSite2, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  Gametes_by_SizeSite2
# p-value = 0.4773
# alternative hypothesis: two.sided


####  Colony Size by gamete stage comparisons ####

### Oocyte Stages by Size ####
# table of frequencies
GamSizeData3 = table(data$ColonySize_cm,  data$OocyteStage, data$SuspectedLineage) 

# display  
GamSizeData3 #table of counts by size and site
rownames(GamSizeData3)[rownames(GamSizeData3) == ""] = "Unknown"

# convert dataframe to tablewith site and lineage characteristics included
GamSizeFrame3 <- as.data.frame(GamSizeData3)
GamSizeFrame3 <- rename(GamSizeFrame3,  Size = Var1, OoStage = Var2, Lineage = Var3)
GamSizeFrame3$OoStage <- factor(GamSizeFrame3$OoStage)
GamSizeFrame3$Lineage <- factor(GamSizeFrame3$Lineage)
GamSizeFrame3$Freq <- as.numeric(GamSizeFrame3$Freq)


#filter to non-zero frequencies
#only run for figure
#GamSizeFrame3 <- subset(GamSizeFrame3, Freq != "0") #only run for SpStages - leave zeroes for OoStages figure

#Pretty plot
#reorder variables
f=c('S','M', 'L', 'XL')
GamSizeFrame3 <- within(GamSizeFrame3, Size<- factor(Size, levels=f))

#remove unknown / NA colonies
GamSizeFrame3  <- subset(GamSizeFrame3, OoStage != "Unknown")

#only run for figure
GamSizeFrameOFig <- subset(GamSizeFrame3, OoStage != "")

### Fig 2G
###Coral sizes by Oocyte Stage --- All
ggplot(GamSizeFrameOFig, aes(x = Size, y= Freq, fill = OoStage)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("#930516","#F06F3C", "#FFA875","grey50", "black" ))+
  geom_text(label = GamSizeFrameOFig$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  
  ylab("Oocyte Stage Frequency")+
  labs(fill = "Stages")+
  theme_classic() +
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#### Stats Oocyte Stages by Size ####
# oocyte stage differ by Size?
OoStage_by_Size <- xtabs(Freq ~ OoStage + Size, data = GamSizeFrame3)
#remove Unknown/NA blank row of counts from table
OoStage_by_Size <- OoStage_by_Size[rownames(OoStage_by_Size) != "", ]
# Drop empty rows and columns from the contingency table (Sizes without Oocytes)
OoStage_by_Size <- OoStage_by_Size[rowSums(OoStage_by_Size) > 0, colSums(OoStage_by_Size) > 0]
#chisq.test(OoStage_by_Size)
# Pearson's Chi-squared test
# data:  OoStage_by_Size
# X-squared = 3.5714, df = 4, p-value = 0.4671
fisher.test(OoStage_by_Size, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  OoStage_by_Size
# p-value = 0.5268
# alternative hypothesis: two.sided

# oocyte stage differ by Lineage?
OoStage_by_Lineage <- xtabs(Freq ~ OoStage + Lineage, data = GamSizeFrame3)
#remove Unknown/NA blank row of counts from table
OoStage_by_Lineage <- OoStage_by_Lineage[rownames(OoStage_by_Lineage) != "", ]
# Drop empty rows and columns from the contingency table (Sizes without Oocytes)
#OoStage_by_Size <- OoStage_by_Size[rowSums(OoStage_by_Size) > 0, colSums(OoStage_by_Size) > 0]
#chisq.test(OoStage_by_Lineage)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  OoStage_by_Lineage
# X-squared = 3.4286, df = 2, p-value = 0.1801
fisher.test(OoStage_by_Lineage, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  OoStage_by_Lineage
# p-value = 0.3636
# alternative hypothesis: two.sided

### Now within lineage across sites
#Lineage 1 comparisons across sites  ---- Not suitable for testing because eggs on observed in L individuals (no comparison)
# Lin1 <- subset(GamSizeFrame3, Lineage == "1")
# 
# OoStage_by_Size1 <- xtabs(Freq ~ OoStage  + Size, data = Lin1)
# #remove Unknown/NA blank row of counts from table
# OoStage_by_Size1 <- OoStage_by_Size1[rownames(OoStage_by_Size1) != "", ]
# # Drop empty rows and columns from the contingency table (Sizes without Oocytes)
# OoStage_by_Size1 <- OoStage_by_Size1[rowSums(OoStage_by_Size1) > 0, colSums(OoStage_by_Size1) > 0]
# chisq.test(OoStage_by_Size1)

#Lineage 2 comparisons across sites --- low frequencies across sites, likely not a dependable p-value
Lin2<- subset(GamSizeFrame3, Lineage == "2")

OoStage_by_Size2 <- xtabs(Freq ~ OoStage + Size, data = Lin2)
#remove Unknown/NA blank row of counts from table
OoStage_by_Size2 <- OoStage_by_Size2[rownames(OoStage_by_Size2) != "", ]
# Drop empty rows and columns from the contingency table (Sizes without Oocytes)
OoStage_by_Size2 <- OoStage_by_Size2[rowSums(OoStage_by_Size2) > 0, colSums(OoStage_by_Size2) > 0]
#chisq.test(OoStage_by_Size2)#not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  OoStage_by_Size2
# X-squared = 1.3194, df = 2, p-value = 0.517
fisher.test(OoStage_by_Size2, workspace = 2e7)
# Fisher's Exact Test for Count Data
# data:  OoStage_by_Size2
# p-value = 0.7429
# alternative hypothesis: two.sided


#### Spermary Stages by Size ####
# table of frequencies
GamSizeData4 = table( data$SpermaryStage,  data$ColonySize_cm,data$SuspectedLineage) 

# display  
GamSizeData4 #table of counts by size and site
rownames(GamSizeData4)[rownames(GamSizeData4) == ""] = "Unknown"

# convert dataframe to tablewith site and lineage characteristics included
GamSizeFrame4 <- as.data.frame(GamSizeData4)
GamSizeFrame4 <- rename(GamSizeFrame4,  SpStage = Var1, Size = Var2, Lineage = Var3)
GamSizeFrame4$SpStage <- factor(GamSizeFrame4$SpStage)
GamSizeFrame4$Lineage <- factor(GamSizeFrame4$Lineage)
GamSizeFrame4$Freq <- as.numeric(GamSizeFrame4$Freq)


#filter to non-zero frequencies
#only run for figure
GamSizeFrame4 <- subset(GamSizeFrame4, Freq != "0") #only run for SpStages figure

#Pretty plot
#reorder variables
f=c('S','M', 'L', 'XL')
GamSizeFrame4 <- within(GamSizeFrame4, Size<- factor(Size, levels=f))


#only run for figure
GamSizeFrameSFig <- subset(GamSizeFrame4, SpStage != "Unknown")
GamSizeFrameSFig <- subset(GamSizeFrameSFig, SpStage != "")

### Fig 2I
###Coral sizes by Spermary Stage --- All
ggplot(GamSizeFrameSFig, aes(x = Size, y= Freq, fill = SpStage)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
  geom_bar(stat="identity", color = "black")+
  scale_fill_manual(values = c("#8be0fc", "#67a9cf","#2166ac","#01304C", "black" ))+
  geom_text(label = GamSizeFrameSFig$Freq,size = 6, color = "white",position = position_stack(vjust = 0.5), fontface = "bold")+
  facet_grid2(Lineage~., 
              strip = strip_themed(background_y = elem_list_rect(fill = LinCol), 
                                   text_y = elem_list_text(size = 17, face = "bold", color = "white")))+
  
  theme_classic() +
  ylab("Spermatocyte Stage Frequency")+
  labs(fill = "Stages")+
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))

#Stats Stages by Size
# spermatocyte stage differ by Size?
SpStage_by_Size <- xtabs(Freq ~ SpStage + Size, data = GamSizeFrame4)
#remove Unknown/NA blank row of counts from table
SpStage_by_Size <- SpStage_by_Size[rownames(SpStage_by_Size) != "Unknown", ]
# Drop empty rows and columns from the contingency table (Sizes without Spcytes)
#SpStage_by_Size <- SpStage_by_Size[rowSums(SpStage_by_Size) > 0, colSums(SpStage_by_Size) > 0]
#chisq.test(SpStage_by_Size) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStage_by_Size
# X-squared = 12.557, df = 9, p-value = 0.1837
fisher.test(SpStage_by_Size, workspace = 2e7) #needed more workspace due to size of contingency table
# Fisher's Exact Test for Count Data
# data:  SpStage_by_Size
# p-value = 0.1877
# alternative hypothesis: two.sided

#spermatocyte stage differ by Lineage?
SpStage_by_Lineage <- xtabs(Freq ~ SpStage + Lineage, data = GamSizeFrame4)
#remove Unknown/NA blank row of counts from table
SpStage_by_Lineage <- SpStage_by_Lineage[rownames(SpStage_by_Lineage) != "Unknown", ]
# Drop empty rows and columns from the contingency table (Sizes without Spcytes)
#SpStage_by_Size <- SpStage_by_Size[rowSums(SpStage_by_Size) > 0, colSums(SpStage_by_Size) > 0]
#chisq.test(SpStage_by_Lineage) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStage_by_Lineage
# X-squared = 8.8391, df = 3, p-value = 0.03151
fisher.test(SpStage_by_Lineage)
# Fisher's Exact Test for Count Data
# data:  SpStage_by_Lineage
# p-value = 0.03177
# alternative hypothesis: two.sided


### Now within lineage across sites
#Lineage 1 comparisons across sites
Lin1 <- subset(GamSizeFrame4, Lineage == "1")

SpStage_by_Size1 <- xtabs(Freq ~ SpStage  + Size, data = Lin1)
#remove Unknown/NA blank row of counts from table
SpStage_by_Size1 <- SpStage_by_Size1[rownames(SpStage_by_Size1) != "Unknown", ]
# Drop empty rows and columns from the contingency table (Sizes without Spcytes)
#SpStage_by_Size1 <- SpStage_by_Size1[rowSums(SpStage_by_Size1) > 0, colSums(SpStage_by_Size1) > 0]
#chisq.test(SpStage_by_Size1) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStage_by_Size1
# X-squared = 7.5057, df = 9, p-value = 0.5846
fisher.test(SpStage_by_Size1)
# Fisher's Exact Test for Count Data
# data:  SpStage_by_Size1
# p-value = 0.7118
# alternative hypothesis: two.sided

#Lineage 2 comparisons across sites --- low frequencies across sites, likely not a dependable p-value
Lin2<- subset(GamSizeFrame4, Lineage == "2")

SpStage_by_Size2 <- xtabs(Freq ~ SpStage + Size, data = Lin2)
#remove Unknown/NA blank row of counts from table
SpStage_by_Size2 <- SpStage_by_Size2[rownames(SpStage_by_Size2) != "Unknown", ]
#remove XL size and Stage IV spermaries cause of too many zero counts, so we meet chi-square assumptions
#SpStage_by_Size2 <- SpStage_by_Size2[rownames(SpStage_by_Size2) != "IV", colnames(SpStage_by_Size2) != "XL"]
#chisq.test(SpStage_by_Size2) #not reliable with small sample sizes like mine -- decided to use fisher's exact test instead
# Pearson's Chi-squared test
# data:  SpStage_by_Size2
# X-squared = 11.911, df = 9, p-value = 0.2184
fisher.test(SpStage_by_Size2)
# Fisher's Exact Test for Count Data
# data:  SpStage_by_Size2
# p-value = 0.1842
# alternative hypothesis: two.sided


##### Bathymetry Visual Analyses ####

library(tidyverse) #For Bathymetry plots
library(ggspatial)
library(scales)
library(grid)
library(gridExtra)
#library(cowplot)
library(dplyr)
library(sf)
library(terra)
library(fields)  # for Tps()
library(ggplot2)
library(ggh4x)
library(ggmap)
library(spatstat.geom) #for spatial analyses
library(spatstat.explore)
library(spatstat)


# Load your GPS + depth data
#if not already done above at line 531
data <- read.csv("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/Panama_SSID_2024-2025_Metadata.csv")

data <- data %>%
  mutate(Sex_Sept2024 = recode(Sex_Sept2024,
                               "F" = "Oocytes",
                               "M" = "Spermatocytes",
                               "H" = "Both"))
#remove corals with bad coordinates, one PD colony
metadata <- subset(data, SampleID != "175")

# Split master metadata up for site specific analyses
# Filter to a specific site 
#--- NEED TO RUN EACH SITE ALL THE WAY THROUGH TO MAP CREATION ANALYSIS BEFORE DOING THE NEXT SITE ----
 site_name <-                     #Site for analysis, change as needed
   #Sites with egg producers         
             #"Hospital Point"
             "Punta Donato"
             #"STRI Point"  ###Need to adjust ggmap zoom to 19 for this site!!!!


# 1. Subset and convert to sf object
SiteMeta <- metadata %>% filter(Site == site_name)
bath_data <- SiteMeta %>% 
  dplyr::select(Lon, Lat, Depth_m) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%  # WGS84
  st_transform(crs = 32617)  # UTM Zone 17N

# 2. Create regular grid over the bounding box
bbox <- st_bbox(bath_data)
x_seq <- seq(bbox["xmin"], bbox["xmax"], length.out = 300)
y_seq <- seq(bbox["ymin"], bbox["ymax"], length.out = 300)
grid <- expand.grid(x = x_seq, y = y_seq)
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32617)

# 3. Fit thin plate spline to bathymetry data
coords_matrix <- st_coordinates(bath_data)
bath_spline <- Tps(coords_matrix, bath_data$Depth_m)

# 4. Predict bathymetry values at grid points
grid_coords <- st_coordinates(grid_sf)
grid$Depth_pred <- predict(bath_spline, grid_coords)

# 5. Convert to terra raster
depth_rast <- terra::rast(data.frame(x = grid_coords[,1], 
                                     y = grid_coords[,2], 
                                     z = grid$Depth_pred),
                          type = "xyz",
                          crs = "EPSG:32617")

# Reproject bathymetry raster from UTM back to lon/lat (WGS84)
depth_rast_ll <- terra::project(depth_rast, "EPSG:4326")

# Convert to data frame for plotting
depth_df_ll <- as.data.frame(depth_rast_ll, xy = TRUE)
 names(depth_df_ll) <- c("lon", "lat", "depth")

# Convert your data to sf for plotting
depth_sf <- st_as_sf(depth_df_ll, coords = c("lon", "lat"), crs = 4326)

# Back to df with geometry split (needed for ggplot raster-style plotting)
depth_df_ll <- cbind(st_coordinates(depth_sf), depth = depth_sf$depth)
colnames(depth_df_ll)[1:2] <- c("lon", "lat")

# Inital Plot with coord_sf() and proper CRS
ggplot() +
  geom_raster(data = depth_df_ll, aes(x = lon, y = lat, fill = depth)) +
  scale_fill_viridis_c(trans = 'reverse',option = "plasma",name = "Depth (m)", direction = 1) +
  geom_point(data = SiteMeta, aes(x = Lon, y = Lat), 
             shape = 21, fill = "white", color = "black", size = 3) +
  geom_text(data = SiteMeta, aes(x = Lon, y = Lat, label = SampleID), 
            size = 5, hjust = -0.2,  color = "black", fontface = "bold") +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 2,
                   text_cex = 1.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         line_col = "white",
                         style = north_arrow_fancy_orienteering(
                           fill = c("white", "black"),  # Arrow fill colors
                           line_col = "white",          # Arrow border
                           text_col = "white")) +
  coord_sf(crs = st_crs(4326), datum = st_crs(4326), label_graticule = "SW") +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text = element_text(color = "black", face = "bold", size = 17),
        axis.title = element_text(color = "black", face = "bold", size = 17),
        legend.title = element_text(color = "black", face = "bold", size = 17),
        legend.text = element_text(color = "black", face = "bold", size = 17),
        plot.title = element_text(face = "bold", size = 17))+
  labs( x = "Longitude",y = "Latitude")

###ggmap version of plot with bathymetry contours
#I think I like the raster version below side by side with the sample map better
#added sex, size characteristics
# Get map center
lat <- mean(SiteMeta$Lat)
lon <- mean(SiteMeta$Lon)

#Google maps needs an API key to use - free usage up to a quota then charges (get ~50,000 static maps a month)
gps.com <- gps[complete.cases(gps$Lat),]
#Have to follow rules of Google Maps attribution to use in articles/presentations
#https://about.google/brand-resource-center/products-and-services/geo-guidelines/#google-map

#API Key needed for running ggmap- may cost something if go over quota; https://cloud.google.com/maps-platform/terms
#for information about API Keys and ggmap use: https://cran.r-project.org/web/packages/ggmap/readme/README.html
ggmap::register_google(key="#InsertGoogleAPIKeyHere#",write=TRUE)

#Fetch Google satellite basemap (used zoom 21  PD and zoom 21 HP, and zoom 19 for SP)
map_bg <- get_googlemap(center = c(lon = lon, lat = lat),
                        zoom = 21, #change zoom based on site
                        maptype = "satellite",
                        scale = 2) #High resolution

#define orders for figures
f=c('Spermatocytes','Oocytes','Both')
SiteMeta<- within(SiteMeta, Sex_Sept2024 <- factor(Sex_Sept2024, levels=f))

f=c('S','M','L', 'XL')
SiteMeta<- within(SiteMeta, ColonySize_cm <- factor(ColonySize_cm, levels=f))

ggmap(map_bg) +
  geom_raster(data = depth_df_ll, aes(x = lon, y = lat, fill = depth), alpha = 0.75) +
  scale_fill_viridis_c(trans = 'reverse', direction = 1, option = "plasma", name = "Depth (m)") +
  geom_point(data = SiteMeta, aes(x = Lon, y = Lat, color = Sex_Sept2024,
                                 # shape = ColonySize_cm
                                  ),  size = 8)+
  scale_color_manual(values = c("Oocytes" = "#E59EDD", "Spermatocytes" = "#50164A", "Both" = "#A02B93")) +
  #scale_shape_manual(values = c("S" = 18, "M" = 19, "L" = 15, "XL" = 17)) +
  annotation_scale(location ="bl",pad_y = unit(0.35, "in"), width_hint = 0.2,
                   bar_cols = c("white","black"),text_col = "white",line_width = 2,
                   text_cex = 1.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         line_col = "white",
                         style = north_arrow_fancy_orienteering(
                           fill = c("white", "black"),  # Arrow fill colors
                           line_col = "white",          # Arrow border
                           text_col = "white")) +           # 'N' text color
  coord_sf(crs = st_crs(4326), datum = st_crs(4326), label_graticule = "SW") +
  theme(axis.text = element_text(color = "black", face = "bold", size = 17),
        axis.title = element_text(color = "black", face = "bold", size = 17),
        legend.title = element_text(color = "black", face = "bold", size = 17),
        legend.text = element_text(color = "black", face = "bold", size = 17),
        plot.title = element_text(face = "bold", size = 17),
        axis.ticks = element_line(color = "black"))+ 
  labs(title=site_name, x = "Longitude",y = "Latitude")

ggmap(map_bg) +
  geom_raster(data = depth_df_ll, aes(x = lon, y = lat, fill = depth), alpha = 0.75) +
  scale_fill_viridis_c(trans = 'reverse', direction = 1, option = "plasma", name = "Depth (m)") +
  geom_point(data = SiteMeta, aes(x = Lon, y = Lat, color = Sex_Sept2024), size = 8) +
  scale_color_manual(values = c("Oocytes" = "#E59EDD", "Spermatocytes" = "#50164A", "Both" = "#A02B93")) +
  annotation_scale(location ="bl", pad_y = unit(0.35, "in"), width_hint = 0.2,
                   bar_cols = c("white", "black"), text_col = "white", line_width = 2,
                   text_cex = 1.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         line_col = "white",
                         style = north_arrow_fancy_orienteering(
                           fill = c("white", "black"),  # Arrow fill colors
                           line_col = "white",          # Arrow border
                           text_col = "white")) +           # 'N' text color
  coord_sf(crs = st_crs(4326), datum = st_crs(4326), label_graticule = "SW") +
  theme(axis.text = element_text(color = "black", face = "bold", size = 17),
        axis.title = element_text(color = "black", face = "bold", size = 17),
        legend.title = element_text(color = "black", face = "bold", size = 17),
        legend.text = element_text(color = "black", face = "bold", size = 17),
        plot.title = element_text(face = "bold", size = 17),
        axis.ticks = element_line(color = "black")) +
  labs(title = site_name, x = "Longitude", y = "Latitude") 

#stretch Rstudio/R window as needed to get map large and points to scale

#!!! Google maps requires proper attribution for usage in research journals
#!!! Need to leave Google and other attributions watermarked on map tiles unaltered and legible in figure.
#!!! Need to include attribution text in methods and figure legends.
#I.e., "Basemap imagery from Google Maps API (Map data ©2025 Google; imagery ©2025 Maxar Technologies, CNES/Airbus, TerraMetrics, and others)."


# #Tried with other open source packages below --- but couldn't get useful or zoomed in enough maps
# #### Without google maps --- using stadia maps (previously stamen maps) instead
# #stadia maps also need an API key to use - free usage up to a quota then charges
# # Define bounding box for your map
# bbox <- c(left = lon - 0.01,
#           bottom = lat - 0.01,
#           right = lon + 0.01,
#           top = lat + 0.01)
# 
# # Download terrain map from Stadia Maps (Stamen tiles)
# map_bg <- get_stadiamap(
#   bbox = bbox,
#   maptype = "stamen_terrain",  # same map style as before
#   zoom = 14,                   # adjust zoom level as needed
#   crop = TRUE,
#   messaging = TRUE,
#   urlonly = FALSE
# )
# 
# # Required packages
# library(ggplot2)
# library(ggspatial)
# library(sf)
# library(rosm)         # for OSM basemap tiles
# library(prettymapr)   # optional, for scale/north arrow
# library(dplyr)
# library(viridis)
# 
# # Ensure the object is a data frame --- works to make map but only gives blue "ocean tiles" so not informative
# depth_df_ll <- as.data.frame(depth_df_ll)
# 
# # Convert your raster dataframe to sf (if not already)
# depth_sf <- st_as_sf(depth_df_ll, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
# 
# # Bounding box for your site
# bbox <- st_bbox(depth_sf)
# 
# # Plot with free OpenStreetMap basemap
# ggplot() +
#   annotation_map_tile(type = "osm", zoomin = 0) + 
#   geom_raster(data = depth_df_ll, aes(x = lon, y = lat, fill = depth), alpha = 0.75) +
#   scale_fill_viridis_c(trans = 'reverse', direction = 1, option = "plasma", name = "Depth (m)") +
#   geom_point(data = SiteMeta, aes(x = Lon, y = Lat, color = Sex_Sept2024), size = 4) +
#   scale_color_manual(values = c("Oocytes" = "#E59EDD", "Spermatocytes" = "#50164A", "Both" = "#A02B93")) +
#   annotation_scale(location = "bl", bar_cols = c("white", "black"), text_col = "black") +
#   annotation_north_arrow(location = "bl", which_north = "true",
#                          style = north_arrow_fancy_orienteering()) +
#   coord_sf(crs = st_crs(4326), xlim = c(bbox["xmin"], bbox["xmax"]),
#            ylim = c(bbox["ymin"], bbox["ymax"])) +
#   theme_minimal(base_size = 14) +
#   labs(title = site_name, x = "Longitude", y = "Latitude")
# 
# ###With stamen plot --- not informative
# library(ggplot2)
# library(ggspatial)
# 
# ggplot() +
#   annotation_map_tile(
#     type = "cartolight",  # Also try "osm", "cartodark", or "stamenterrain"
#     zoom = 15
#   ) +
#   geom_raster(data = depth_df_ll, aes(x = lon, y = lat, fill = depth), alpha = 0.7) +
#   scale_fill_viridis_c(trans = 'reverse', option = "plasma", name = "Depth (m)", direction = 1) +
#   geom_point(data = SiteMeta, aes(x = Lon, y = Lat),
#              shape = 21, fill = "white", color = "black", size = 3) +
#   coord_sf(crs = st_crs(4326), datum = st_crs(4326)) +
#   labs(x = "Longitude", y = "Latitude") +
#   theme_minimal()
# 
# 
# #Try leaflet to plot
# install.packages("webshot")
# webshot::install_phantomjs()  # installs PhantomJS for taking screenshots
# library(leaflet)
# library(webshot)
# library(mapview)
# library(terra)
# 
# # Convert your raster to a format Leaflet understands
# depth_raster <- rast(depth_df_ll, type = "xyz", crs = "EPSG:4326")
# 
# # Use leaflet to plot -- works but interactive in R
# leaflet() %>% # zoom too far out but works
#   addProviderTiles(providers$Esri.WorldImagery) %>%  # satellite basemap
#   addRasterImage(depth_raster, colors = colorNumeric("plasma", NULL, reverse = TRUE), opacity = 0.6) %>%
#   addCircleMarkers(data = SiteMeta, ~Lon, ~Lat, label = ~SampleID, radius = 5,
#                    fillColor = "white", color = "black", fillOpacity = 1, weight = 1) %>%
#   addScaleBar(position = "bottomleft")
# 
# # works but zoom isn't close enough for informative interpretation, 
# #zoom 20 is max that is free through leaflet. 
# #Only options are to use googlemap above or to download higher res map (likely through a paid service)
# m <- leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery) %>%  # satellite basemap
#   setView(lng = mean(SiteMeta$Lon), lat = mean(SiteMeta$Lat), zoom = 20) %>%  # adjust zoom here
#   addRasterImage(depth_raster, colors = colorNumeric("plasma", NULL, reverse = TRUE), opacity = 0.6) %>%
#   addCircleMarkers(data = SiteMeta, ~Lon, ~Lat, label = ~SampleID, radius = 5,
#                    fillColor = "white", color = "black", fillOpacity = 1, weight = 1) %>%
#   addScaleBar(position = "bottomleft")
# 
# m 
# #save map-- !!! saves map but not overlay and points
# mapshot(m, file = "test_map.png", vwidth = 1600, vheight = 1200)




#### Determining if coral densities vary by depth ####
#Based on Lesneski 2019 -- used Chi-Square
library(dplyr)
library(ggplot2)
library(splines)

# Define depth bins for shallow range
depth_bins <- c(0, 1, 2, 3.5, 4.6)
depth_labels <- c("0-1", "1.01-2", "2.01-3.5", "3.51-4.6")

# Create results storage
chi_results <- data.frame()
plot_data_all <- data.frame()

# Loop over each site
sites <- unique(data$Site)

results <- list()

# Create an empty data frame to store all plot data
plot_data_all <- data.frame()


for (site_name in sites) {
  
  # Subset data for one site
  site_data <- data %>%
    filter(Site == site_name & !is.na(Depth_m))
  
  # Assign depth zones
  site_data$Depth_Zone <- cut(site_data$Depth_m,
                              breaks = depth_bins,
                              labels = depth_labels,
                              include.lowest = TRUE)
  
  # Observed coral counts per depth bin
  observed_counts <- table(site_data$Depth_Zone)
  
  # Estimate relative area in each depth bin via kernel density on depths
  dens <- density(site_data$Depth_m, from = 0, to = 4.6, n = 1000) #divided base don where corals are most commonly located in plots
  depth_df <- data.frame(depth = dens$x, density = dens$y)
  
  # Assign each depth point to a bin
  depth_df$bin <- cut(depth_df$depth, breaks = depth_bins, labels = depth_labels, include.lowest = TRUE)
  
  # Approximate relative area per bin by summing density values
  area_by_bin <- depth_df %>%
    group_by(bin) %>%
    summarise(area = sum(density), .groups = "drop")
  
  # Normalize areas to match total coral count
  total_corals <- sum(observed_counts)
  area_by_bin$expected <- (area_by_bin$area / sum(area_by_bin$area)) * total_corals
  
  # Match observed to expected by bin
  matched_observed <- observed_counts[match(area_by_bin$bin, names(observed_counts))]
  matched_expected <- area_by_bin$expected
  
  # Chi-square test
  chisq_test <- chisq.test(x = matched_observed, p = matched_expected / sum(matched_expected), rescale.p = TRUE)
  
  # Store results
  results[[site_name]] <- list(
    observed = matched_observed,
    expected = round(matched_expected, 2),
    chisq_test = chisq_test
  )
  
  # Optional printout
  print(paste("Site:", site_name))
  print(chisq_test)
  
  # Create plot_data correctly
  plot_data <- data.frame(
    Site = rep(site_name, length(area_by_bin$bin)),  # Replicate site name for each bin
    Depth_Zone = area_by_bin$bin,  # Depth zones
    Value = c(matched_observed, round(matched_expected, 2)),  # Concatenate observed and expected values
    Type = rep(c("Observed", "Expected"), each = length(area_by_bin$bin))  # Type: "Observed" or "Expected"
  )
  
  # Append plot data for all sites
  plot_data_all <- rbind(plot_data_all, plot_data)  # Append to the cumulative data frame
  
# Plot comparison of observed and expected counts as grouped bars
  p <- ggplot(plot_data, aes(x = Depth_Zone, y = Value, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +  # Position dodge to group bars
    theme_minimal() +
    labs(title = paste("Site:", site_name), y = "Counts", x = "Depth Zone") +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic() +
    theme(title = element_text(size=17, face = "bold", color = "black"), 
          legend.position="top", 
          legend.title=element_text(size=17, face = "bold", color = "black"), 
          legend.text=element_text(size=17, face = "bold", color="black"),
          axis.title.x = element_blank(),
          axis.text = element_text(color = "black", size = 17, face = "bold"),
          axis.title.y = element_text(size = 17, color = "black", face = "bold"),
          axis.ticks = element_line(color = "black"))+
    scale_fill_manual(values = c("grey50", "black"))  # Set custom colors for observed and expected
  
  # Explicitly print the plot to display
  print(p)
}

#site colors
SiteColors = c("#C47120","#F79E2C","#FAB095", "#3C6E96","#AABAEE", "#AD961D" )#)"#CFD8F5","#768EA1

# Example: reorder the facets in a specific order
plot_data_all$Site <- factor(plot_data_all$Site, 
                             levels = c('Sid Ciudad','Punta Caracol', 'Cristobal Island', 'Hospital Point', 'Punta Donato', 'STRI Point'))

# Optional: Combine all data from all sites for one large plot of etsimated v. observed densities
ggplot(plot_data_all, aes(x = Depth_Zone, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap2(~ Site,
             strip = strip_themed(
               text_x = elem_list_text(face = "bold", size = 17, 
                                       color = "black")
               ,
               background_x = elem_list_rect(fill = SiteColors))) +
  theme_minimal() +
  labs(title = "Observed vs Expected Coral Counts by Depth Zone", y = "Counts", x = "Depth Zone") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic() +
  theme(title = element_text(size=17, face = "bold", color = "black"), 
        legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))+
  scale_fill_manual(values = c("grey", "black"))

# only CI significantly clumped by depth, all other sites are not.
# means all sites with oocyte to spermary producer comparisons distributions are not affected by depth

# [1] "Site: STRI Point"
# Chi-squared test for given probabilities
# data:  matched_observed
# X-squared = 0.10034, df = 3, p-value = 0.9918
# 
# [1] "Site: Punta Caracol"
# Chi-squared test for given probabilities
# data:  matched_observed
# X-squared = 0.92462, df = 3, p-value = 0.8195
# 
# [1] "Site: Punta Donato"
# Chi-squared test for given probabilities
# data:  matched_observed
# X-squared = 8.1437e-06, df = 3, p-value = 1
# 
# [1] "Site: Hospital Point"
# Chi-squared test for given probabilities
# data:  matched_observed
# X-squared = 0.43135, df = 3, p-value = 0.9337
# 
# [1] "Site: Sid Ciudad"
# Chi-squared test for given probabilities
# data:  matched_observed
# X-squared = 0.0084908, df = 3, p-value = 0.9998
# 
# [1] "Site: Cristobal Island"
# Chi-squared test for given probabilities
# data:  matched_observed
# X-squared = 15.262, df = 3, p-value = 0.001606


#### Complete Spatial Randomness (CSR) Analysis ####
# Load required libraries
library(spatstat.geom)    # For point pattern geometry
library(spatstat.core)    # For spatial tests
library(spatstat.explore) # For spatial analysis
library(dplyr)            # For data manipulation
library(ggplot2)          # For plotting
library(readr)            # For reading data

# Load Data 
#if not already done above
data <- read.csv("~/Desktop/BU NSF Postdoc/Panama Work/SexDifferencesMS_Panama2024/Panama_SSID_2024-2025_Metadata.csv")


#Make a comparison of egg producers to not egg producers (streamlines code and increases power for sites with mixed oocyte producers)
data <- data %>%      
  mutate(Sex_Sept2024 = recode(Sex_Sept2024,
                               "F" = "Oocytes",
                               "M" = "Spermatocytes",
                               "H" = "Oocytes"))

#remove corals without sex determinations
data$Sex_Sept2024[data$Sex_Sept2024 == ""] <- "Unknown"
metadata <- subset(data, Sex_Sept2024 != "Unknown")

#remove corals with bad coordinates, if not already done above
metadata <- subset(metadata, SampleID != "175")

num_rows <- nrow(metadata) # umber of corals leftt for site analyses
print(num_rows) #99 Corals in analysis

# Split master metadata up for site specific analyses 
#--- NEED TO RUN EACH SITE ALL THE WAY THROUGH ANALYSIS to distance histograms BEFORE DOING THE NEXT SITE ----
# Filter to a specific site and run from here through line before starting next site
site_name <-                     #Site for analysis, change as needed
  #Sites with egg producers         
      "Hospital Point"
     # "Punta Donato"
     # "STRI Point"  ###Need to adjust ggmap zoom to 19!!!!
#Sites with no egg producers
#            "Sid Ciudad"
#             "Punta Caracol"
#             "Cristobal Island"

#Subset to site
SiteMeta <- metadata %>% filter(Site == site_name)
head(SiteMeta)
num_rows <- nrow(SiteMeta)#number of coral colonies included in analysis
print(num_rows)
#HP - 18
#PD - 15
#SP - 20

# Set Up the Point Pattern (ppp) --- using Lat and Lon --- maybe not best way due to scale of area analyzed

# ### --- as Lon/Lat coordinates --- decided to use meters for better resolution
# # Rename columns for convenience if needed
# SiteMeta <- SiteMeta %>%
#   rename(x = Lon, 
#          y = Lat)
# 
# # Define observation window (bounding box)
# xrange <- range(SiteMeta$x)
# yrange <- range(SiteMeta$y)
# 
# # Create the ppp object
# ppp_points <- ppp(
#   x = SiteMeta$x,
#   y = SiteMeta$y,
#   window = owin(xrange = xrange, yrange = yrange),
#   marks = factor(SiteMeta$Sex_Sept2024)  # Use Sex Type as mark
# )
# 
# # Quick plot
# plot(ppp_points, main = paste(site_name, "Spatial Distribution by Sex Type"))

### --- as meters

# Set Up the Point Pattern (ppp) --- using meters --- probably better due to scale of site analyzed
# Rename columns for convenience if needed
SiteMeta <- SiteMeta %>%
  rename(Longitude = Lon, 
         Latitude = Lat)

# Step 1: Convert to sf object
site_sf <- st_as_sf(SiteMeta, coords = c("Longitude", "Latitude"), crs = 4326)  # 4326 = WGS84

# Step 2: Project to UTM (choose correct UTM zone based on your study location)
site_sf_utm <- st_transform(site_sf, crs = 32617)  # example: UTM zone 17N (Panama/Caribbean area)

# Step 3: Extract projected coordinates
coords <- st_coordinates(site_sf_utm)

# Step 4: Make sure Sex is a factor (VERY IMPORTANT!)
SiteMeta$Sex_Sept2024 <- as.factor(SiteMeta$Sex_Sept2024)

# Step 5: Make ppp object
reef_ppp <- ppp(
  x = coords[,1],
  y = coords[,2],
  window = owin(xrange = range(coords[,1]), yrange = range(coords[,2])),
  marks = SiteMeta$Sex_Sept2024
)
# Warning message: data contain duplicated points --- this is because some of our colonies are side by side

# Quick plot
plot(reef_ppp, main = paste(site_name, "Spatial Distribution by Sex Type"))


#### Test for Complete Spatial Randomness (CSR) --- whole dataset, no marks ####
# in meters --- to get real world distance information

quad_test <- quadrat.test(reef_ppp, nx = 4, ny = 4)
print(quad_test)
plot(reef_ppp, main = "Quadrat Test")
plot(quad_test, add = TRUE)
#HP -- Clumpy -- meters
# Chi-squared test of CSR using quadrat counts
# data:  reef_ppp
# X2 = 35.333, df = 15, p-value = 0.004408 *** not random
# alternative hypothesis: two.sided
# Quadrats: 4 by 4 grid of tiles

#SP --- clumpy
# Chi-squared test of CSR using quadrat counts
# data:  reef_ppp
# X2 = 55.2, df = 15, p-value = 3.306e-06
# alternative hypothesis: two.sided
# Quadrats: 4 by 4 grid of tiles

#PD --- colonies are generally clumpy
# Chi-squared test of CSR using quadrat counts
# data:  reef_ppp
# X2 = 20.2, df = 15, p-value = 0.3288
# alternative hypothesis: two.sided
# Quadrats: 4 by 4 grid of tiles


# (b) Nearest neighbor distance test (more sensitive) - Gtest
nn_test <- dclf.test(reef_ppp, fun = Gest, alternative = "two.sided") #All default
print(nn_test)
nn_test1 <- envelope(reef_ppp, Gest,  nsim = 999)
plot(nn_test1, main = "Nearest Neighbor G-Test for CSR All")
#HP --- not clumpy --- meters
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: G(r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 10.6404457739361]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 0.29302, rank = 7, p-value = 0.07  ### random, but almost not

#SP --- not clumpy
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: G(r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 33.5836293071739]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 0.34485, rank = 46, p-value = 0.46

#PD --- Plot - density greater <1 m than expected for all gamete comparisons, rest random
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: G(r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 9.19782570743172]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 0.52853, rank = 17, p-value = 0.17  ## random

# Pairwise Comparisons - Gcross
nn_test <- dclf.test(reef_ppp, fun = Gcross, i = "Oocytes", j = "Spermatocytes", alternative = "two.sided")
print(nn_test)
nn_test1 <- envelope(reef_ppp, Gcross, i = "Oocytes", j = "Spermatocytes", nsim = 999)
plot(nn_test1, main = "Nearest Neighbor G-Test for CSR OoSp")
#PD ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Oocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 11.8743419287338]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 3.3327, rank = 12, p-value = 0.12

#HP -- Observed greater than expected across distances but within 95% envelopes
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Oocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 12.0651314385871]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 1.7968, rank = 21, p-value = 0.21  ### random

#SP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Oocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 37.5476390309975]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 1.5793, rank = 45, p-value = 0.45

nn_test <- dclf.test(reef_ppp, fun = Gcross,  i = "Spermatocytes", j = "Oocytes", alternative = "two.sided")
print(nn_test)
nn_test1 <- envelope(reef_ppp, Gest, i = "Spermatocytes", j = "Oocytes", nsim = 999)
plot(nn_test1, main = "Nearest Neighbor G-Test for CSR OoOo")
#HP
# data:  reef_ppp
# u = 0.44707, rank = 78, p-value = 0.78

#PD
# data:  reef_ppp
# u = 0.44178, rank = 55, p-value = 0.55

#SP
# data:  reef_ppp
# u = 8.8476, rank = 30, p-value = 0.3


nn_test <- dclf.test(reef_ppp, fun = Gcross,  i = "Oocytes", j = "Oocytes", alternative = "two.sided")
print(nn_test)
nn_test1 <- envelope(reef_ppp, Gest, i = "Oocytes", j = "Oocytes", nsim = 999)
plot(nn_test1, main = "Nearest Neighbor G-Test for CSR OoOo")

#PD ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Oocytes", "Oocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 13.3300879468539]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 3.5433, rank = 28, p-value = 0.28

#HP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Oocytes", "Oocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 17.5932708641763]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 9.9709, rank = 32, p-value = 0.32  ### random

#SP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Oocytes", "Oocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 63.4650030927149]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 2.7332, rank = 84, p-value = 0.84

nn_test <- dclf.test(reef_ppp, fun = Gcross,  i = "Spermatocytes", j = "Spermatocytes", alternative = "two.sided")
print(nn_test)
nn_test1 <- envelope(reef_ppp, Gest, i = "Spermatocytes", j = "Spermatocytes", nsim = 999)
plot(nn_test1, main = "Nearest Neighbor G-Test for CSR SpSp")

#PD ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Spermatocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 11.8743419287338]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 4.5494, rank = 14, p-value = 0.14

#HP ---
# plot(nn_test1, main = "Nearest Neighbor G-Test for CSR")
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Spermatocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 12.0651314385871]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 0.53632, rank = 19, p-value = 0.19  ## random

#SP ---
#	Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "G"["Spermatocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 37.5476390309975]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 1.0923, rank = 24, p-value = 0.24


#(b) Pairwise distances across individuals - Kest
#meters
nn_test <- dclf.test(reef_ppp, fun = Kest, alternative = "two.sided") #all corals, no marks
print(nn_test)
#visualize differences with Monte Carlo envelopes with expected and observed
nn_test1 <- envelope(reef_ppp, Kest, nsim = 999)
plot(nn_test1, main = "Pairwise Comparison Kest for CSR All")

#HP --- density differences ~1-2m
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: K(r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.65751349613129]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 2939.5, rank = 2, p-value = 0.02 ** not random

#PD --- 
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: K(r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.14923915170948]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 1463.3, rank = 5, p-value = 0.05 ** not random

#SP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: K(r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 13.3673230770655]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 2017019, rank = 1, p-value = 0.01

#Pairwise distance comparisons with mark - Kcross with marks
nn_test <- dclf.test(reef_ppp,fun = Kcross, i ="Spermatocytes", j =  "Oocytes", alternative = "two.sided") #explicitly compare Oo and Sp
print(nn_test)
nn_test1 <- envelope(reef_ppp, Kcross, i ="Spermatocytes", j =  "Oocytes", nsim = 999)
plot(nn_test1, main = "Pairwise Comparison Kcross for CSR SpOo")
#SP
# data:  reef_ppp
# u = 1624208, rank = 4, p-value = 0.04

#HP
# data:  reef_ppp
# u = 735.57, rank = 58, p-value = 0.58

#PD
# data:  reef_ppp
# u = 2909.5, rank = 6, p-value = 0.06


nn_test <- dclf.test(reef_ppp,fun = Kcross, i = "Oocytes", j = "Spermatocytes", alternative = "two.sided") #explicitly compare Oo and Sp
print(nn_test)
nn_test1 <- envelope(reef_ppp, Kcross, , i = "Oocytes", j = "Spermatocytes", nsim = 999)
plot(nn_test1, main = "Pairwise Comparison Kcross for CSR OoSp")

#HP --- Oo-Sp - random - high overlap of observed and expected, well within envelope
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Oocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.65751349613129]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 712.97, rank = 66, p-value = 0.66 ### random

#PD ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Oocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.14923915170948]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 3109.2, rank = 4, p-value = 0.04 *** not random

#SP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Oocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 13.3673230770655]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 1945730, rank = 2, p-value = 0.02

nn_test <- dclf.test(reef_ppp,fun = Kcross, i = "Oocytes", j = "Oocytes", alternative = "two.sided") #explicitly compare Oo and Sp
print(nn_test)
nn_test1 <- envelope(reef_ppp, Kcross, i = "Oocytes", j = "Oocytes", nsim = 999)
plot(nn_test1, main = "Pairwise Comparison Kcross for CSR OoOo")

#HP --- likely not robust due to sample size
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Oocytes", "Oocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.65751349613129]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 4338.8, rank = 57, p-value = 0.57  ## random

#PD ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Oocytes", "Oocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.14923915170948]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 7026.9, rank = 7, p-value = 0.07 *** not random

#SP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Oocytes", "Oocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 13.3673230770655]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 844934, rank = 48, p-value = 0.48



nn_test <- dclf.test(reef_ppp,fun = Kcross, i = "Spermatocytes", j = "Spermatocytes", alternative = "two.sided") #explicitly compare Oo and Sp
print(nn_test)
nn_test1 <- envelope(reef_ppp, Kcross, i = "Spermatocytes", j = "Spermatocytes", nsim = 999)
plot(nn_test1, main = "Pairwise Comparison Kcross for CSR SpSp")

#HP --- consistently above expected, though only above envelope at ~1m
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Spermatocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.65751349613129]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 7226.6, rank = 2, p-value = 0.02 ### not random

#PD ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Spermatocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 4.14923915170948]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 184.95, rank = 82, p-value = 0.82 ### random

#SP ---
# Diggle-Cressie-Loosmore-Ford test of CSR
# Monte Carlo test based on 99 simulations
# Summary function: "K"["Spermatocytes", "Spermatocytes"](r)
# Reference function: theoretical
# Alternative: two.sided
# Interval of distance values: [0, 13.3673230770655]
# Test statistic: Integral of squared absolute deviation
# Deviation = observed minus theoretical
# data:  reef_ppp
# u = 2619117, rank = 2, p-value = 0.02


# #Decide due to low replication to not do the Mark Connection analysis below
# ### (d) Analyze Mark Connection (i.e., egg v. sperm production)
# 
# #Function for calculating mark connection
# analyze_mark_connection <- function(ppp_object, mark_i, mark_j, nsim = 999) {
#   
#   # Print which comparison is running
#   cat("\nRunning Mark Connection Test for:", mark_i, "to", mark_j, "\n")
#   
#   # Build envelope
#   env <- envelope(
#     ppp_object,
#     fun = markconnect,
#     i = mark_i,
#     j = mark_j,
#     nsim = nsim,
#     simulate = expression(rlabel(ppp_object)),
#     global = TRUE,
#     verbose = FALSE
#   )
#   
#   # Plot (fully auto-handled)
#   plot(env,
#        main = paste0("Mark Connection: ", mark_i, " → ", mark_j),
#        legend = FALSE,
#        shade = c("hi", "lo"),
#        xlab = "Distance (r)",
#        ylab = paste0("P(", mark_i, " near ", mark_j, ")"))
#   
#   abline(h = mean(marks(ppp_object) == mark_j), col = "red", lty = 3)
#   
#   cat("Finished!\n")
#   
#   return(env)
# }
# 
# 
# #Plot
# #Y-axis: Estimated probability that a point of type j is within a given distance r of a point of type i
# #black solid line = Observed mark connection function of mark i to mark j
# #Grey shading = CSR 95% simulation envelope under the null model of independence.
# #red dashed line = Theoretical expected value under complete spatial randomness (CSR) or Null expectation under independence (proportion of j marks)
# 
# #Interpret Plot:
# # If the black curve > envelope: i points are more often near j points than expected → positive association.
# # If the black curve < envelope: i points are less often near j points than expected → segregation.
# # If the black curve is within envelope: No evidence of spatial dependence between i and j → random labeling holds
# #     ->"tendency to have fewer/more X neighbors than random, but N.S. if still in grey envelope"
# #     ->"observed ratio within expectations under random chance across 0–4 meters."
# 
# 
# # Spermatocytes-Spermatocytes interaction
# env_Spermatocytes_Spermatocytes <- analyze_mark_connection(reef_ppp, mark_i = "Spermatocytes", mark_j = "Spermatocytes")
# summary(env_Spermatocytes_Spermatocytes)
# 
# #HP --- Fluctuations are normal random variation.
# # → Small tendency for Males to have slightly fewer Male neighbors than random at ~2m.
# # But since the curve stays inside the CSR envelope, this is not statistically significant.
# # Simultaneous critical envelopes for p["Spermatocytes", "Spermatocytes"](r)
# # and observed value for ‘ppp_object’
# # Obtained from 999 evaluations of user-supplied expression
# # Theoretical (i.e. null) mean value of p["Spermatocytes", "Spermatocytes"](r) estimated from a separate set of 999 
# # simulations
# # Alternative: two.sided
# # Envelopes computed as mean of simulations plus/minus maximum simulated value of maximum absolute deviation
# # Significance level of Monte Carlo test: 1/1000 = 0.001  
# # Data: ppp_object
# 
# #PD --- observed always below red dashed lines but within envelope
# #   --- Spermary producers tend to have less spermary producing neighbors than chance across distance?
# # Simultaneous critical envelopes for p["Spermatocytes", "Spermatocytes"](r)
# # and observed value for ‘ppp_object’
# # Obtained from 999 evaluations of user-supplied expression
# # Theoretical (i.e. null) mean value of p["Spermatocytes", "Spermatocytes"](r) estimated from a separate set of 999 
# # simulations
# # Alternative: two.sided
# # Envelopes computed as mean of simulations plus/minus maximum simulated value of maximum absolute deviation
# # Significance level of Monte Carlo test: 1/1000 = 0.001
# # Data: ppp_object
# 
# #SP ---
# #Significance level of Monte Carlo test: 1/1000 = 0.001
# 
# 
# # Oocytes-Spermatocytes interaction
# env_Oocytes_Spermatocytes <- analyze_mark_connection(reef_ppp, mark_i = "Oocytes", mark_j = "Spermatocytes")
# summary(env_Oocytes_Spermatocytes)
# 
# #HP ---
# # Simultaneous critical envelopes for p["Oocytes", "Spermatocytes"](r)
# # and observed value for ‘ppp_object’
# # Obtained from 999 evaluations of user-supplied expression
# # Theoretical (i.e. null) mean value of p["Oocytes", "Spermatocytes"](r) estimated from a separate set of 999 simulations
# # Alternative: two.sided
# # Envelopes computed as mean of simulations plus/minus maximum simulated value of maximum absolute deviation
# # Significance level of Monte Carlo test: 1/1000 = 0.001
# # Data: ppp_object
# 
# #PD --- observed within envelope, likely due to low n, but observed almost always greater (especially at 0 and 2m) than expected across depths
# #   --- Oocyte producers tend to have more spermary producing neighbors than chance .
# # Simultaneous critical envelopes for p["Oocytes", "Spermatocytes"](r)
# # and observed value for ‘ppp_object’
# # Obtained from 999 evaluations of user-supplied expression
# # Theoretical (i.e. null) mean value of p["Oocytes", "Spermatocytes"](r) estimated from a separate set of 999 simulations
# # Alternative: two.sided
# # Envelopes computed as mean of simulations plus/minus maximum simulated value of maximum absolute deviation
# # Significance level of Monte Carlo test: 1/1000 = 0.001
# # Data: ppp_object
# 
# #SP ---
# #Significance level of Monte Carlo test: 1/1000 = 0.001
# 
# # Oocytes-Oocytes interaction
# env_Oocytes_Oocytes <- analyze_mark_connection(reef_ppp, mark_i = "Oocytes", mark_j = "Oocytes")
# summary(env_Oocytes_Oocytes)
# 
# #HP --- not robust all flat lines
# # Simultaneous critical envelopes for p["Oocytes", "Oocytes"](r)
# # and observed value for ‘ppp_object’
# # Obtained from 999 evaluations of user-supplied expression
# # Theoretical (i.e. null) mean value of p["Oocytes", "Oocytes"](r) estimated from a separate set of 999 simulations
# # Alternative: two.sided
# # Envelopes computed as mean of simulations plus/minus maximum simulated value of maximum absolute deviation
# # Significance level of Monte Carlo test: 1/1000 = 0.001
# # Data: ppp_object
# 
# #PD --- within envelope, but lower than expected at 3-4 m than 1-2 m
# #   --- Oocyte producers tend to have more spermary producing neighbors than chance.
# # Simultaneous critical envelopes for p["Oocytes", "Oocytes"](r)
# # and observed value for ‘ppp_object’
# # Obtained from 999 evaluations of user-supplied expression
# # Theoretical (i.e. null) mean value of p["Oocytes", "Oocytes"](r) estimated from a separate set of 999 simulations
# # Alternative: two.sided
# # Envelopes computed as mean of simulations plus/minus maximum simulated value of maximum absolute deviation
# # Significance level of Monte Carlo test: 1/1000 = 0.001
# # Data: ppp_object
# 
# #SP ---
# #Significance level of Monte Carlo test: 1/1000 = 0.001




#### Visualize distances between points by reproductive category as overlapped histograms####
#— i.e., all individuals, oocyte producers, and spermary producers — as histograms of nearest-neighbor distances 

# ### This one compares all and within gamete producer groups --- not used in MS
# #Nearest Neighbor - Gest/Gcross
# # All individuals (regardless of sex, coral to coral)
# nnd_all <- nndist(reef_ppp)
# 
# # Subset to oocyte producers (oocyte producer to next oocyte producer)
# ppp_oo <- reef_ppp[marks(reef_ppp) == "Oocytes"]
# 
# # Subset to spermary producers (spermary producer to next spermary producer)
# ppp_sp <- reef_ppp[marks(reef_ppp) == "Spermatocytes"]
# 
# # Nearest neighbor distances
# nnd_oo <- nndist(ppp_oo)
# nnd_sp <- nndist(ppp_sp)
# 
# # Combine all into one data frame for ggplot
# nnd_df <- data.frame(
#   Distance = c(nnd_all, nnd_oo, nnd_sp),
#   Group = c(
#     rep("All", length(nnd_all)),
#     rep("Oocytes", length(nnd_oo)),
#     rep("Spermatocytes", length(nnd_sp))
#   )
# )
# 
# #Side by side histograms of distances
# ggplot(nnd_df, aes(x = Distance, fill = Group)) +
#   geom_histogram(position = "identity", alpha = 1, color = "black") + #binwidth = 1.5,
#   facet_wrap(~ Group, scales = "free_y") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   labs(
#     x = "Nearest Neighbor Distance (m)",
#     y = "Frequency",
#     title = paste(site_name, "- Nearest Neighbor Distances by Gamete Type")
#   ) +
#   scale_fill_manual(values = c("grey75","#E59EDD", "#50164A")) +
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.title.x = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"),
#         strip.text = element_text(size = 17, face = "bold"))
# 
# # All distance comparisons overlayed in the same plot
# ggplot(nnd_df, aes(x = Distance, fill = Group)) +
#   geom_histogram(binwidth = 1.5, position = "identity", alpha = 0.75, color = "black") +
#   theme_classic() +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(
#     x = "Nearest Neighbor Distance (m)",
#     y = "Frequency",
#     title = paste(site_name, "- Nearest Neighbor Distances by Gamete Type")
#   ) +
#   scale_fill_manual(values = c("grey75","#E59EDD", "#50164A")) +
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.title.x = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))
# 
# 
# # Pairwise distances - Kest/Kcross
# # Pairwise distances for all individuals
# dist_all <- pairdist(reef_ppp)
# dist_all_vals <- dist_all[lower.tri(dist_all)]  # Only use lower triangle (no repeats or diagonals)
# 
# # Oocytes
# dist_oo <- pairdist(ppp_oo)
# dist_oo_vals <- dist_oo[lower.tri(dist_oo)]
# 
# # Spermatocytes
# dist_sp <- pairdist(ppp_sp)
# dist_sp_vals <- dist_sp[lower.tri(dist_sp)]
# 
# pairwise_df <- data.frame(
#   Distance = c(dist_all_vals, dist_oo_vals, dist_sp_vals),
#   Group = c(
#     rep("All", length(dist_all_vals)),
#     rep("Oocytes", length(dist_oo_vals)),
#     rep("Spermatocytes", length(dist_sp_vals))
#   )
# )
# 
# #Side by side histograms of distances
# ggplot(pairwise_df, aes(x = Distance, fill = Group)) +
#   geom_histogram(binwidth = 1.5, position = "identity", alpha = 1, color = "black") +
#   facet_wrap(~ Group, scales = "free_y") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   labs(
#     x = "Pairwise Distance (m)",
#     y = "Frequency",
#     title = paste(site_name, "- Pairwise Distances by Gamete Type")
#   ) +
#   scale_fill_manual(values = c("grey75","#E59EDD", "#50164A")) +
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.title.x = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"),
#         strip.text = element_text(size = 17, face = "bold"))
# 
# # All distance comparisons overlayed in the same plot
# ggplot(pairwise_df, aes(x = Distance, fill = Group)) +
#   geom_histogram(binwidth = 1.5, position = "identity", alpha = 0.75, color = "black") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   labs(
#     x = "Pairwise Distance (m)",
#     y = "Frequency",
#     title = paste(site_name, "- Overlay of Pairwise Distances by Gamete Type")
#   ) +
#   scale_fill_manual(values = c("grey75","#E59EDD", "#50164A")) +
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.title.x = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))


### These histogram compares all, Oo-Sp, and Oo-Oo Nearest Neighbor distances 
# Fig 4 D-F

# Subset point pattern to groups
ppp_all <- reef_ppp
ppp_sp <- reef_ppp[marks(reef_ppp) == "Spermatocytes"]
ppp_oo <- reef_ppp[marks(reef_ppp) == "Oocytes"]

# All-to-All nearest neighbor distances
nnd_all <- nndist(ppp_all)

# Sp-to-Oo: Distance from each Spermatocytes point to the closest Oocytes point
nnd_sp_to_oo <- nncross(ppp_sp, ppp_oo)$dist

#Oo-to-Sp --> # doesn't work due to different numbers of rows
nnd_oo_to_sp <- nncross(ppp_oo, ppp_sp, )$dist

# Sp-to-Sp: Distance from each Spermatocytes point to the closest *other* Spermatocytes point
# Exclude self in calculation by using `k=2` (first neighbor is itself)
nnd_sp_to_sp <- nndist(ppp_sp, k=2)

# Oo-to-Oo: Distance from each Oocyte point to the closest *other* Oocyte point
# Exclude self in calculation by using `k=2` (first neighbor is itself)
nnd_oo_to_oo <- nndist(ppp_oo, k=2)

# Combine into dataframe for Oocyte comparisons 
nnd_df <- data.frame(
  #Distance = c(nnd_all, nnd_sp_to_sp, nnd_sp_to_oo),
  Distance = c(nnd_all, nnd_oo_to_oo, nnd_oo_to_sp), #oocytes
  Group = factor(c(
    rep("All ↔ All", length(nnd_all)),
    rep("Oo ↔ Oo", length(nnd_oo_to_oo)), #oocytes
    rep("Oo ↔ Sp", length(nnd_oo_to_sp)) #oocytes
  )
  , levels = c("All ↔ All", "Oo ↔ Sp", "Oo ↔ Oo"))  # #oocytes order for levels
)

#Side by side histograms of distances
ggplot(nnd_df, aes(x = Distance, fill = Group)) +
  geom_histogram(position = "identity", alpha = 1, color = "black") + #binwidth = 1.5,
  facet_wrap(~ Group, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Frequency",
    title = paste(site_name, "- Nearest Neighbor Distances by Gamete Type")
  ) +
  scale_fill_manual(values = c("grey75", "#50164A", "#E59EDD")) +
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 17, face = "bold"))

# All distance comparisons overlayed in the same plot
ggplot(nnd_df, aes(x = Distance, fill = Group)) +
  geom_histogram(binwidth = 1.5, position = "identity", alpha = 0.55, color = "black") +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Frequency",
    title = paste(site_name, "- Nearest Neighbor Distances by Gamete Type")
  ) +
  scale_fill_manual(values = c("grey75", "#50164A", "#E59EDD")) +
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))



### These histogram compares all, Sp-Oo, and Sp-Sp Nearest Neighbor distances 
#(uses data from above histogram calculations)

# Combine into dataframe for Spermatocyte comparisons
nnd_df <- data.frame(
  Distance = c(nnd_all, nnd_sp_to_sp, nnd_sp_to_oo),
  Group = factor(c(
    rep("All ↔ All", length(nnd_all)),
    rep("Sp ↔ Sp", length(nnd_sp_to_sp)), 
    rep("Sp ↔ Oo", length(nnd_sp_to_oo)) 
  ), levels = c("All ↔ All", "Sp ↔ Sp", "Sp ↔ Oo"))  # Spermatocytes order for levels
)

#Side by side histograms of distances
ggplot(nnd_df, aes(x = Distance, fill = Group)) +
  geom_histogram(position = "identity", alpha = 1, color = "black") + #binwidth = 1.5,
  facet_wrap(~ Group, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Frequency",
    title = paste(site_name, "- Nearest Neighbor Distances by Gamete Type")
  ) +
  scale_fill_manual(values = c("grey75", "#50164A", "#E59EDD")) +
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 17, face = "bold"))

# All distance comparisons overlayed in the same plot
ggplot(nnd_df, aes(x = Distance, fill = Group)) +
  geom_histogram(binwidth = 1.5, position = "identity", alpha = 0.55, color = "black") +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Frequency",
    title = paste(site_name, "- Nearest Neighbor Distances by Gamete Type")
  ) +
  scale_fill_manual(values = c("grey75", "#50164A", "#E59EDD")) +
  theme(legend.position="top", 
        legend.title=element_text(size=17, face = "bold", color = "black"), 
        legend.text=element_text(size=17, face = "bold", color="black"),
        axis.text = element_text(color = "black", size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold"),
        axis.ticks = element_line(color = "black"))


# # Pairwise distances - Kest/Kcross --- Not in MS
# # Subset point pattern to groups
# ppp_all <- reef_ppp
# ppp_sp  <- reef_ppp[marks(reef_ppp) == "Spermatocytes"]
# ppp_oo  <- reef_ppp[marks(reef_ppp) == "Oocytes"]
# 
# # All pairwise distances (excluding zero/self)
# all_dists <- pairdist(ppp_all)
# all_dists <- all_dists[lower.tri(all_dists)]  # avoid duplicates & self
# 
# #Sp → Sp: All pairwise distances among Spermatocytes (exclude diagonal)
# sp_to_sp_dists <- pairdist(ppp_sp)
# sp_to_sp_dists <- sp_to_sp_dists[lower.tri(sp_to_sp_dists)]
# 
# #Oo → Oo: All pairwise distances among Spermatocytes (exclude diagonal)
# oo_to_oo_dists <- pairdist(ppp_oo)
# oo_to_oo_dists <- oo_to_oo_dists[lower.tri(oo_to_oo_dists)]
# 
# #Sp → Oo: All pairwise distances from Spermatocytes to Oocytes
# sp_to_oo_dists <- as.vector(crossdist(ppp_sp, ppp_oo))
# 
# # Optional: add Oo → Sp if you want symmetry
# oo_to_sp_dists <- as.vector(crossdist(ppp_oo, ppp_sp))
# 
# # Combine into data frame
# pairwise_df <- data.frame(
#   #Distance = c(all_dists, sp_to_sp_dists, sp_to_oo_dists),
#   Distance = c(all_dists, oo_to_sp_dists, oo_to_oo_dists), #oocytes
#   Group = factor(c(
#     rep("All ↔ All", length(all_dists)),
#     #rep("Sp ↔ Sp", length(sp_to_sp_dists)),
#     rep("Oo ↔ Sp", length(oo_to_sp_dists)), #oocytes
#     #rep("Sp ↔ Oo", length(sp_to_oo_dists))
#     rep("Oo ↔ Oo", length(oo_to_oo_dists)) #oocytes
#   )#, levels = c("All ↔ All", "Sp ↔ Sp", "Sp ↔ Oo"))  # spermary order for levels
#   , levels = c("All ↔ All", "Oo ↔ Sp", "Oo ↔ Oo"))  #oocytes order for levels
# )
# 
# 
# #Side by side histograms of distances
# ggplot(pairwise_df, aes(x = Distance, fill = Group)) +
#   geom_histogram(binwidth = 1.5, position = "identity", alpha = 1, color = "black") +
#   facet_wrap(~ Group, scales = "free_y") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   labs(
#     x = "Pairwise Distance (m)",
#     y = "Frequency",
#     title = paste(site_name, "- Pairwise Distances by Gamete Type")
#   ) +
#   scale_fill_manual(values = c("grey75", "#50164A","#E59EDD")) +
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.title.x = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"),
#         strip.text = element_text(size = 17, face = "bold"))
# 
# # All distance comparisons overlayed in the same plot
# ggplot(pairwise_df, aes(x = Distance, fill = Group)) +
#   geom_histogram(binwidth = 1.5, position = "identity", alpha = 0.55, color = "black") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic() +
#   labs(
#     x = "Pairwise Distance (m)",
#     y = "Frequency",
#     title = paste(site_name, "- Overlay of Pairwise Distances by Gamete Type")
#   ) +
#   scale_fill_manual(values = c("grey75","#50164A","#E59EDD")) +
#   theme(legend.position="top", 
#         legend.title=element_text(size=17, face = "bold", color = "black"), 
#         legend.text=element_text(size=17, face = "bold", color="black"),
#         axis.text = element_text(color = "black", size = 17, face = "bold"),
#         axis.title.y = element_text(size = 17, color = "black", face = "bold"),
#         axis.title.x = element_text(size = 17, color = "black", face = "bold"),
#         axis.ticks = element_line(color = "black"))
