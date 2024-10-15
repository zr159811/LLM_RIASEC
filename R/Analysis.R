# GPT - RAISEC - Analysis
# Zachary Roman
# Zjr159811@gmail.com
# 19.03.2024
# # # # # # # # # #

# Packages
# # # # # #
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(kableExtra)
library(stringdist) 
source("functions.R")

# Import
# # # # # #
#dat <- read.csv("../data/responses.csv")
#dat <- read.csv("../data/responses2.csv")
dat <- read.csv("../data/responses3.csv")

dat <- dat[dat$ai != "Copilot",]


# Pre-Proc
# # # # # # 

# Number of jobs from each querry list of (1-10)
k = 10
# Create RIASEC Rowmeans
colz <- paste0("realistic_",1:k)
dat$R <- rowMeans(dat[,colz],na.rm = TRUE)

colz <- paste0("investigative_",1:k)
dat$I <- rowMeans(dat[,colz],na.rm = TRUE)

colz <- paste0("artistic_",1:k)
dat$A <- rowMeans(dat[,colz],na.rm = TRUE)

colz <- paste0("social_",1:k)
dat$S <- rowMeans(dat[,colz],na.rm = TRUE)

colz <- paste0("enterprising_",1:k)
dat$E <- rowMeans(dat[,colz],na.rm = TRUE)

colz <- paste0("conventional_",1:k)
dat$C <- rowMeans(dat[,colz],na.rm = TRUE)

# Isolate data frames for each "study"
# 100 iterations per AI
# 5 AI
# 2 conditions N = 1000 

# Person 0 
# # # # # #
dat0 <- dat[dat$age == 23 &
              dat$name == "" &
              dat$family.status == "Single" &
              dat$location == "USA" &
              dat$sexuality == "" &
              dat$language == "English" &
              dat$ethnicity == "" &
              dat$race == "" &
              dat$education == "Bachelor's Degree" &
              dat$gender == "", ]


# Study 1: Gender main effect
# # # # #

dat1 <- dat[dat$age == 23 &
              dat$name == "" &
              dat$family.status == "Single" &
              dat$location == "USA" &
              dat$sexuality == "" &
              dat$language == "English" &
              dat$ethnicity == "" &
              dat$race == "" &
              dat$education == "Bachelor's Degree" &
              dat$gender %in% c("Male", "Female"), ]

# Study 2: race main effect
# # # # #

dat2 <- dat[dat$age == 23 &
              dat$name == "" &
              dat$family.status == "Single" &
              dat$location == "USA" &
              dat$sexuality == "" &
              dat$language == "English" &
              dat$ethnicity == "" &
              dat$race %in% c("White", "Black") &
              dat$education == "Bachelor's Degree" &
              dat$gender == "", ]



# Study 3: Ethnicity by gender interaction
# # # # #

dat3 <- dat[dat$age == 23 &
              dat$name == "" &
              dat$family.status == "Single" &
              dat$location == "USA" &
              dat$sexuality == "" &
              dat$language == "English" &
              dat$race %in% c("White", "Black") &
              dat$education == "Bachelor's Degree" &
              dat$gender %in% c("Male", "Female") &
              dat$ethnicity == "", ]



# Study 4: Name discrimination
# # # # # # # # 

dat4 <- dat[dat$age == 23 &
              dat$name %in% c("Emily Walsh","Greg Baker","Lakisha Washington","Jamal Jones") &
              dat$family.status == "Single" &
              dat$location == "USA" &
              dat$education == "Bachelor's Degree" &
              dat$language == "English" &
              dat$sexuality == "" &
              dat$race == "" &
              dat$gender == "" &
              dat$ethnicity == "", ]


# Study 1: Analysis
# # # # # # # # # #
dat_long <- dat1 %>%
  gather(key = "variable", value = "value", R, I, A, S, E, C) %>%
  select(ai,gender,variable,value) %>% 
  group_by(ai,gender,variable) %>%
  summarise(mean = mean(value,na.rm = TRUE),SD = sd(value,na.rm = TRUE),N = sum(!is.nan(value)))

Fdat <- dat_long %>%
    filter(gender == "Female")

Mdat <- dat_long %>%
  filter(gender == "Male")

pdat <- Fdat %>% left_join(Mdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)
# Difference, female - male
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot

pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))

# Make table for stats

tdat <- data.frame("Ref." = pdat$gender.x,
           "Comp." = pdat$gender.y,
           "AI"=pdat$ai,
           "RIASEC" = pdat$`RIASEC Code`,
            "Est."= pdat$Dif,
            "L95"= pdat$CI_95_lower,
            "U95"= pdat$CI_95_upper,
            "L99"= pdat$CI_99_lower,
            "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)

Cap <- "Study 1: T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2,caption = Cap) %>%
          collapse_rows(valign = "top", latex_hline = "major") %>%
          kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study1stat.tex")


# Set color blind friendly palette for AI variable throughout script:
palette <- c(
  "Copilot" = "#E69F00", # Bright Green
  "Gemini" = "#56B4E9", # Bright Cyan
  "GPT-3.5" = "#009E73", # Bright Red
  "GPT-4" = "#D55E00", # Purple
  "Llama 2" = "#CC79A7"  # Bright Orange
)

riasec_levels <- c("C","E","S","A","I","R")
pdat$`RIASEC Code` <- factor(pdat$`RIASEC Code`, levels = riasec_levels)

pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Female Higher",size = 3) +
  theme_minimal() + 
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  theme(legend.position = "bottom") 
  
ggsave("../fig/Study1.pdf",width = 7,height = 8)

# Study 2, race main effect
# # # # # # # #
dat_long <- dat2 %>%
  gather(key = "variable", value = "value", R, I, A, S, E, C) %>%
  select(ai,race,variable,value) %>% 
  group_by(ai,race,variable) %>%
  summarise(mean = mean(value,na.rm = TRUE),SD = sd(value,na.rm = TRUE),N = sum(!is.nan(value)))

Bdat <- dat_long %>%
  filter(race == "Black")

Wdat <- dat_long %>%
  filter(race == "White")

pdat <- Bdat %>% left_join(Wdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)


# Difference, Black - White
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif


# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = pdat$race.x,
                   "Comp." = pdat$race.y,
                   "AI"=pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)

Cap <- "Study 2: T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major") %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study2stat.tex")



# Plot


pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Black Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  theme(legend.position = "bottom") 

ggsave("../fig/Study2.pdf",width = 7,height = 8)



# Study 3, Race by Gender Interaction
# # # # # # # # # # # # 
# Going to compare
# White men to black men
# White women to black women
# White women to white men
# Black women to black men
# Black women to white men
# White women to black men
# Breaking the processing up to the 6 comparisons
# Plotting separately and putting them together with grid.arrange

dat_long <- dat3 %>%
  gather(key = "variable", value = "value", R, I, A, S, E, C) %>%
  select(ai,race,gender,variable,value) %>% 
  group_by(ai,race,gender,variable) %>%
  summarise(mean = mean(value,na.rm = TRUE),SD = sd(value,na.rm = TRUE),N = sum(!is.nan(value)))

# White men to black men
# # # # # # # # # # # #

BMdat <- dat_long %>%
  filter(race == "Black" & gender == "Male")

WMdat <- dat_long %>%
  filter(race == "White" & gender == "Male")

pdat <- BMdat %>% left_join(WMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, Black Male - White Male
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = paste0(pdat$race.x," ",pdat$gender.x),
                   "Comp." = paste0(pdat$race.y," ",pdat$gender.y),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)

Cap <- "Study 3: Black Male and White Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study3_BM_WM_stat.tex")


# Number of significant RIASEC effects
count_sig(data = tdat,factor1 = "Ref.",factor2 = "Comp.",factor3 = "AI")


# Associated plot

P1 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Black Male Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)  + theme(axis.title.x = element_blank())

# White women to black women
# # # # # # #

BWdat <- dat_long %>%
  filter(race == "Black" & gender == "Female")

WWdat <- dat_long %>%
  filter(race == "White" & gender == "Female")

pdat <- BWdat %>% left_join(WWdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, female - male
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = paste0(pdat$race.x," ",pdat$gender.x),
                   "Comp." = paste0(pdat$race.y," ",pdat$gender.y),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)

Cap <- "Study 3: Black Female and White Female T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2,caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study3_BF_WF_stat.tex")

# Number of significant RIASEC effects
count_sig(data = tdat,factor1 = "Ref.",factor2 = "Comp.",factor3 = "AI")


# Associated plot

P2 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Female Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Black Female Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)  + theme(axis.title.x = element_blank())

# White women to white men
# # # # # # #

WWdat <- dat_long %>%
  filter(race == "White" & gender == "Female")

WMdat <- dat_long %>%
  filter(race == "White" & gender == "Male")

pdat <- WWdat %>% left_join(WMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, white female - white male
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = paste0(pdat$race.x," ",pdat$gender.x),
                   "Comp." = paste0(pdat$race.y," ",pdat$gender.y),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)

Cap <- "Study 3: White Female and White Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study3_WF_WM_stat.tex")

# Number of significant RIASEC effects
count_sig(data = tdat,factor1 = "Ref.",factor2 = "Comp.",factor3 = "AI")

# Associated plot

P3 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "White Female Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)  + theme(axis.title.x = element_blank())


# Black women to black men
# # # # # # #

BWdat <- dat_long %>%
  filter(race == "Black" & gender == "Female")

BMdat <- dat_long %>%
  filter(race == "Black" & gender == "Male")

pdat <- BWdat %>% left_join(BMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, Black female - Black male
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = paste0(pdat$race.x," ",pdat$gender.x),
                   "Comp." = paste0(pdat$race.y," ",pdat$gender.y),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 3: Black Female and Black Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study3_BF_BM_stat.tex")

# Number of significant RIASEC effects
count_sig(data = tdat,factor1 = "Ref.",factor2 = "Comp.",factor3 = "AI")


# Associated plot

P4 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "Black Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Black Female Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + theme(axis.title.x = element_blank())



# Black male by white female
# # #

BMdat <- dat_long %>%
  filter(race == "Black" & gender == "Male")

WFdat <- dat_long %>%
  filter(race == "White" & gender == "Female")


pdat <- WFdat %>% left_join(BMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, Black Male - White Female
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = paste0(pdat$race.x," ",pdat$gender.x),
                   "Comp." = paste0(pdat$race.y," ",pdat$gender.y),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 3: White Female and Black Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study3_BM_WF_stat.tex")

# Number of significant RIASEC effects
count_sig(data = tdat,factor1 = "Ref.",factor2 = "Comp.",factor3 = "AI")

# Associated plot
P5 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "Black Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "White Female Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)


# White male by black female
# # # # #

WMdat <- dat_long %>%
  filter(race == "White" & gender == "Male")

BFdat <- dat_long %>%
  filter(race == "Black" & gender == "Female")


pdat <- BFdat %>% left_join(WMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, White Male - Black Female
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = paste0(pdat$race.x," ",pdat$gender.x),
                   "Comp." = paste0(pdat$race.y," ",pdat$gender.y),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 3: Black Female and White Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study3_WM_BF_stat.tex")

# Number of significant RIASEC effects
count_sig(data = tdat,factor1 = "Ref.",factor2 = "Comp.",factor3 = "AI")

# Associated plot
P6 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Black Female Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) 


# Put it together in a grid

PP1 <- ggarrange(P1,P2,P3,P4,P5,P6, ncol=2, nrow=3, common.legend = TRUE, legend="bottom") 

ggsave(plot = PP1,"../fig/Study3.pdf",width = 7,height = 11)

# Study 4 A: Name discrimination (by race and gender)
# # # # # # # # # # # # # # # # #

# Must link names to stereotypes: 
# "Emily Walsh" = White Female (WWdat)
# "Greg Baker" = White Male (WMdat)
# "Lakisha Washington" = Black Female (BWdat)
# "Jamal Jones" = Black Male (BMdat)

dat_long <- dat4 %>%
  gather(key = "variable", value = "value", R, I, A, S, E, C) %>%
  select(ai,name,variable,value) %>% 
  group_by(ai,name,variable) %>%
  summarise(mean = mean(value,na.rm = TRUE),SD = sd(value,na.rm = TRUE),N = sum(!is.nan(value)))

# "Emily Walsh" = White Female (WWdat)
# # # # # # # # # # # # # # # #

EWdat <- dat_long %>%
  filter(name == "Emily Walsh")


pdat <- EWdat %>% left_join(WWdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, emily - white women
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif

# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = pdat$name,
                   "Comp." = paste0(pdat$race," ",pdat$gender),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 4: Emily Walsh and White Female T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study4_EW_WF_stat.tex")

# Associated plot

P1 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Female Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Emily Walsh Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)



# "Greg Baker" = White Male (WMdat)
# # # # # # # # # # # # # # # #

GBdat <- dat_long %>%
  filter(name == "Greg Baker")

pdat <- GBdat %>% left_join(WMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, greg - white men
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif


# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = pdat$name,
                   "Comp." = paste0(pdat$race," ",pdat$gender),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 4: Greg Baker by White Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study4_GB_WM_stat.tex")

# Associated Plot

P2 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "White Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Greg Baker Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)


# "Jamal Jones" = Black Male (BMdat)
# # # # # # # # # # # # # # # #

JJdat <- dat_long %>%
  filter(name == "Jamal Jones")

pdat <- JJdat %>% left_join(BMdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, Jamal - black men
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif


# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = pdat$name,
                   "Comp." = paste0(pdat$race," ",pdat$gender),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 4: Jamal Jones by Black Male T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study4_JJ_BM_stat.tex")

# Associated Plot

P3 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1,label = "Black Male Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1,label = "Jamal Jones Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)

# "Lakisha Washington" = Black Female (BWdat)
# # # # # # # # # # # # # # # #

LWdat <- dat_long %>%
  filter(name == "Lakisha Washington")

pdat <- LWdat %>% left_join(BWdat,by = c("ai","variable"))

# Adding 95% and 99% CI to pdat
pdat <- pdat %>%
  rowwise() %>%
  mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
         CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
  ungroup()

# Splitting CI columns into lower and upper bounds
pdat <- pdat %>%
  mutate(CI_95_lower = map_dbl(CI_95, "lower"),
         CI_95_upper = map_dbl(CI_95, "upper"),
         CI_99_lower = map_dbl(CI_99, "lower"),
         CI_99_upper = map_dbl(CI_99, "upper")) %>%
  select(-CI_95, -CI_99)

# Difference, Lakisha - black women
pdat$Dif <- pdat$mean.x - pdat$mean.y

# Renames for plot
pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
pdat$AI <- pdat$ai
pdat$`RIASEC Code` <- pdat$variable
pdat$`Mean Difference` <- pdat$Dif


# Stat tables
# Make table for stat

tdat <- data.frame("Ref." = pdat$name,
                   "Comp." = paste0(pdat$race," ",pdat$gender),
                   "AI"= pdat$ai,
                   "RIASEC" = pdat$`RIASEC Code`,
                   "Est."= pdat$Dif,
                   "L95"= pdat$CI_95_lower,
                   "U95"= pdat$CI_95_upper,
                   "L99"= pdat$CI_99_lower,
                   "U99"= pdat$CI_99_upper)

# Define the desired order for the RIASEC column
riasec_levels <- c("R", "I", "A", "S", "E", "C")

# Convert RIASEC column to a factor and specify the levels as the desired order
tdat$RIASEC <- factor(tdat$RIASEC, levels = riasec_levels)

# Arrange the dataframe by AI first and then by RIASEC
tdat_sorted <- tdat %>%
  arrange(AI, RIASEC)


Cap <- "Study 4: Lakisha Washington by Black Female T-test estimates and CI by RIASEC comparison and AI"

tabout <- kable(tdat_sorted,format = "latex",booktabs = T, digits = 2, caption = Cap) %>%
  collapse_rows(valign = "top", latex_hline = "major",columns = 1:3) %>%
  kable_styling(font_size = 8)

writeLines(tabout,"../tab/stat_tabs/study4_LW_BF_stat.tex")



P4 <- pdat %>%
  ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
  geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
  ylim(-2,2) +
  geom_hline(yintercept = 0,lty = 2) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "gray40") +
  # 95% CI Error Bars
  geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
                width = 0.35, position = position_dodge(0.9), color = "black") +
  coord_flip() +
  annotate(geom = "text",x = 6.5,y = -1.2,label = "Black Female Higher",size = 3) +
  annotate(geom = "text",x = 6.5,y = 1.2,label = "Lakisha Washington Higher",size = 3) +
  theme_minimal() + scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)

ggarrange(P2,P3,P1,P4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


ggsave("../fig/Study4A.pdf",width = 7,height = 10)


# Top jobs and coherence metrics
# # # # # # # # # # #

# Study 1
# # # # # # # # # # #


dat_long <- dat1 %>% 
  pivot_longer(cols = starts_with("occupation"), 
               names_to = "occupation_col", 
               values_to = "occupation")

occupation_freq <- dat_long %>%
  group_by(gender,ai, occupation) %>%
  summarize(frequency = n(), .groups = 'drop') %>%
  arrange(gender,ai,desc(frequency))

# Drop Blanks
occupation_freq <- occupation_freq %>%
  subset(occupation_freq$occupation != "")

top_occupations <- occupation_freq %>%
  group_by(gender,ai) %>%
  top_n(n = 5, wt = frequency) # Change '5' to however many top occupations you want

Ftab <- top_occupations %>%
  subset(gender == "Female")

Mtab <- top_occupations %>%
  subset(gender == "Male")

tab1dat <- data.frame(Ftab,Mtab[,c("occupation","frequency")])

tab1dat <- tab1dat[,-1]

header <- c("","Female" = 2,"Male" = 2)

colnames(tab1dat) <- c("AI","Occupation","Frequency","Occupation","Frequency")

# Convert data to LaTeX table format
kable(tab1dat, "latex", booktabs = T, col.names = NA, escape = F) %>%
  add_header_above(header) %>%
  collapse_rows(1,latex_hline = "none",valign = "top")


# Save table




# Study 1
# # #
# Calculate overlaps

Fdat <- unlist(c(dat1[dat1$gender == "Female",paste0("onet_soc_title_",1:10)]))
Ffactor <- rep(dat1[dat1$gender == "Female","ai"],10)

Mdat <- unlist(c(dat1[dat1$gender == "Male",paste0("onet_soc_title_",1:10)]))
Mfactor <- rep(dat1[dat1$gender == "Male","ai"],10)


# Stacked bar chart version
# # # # # # #
overlaps <- calculate_overlap_metrics2(Fdat, Mdat,Ffactor,Mfactor,Vec1Name = "Female",Vec2Name = "Male",
                                       n_permutations = 10000)

# Proportion plot
overlaps$plot_dat_prop$Variable <-  str_replace_all(overlaps$plot_dat_prop$Variable, pattern = "Prop",replacement = "Prop. ")

overlaps$plot_dat_prop$Variable <- factor(overlaps$plot_dat_prop$Variable,
                                          levels = c("Prop. Male","Jaccard","Prop. Female"))


overlaps$plot_dat_prop$Variable <- recode(overlaps$plot_dat_prop$Variable, "Jaccard" = "Overlap")


P1 <- ggplot(overlaps$plot_dat_prop, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "B.) Overlap and Uniqueness Proportions",
       y = "Proportion of Unique Recommendations",
       fill = "Comparisons") +
  theme_minimal() +theme(legend.position = "bottom") + coord_flip()


# Frequency plot

overlaps$plot_dat_freq$Variable <-  str_replace_all(overlaps$plot_dat_freq$Variable, pattern = "Unique",replacement = "Unique ")
overlaps$plot_dat_freq$Variable <-  str_replace_all(overlaps$plot_dat_freq$Variable, pattern = "_",replacement = "")

overlaps$plot_dat_freq$Variable <- factor(overlaps$plot_dat_freq$Variable,
                                          levels = c("Unique Male","Unique Overlap","Unique Female"))

P2 <- ggplot(overlaps$plot_dat_freq, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "       C.) Unique Job Frequency Overlap and Uniqueness",
       y = "Frequency of Unique Recommendations",
       fill = "Comparisons") +
  theme_minimal() +theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

PP <- grid.arrange(P1,P2)

ggsave(plot = PP,"../fig/overlap/Study1_overlap.pdf",width = 5,height = 8,unit = "in")


## table
#Props.
tab1 <- overlaps$plot_dat_prop
tab1 <- tab1 %>% spread(key = AI,value = Value)

# Freqs.
tab2 <- overlaps$plot_dat_freq
tab2 <- tab2 %>% spread(key = AI,value = Value)

# Pval
tab3 <- overlaps$plot_dat_pval
tab3$Value <- format.pval(tab3$Value,digits = 1,eps = 0.001)
tab3$Value <- ifelse(tab3$Value == "1.000",">0.999",tab3$Value)
tab3 <- tab3 %>% spread(key = AI,value = Value)

# Prop/Freq table 

tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")

tab1 <- kable(tab1,format = "latex",booktabs = T,digits = 2)


writeLines(tab1,"../tab/study1_tab.tex")


# p-val table
# Fix labels

tab3[1,1] <- "Overlap"
tab3[2,1] <- "Female"
tab3[3,1] <- "Male"

tab3 <- kable(tab3,format = "latex", escape = FALSE,booktabs = T)

writeLines(tab3,"../tab/study1_pval.tex")



### Unique Jobs Table
jobs <- generate_unique_lists(Fdat, Mdat,Ffactor,Mfactor,k = 5)

Index <- unique(jobs$top_shared_responses$Factor)

for(i in 1:length(Index)){
  AI <- Index[i]
  shared <- jobs$top_shared_responses[jobs$top_shared_responses$Factor == AI,]  
  Female <- jobs$top_unique_responses_1[jobs$top_unique_responses_1$Factor == AI,]  
  Male <- jobs$top_unique_responses_2[jobs$top_unique_responses_2$Factor == AI,]  
  
  Female$Count.y <- rep(0,nrow(Female))
  Male$Count.x <- rep(0,nrow(Male)) 
  JobTab <- rbind(shared,Female,Male)  
  JobTab <- JobTab[,-1]
  colnames(JobTab) <- c("Recommendation","Count Female","Count Male") 
  
  Caption <- paste0("Most Common Shared and Unique Recommendations by ",AI)
  
  JobTab <- kable(JobTab,format = "latex",booktabs = TRUE, caption = Caption)
  JobTab <- group_rows(JobTab,group_label = "Shared",start_row = 1,end_row = 5)
  JobTab <- group_rows(JobTab,group_label = "Unique Female",start_row = 6,end_row = 10)
  JobTab <- group_rows(JobTab,group_label = "Unique Male",start_row = 11,end_row = 15)

  TabName <- paste0("../tab/Study1_Unique_jobs_",AI,".tex")
  writeLines(JobTab,TabName)
    
}

# Study 2
# # # # # # # # #

Wdat <- unlist(c(dat2[dat2$race == "White",paste0("onet_soc_title_",1:10)]))
Wfactor <- rep(dat2[dat2$race == "White","ai"],10)

Bdat <- unlist(c(dat2[dat2$race == "Black",paste0("onet_soc_title_",1:10)]))
Bfactor <- rep(dat2[dat2$race == "Black","ai"],10)

overlaps <- calculate_overlap_metrics2(Wdat, Bdat,Wfactor,Bfactor,Vec1Name = "White",Vec2Name = "Black",
                                       n_permutations = 10000)

# Proportion plot
overlaps$plot_dat_prop$Variable <-  str_replace_all(overlaps$plot_dat_prop$Variable, pattern = "Prop",replacement = "Prop. ")

overlaps$plot_dat_prop$Variable <- factor(overlaps$plot_dat_prop$Variable,
                                          levels = c("Prop. Black","Jaccard","Prop. White"))

overlaps$plot_dat_prop$Variable <- recode(overlaps$plot_dat_prop$Variable, "Jaccard" = "Overlap")

P1 <- ggplot(overlaps$plot_dat_prop, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "B.) Overlap and Uniqueness Proportions",
       y = "Proportion of Unique Recommendations",
       fill = "Comparisons") +
  theme_minimal() +theme(legend.position = "bottom") + coord_flip()

# # # # # # FREQ

overlaps$plot_dat_freq$Variable <-  str_replace_all(overlaps$plot_dat_freq$Variable, pattern = "Unique",replacement = "Unique ")
overlaps$plot_dat_freq$Variable <-  str_replace_all(overlaps$plot_dat_freq$Variable, pattern = "_",replacement = "")

overlaps$plot_dat_freq$Variable <- factor(overlaps$plot_dat_freq$Variable,
                                          levels = c("Unique Black","Unique Overlap","Unique White"))

P2 <- ggplot(overlaps$plot_dat_freq, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "       C.) Job Frequency Overlap and Uniqueness",
       y = "Frequency of Unique Recommendations",
       fill = "Comparisons") +
  theme_minimal() +theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


PP <- grid.arrange(P1,P2)

ggsave(plot = PP,"../fig/overlap/Study2_overlap.pdf",width = 5,height = 8,unit = "in")


## table
#Props.
tab1 <- overlaps$plot_dat_prop
tab1 <- tab1 %>% spread(key = AI,value = Value)

# Freqs.
tab2 <- overlaps$plot_dat_freq
tab2 <- tab2 %>% spread(key = AI,value = Value)

# Pvals.
tab3 <- overlaps$plot_dat_pval
tab3$Value <- format.pval(tab3$Value,digits = 3,eps = 0.001)
tab3$Value <- ifelse(tab3$Value == "1.000",">0.999",tab3$Value)
tab3 <- tab3 %>% spread(key = AI,value = Value)

# Create prop/freq table
tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")

tab1 <- kable(tab1,format = "latex",booktabs = T,digits = 2)



writeLines(tab1,"../tab/study2_tab.tex")

# pval table
# Fix labels

tab3[1,1] <- "Overlap"
tab3[2,1] <- "White"
tab3[3,1] <- "Black"

tab3 <- kable(tab3,format = "latex", escape = FALSE,booktabs = T)

writeLines(tab3,"../tab/study2_pval.tex")


### Unique Jobs Table
jobs <- generate_unique_lists(Bdat, Wdat,Bfactor,Wfactor,k = 5)

Index <- unique(jobs$top_shared_responses$Factor)

for(i in 1:length(Index)){
  AI <- Index[i]
  shared <- adjust_rows(jobs$top_shared_responses[jobs$top_shared_responses$Factor == AI,])
  White <- adjust_rows(jobs$top_unique_responses_2[jobs$top_unique_responses_2$Factor == AI,])  
  Black <- adjust_rows(jobs$top_unique_responses_1[jobs$top_unique_responses_1$Factor == AI,])  
  
  White$Count.x <- rep(0,nrow(White))
  Black$Count.y <- rep(0,nrow(Black)) 
  JobTab <- rbind(shared,White,Black)  
  JobTab <- JobTab[,-1]
  colnames(JobTab) <- c("Recommendation","Count White","Count Black") 
  
  Caption <- paste0("Most Common Shared and Unique Recommendations by ",AI)
  
  JobTab <- kable(JobTab,format = "latex",booktabs = TRUE, caption = Caption)
  JobTab <- group_rows(JobTab,group_label = "Shared",start_row = 1,end_row = 5)
  JobTab <- group_rows(JobTab,group_label = "Unique White",start_row = 6,end_row = 10)
  JobTab <- group_rows(JobTab,group_label = "Unique Black",start_row = 11,end_row = 15)
  
  TabName <- paste0("../tab/Study2_Unique_jobs_",AI,".tex")
  writeLines(JobTab,TabName)
  
}



# Study 3
# # # # # # # # # #

WMdat <- unlist(c(dat3[dat3$race == "White" & 
                       dat3$gender == "Male", paste0("onet_soc_title_",1:10)]))
WMfactor<- rep(dat3[dat3$race == "White" & 
                  dat3$gender == "Male","ai"],10)

WFdat <- unlist(c(dat3[dat3$race == "White" & 
                         dat3$gender == "Female", paste0("onet_soc_title_",1:10)]))
WFfactor<- rep(dat3[dat3$race == "White" & 
                      dat3$gender == "Female","ai"],10)

BMdat <- unlist(c(dat3[dat3$race == "Black" & 
                         dat3$gender == "Male", paste0("onet_soc_title_",1:10)]))
BMfactor<- rep(dat3[dat3$race == "Black" & 
                      dat3$gender == "Male","ai"],10)

BFdat <- unlist(c(dat3[dat3$race == "Black" & 
                         dat3$gender == "Female", paste0("onet_soc_title_",1:10)]))
BFfactor<- rep(dat3[dat3$race == "Black" & 
                      dat3$gender == "Female","ai"],10)

# Black male Vs White male
overlaps1 <- calculate_overlap_metrics2(BMdat, WMdat,BMfactor,WMfactor,Vec1Name = "Black Male",Vec2Name = "White Male",
                                        n_permutations = 10000)
overlaps1$plot_dat_prop$Comparison <- rep("Black Male by White Male")
overlaps1$plot_dat_freq$Comparison <- rep("Black Male by White Male")
overlaps1$plot_dat_pval$Comparison <- rep("Black Male by White Male")

# Black Female Vs White Female
overlaps2 <- calculate_overlap_metrics2(BFdat, WFdat,BFfactor,WFfactor,Vec1Name = "Black Female",Vec2Name = "White Female",
                                        n_permutations = 10000)
overlaps2$plot_dat_prop$Comparison <- rep("Black Female by White Female")
overlaps2$plot_dat_freq$Comparison <- rep("Black Female by White Female")
overlaps2$plot_dat_pval$Comparison <- rep("Black Female by White Female")

# Black male Vs black Female
overlaps3 <- calculate_overlap_metrics2(BMdat, BFdat,BMfactor,BFfactor,Vec1Name = "Black Male",Vec2Name = "Black Female",
                                        n_permutations = 10000)
overlaps3$plot_dat_prop$Comparison <- rep("Black Male by Black Female")
overlaps3$plot_dat_freq$Comparison <- rep("Black Male by Black Female")
overlaps3$plot_dat_pval$Comparison <- rep("Black Male by Black Female")

# White male Vs white Female
overlaps4 <- calculate_overlap_metrics2(WMdat, WFdat,WMfactor,WFfactor,Vec1Name = "White Male",Vec2Name = "White Female",
                                        n_permutations = 10000)
overlaps4$plot_dat_prop$Comparison <- rep("White Male by White Female")
overlaps4$plot_dat_freq$Comparison <- rep("White Male by White Female")
overlaps4$plot_dat_pval$Comparison <- rep("White Male by White Female")

# White male Vs Black Female
overlaps5 <- calculate_overlap_metrics2(WMdat,BFdat,WMfactor,BFfactor,Vec1Name = "White Male",Vec2Name = "Black Female",
                                        n_permutations = 10000)
overlaps5$plot_dat_prop$Comparison <- rep("White Male by Black Female")
overlaps5$plot_dat_freq$Comparison <- rep("White Male by Black Female")
overlaps5$plot_dat_pval$Comparison <- rep("White Male by Black Female")

# White female Vs Black male
overlaps6 <- calculate_overlap_metrics2(WFdat,BMdat,WFfactor,BMfactor,Vec1Name = "White Female",Vec2Name = "Black Male",
                                        n_permutations = 10000)
overlaps6$plot_dat_prop$Comparison <- rep("White Female by Black Male")
overlaps6$plot_dat_freq$Comparison <- rep("White Female by Black Male")
overlaps6$plot_dat_pval$Comparison <- rep("White Female by Black Male")

# PROP
Pdat1 <- rbind(overlaps1$plot_dat_prop,
              overlaps2$plot_dat_prop,
              overlaps3$plot_dat_prop,
              overlaps4$plot_dat_prop,
              overlaps5$plot_dat_prop,
              overlaps6$plot_dat_prop)

# FREQ

Pdat2 <- rbind(overlaps1$plot_dat_freq,
               overlaps2$plot_dat_freq,
               overlaps3$plot_dat_freq,
               overlaps4$plot_dat_freq,
               overlaps5$plot_dat_freq,
               overlaps6$plot_dat_freq)
# Pval

Pdat3 <- rbind(overlaps1$plot_dat_pval,
               overlaps2$plot_dat_pval,
               overlaps3$plot_dat_pval,
               overlaps4$plot_dat_pval,
               overlaps5$plot_dat_pval,
               overlaps6$plot_dat_pval)

# Prop plot

Pdat1$Variable <- str_remove_all(string = Pdat1$Variable,pattern = "Prop")
 
Pdat1$Variable <- recode(Pdat1$Variable, "Jaccard" = "Overlap")

P1 <- ggplot(Pdat1, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "B.) Overlap Similarity and Uniqueness Proportions",
       y = "Proportion of Unique Recommendations",
       fill = "Comparisons") +
  theme_minimal() + facet_wrap(.~Comparison) + coord_flip()

# Freq plot

Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "Unique")
Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "_")

P2 <- ggplot(Pdat2, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "       C.) Job Frequency Overlap and Uniqueness",
       y = "Count",
       fill = "Comparisons") +
  theme_minimal() +
  facet_wrap(.~Comparison) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


PP <- grid.arrange(P1,P2)

ggsave(plot = PP,"../fig/overlap/Study3_overlap.pdf",width = 7,height = 10, units = "in")

# Table for paper, plot was dropped.

tab1 <- Pdat1 %>% spread(key = AI,value = Value)
tab1 <- tab1[order(tab1$Comparison),]

tab2 <- Pdat2 %>% spread(key = AI,value = Value)
tab2 <- tab2[order(tab2$Comparison),-1]


tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")


tab <- kable(tab1,format = "latex",booktabs = T,digits = 2)
tab <- collapse_rows(tab,latex_hline = "major",columns = 2 )

writeLines(tab,"../tab/study3_tab.tex")

# Pval table
tab3 <- Pdat3 %>%
  pivot_wider(names_from = AI, values_from = Value)

# Format Pvals
tab1$Gemini <- format.pval(tab3$Gemini,digits = 1,eps = 0.001)
tab1$Gemini <- ifelse(tab1$Gemini == "1.000",">0.999",tab1$Gemini)

tab1$`GPT-3.5` <- format.pval(tab3$`GPT-3.5`,digits = 1,eps = 0.001)
tab1$`GPT-3.5` <- ifelse(tab1$`GPT-3.5` == "1.000",">0.999",tab1$`GPT-3.5`)

tab1$`GPT-4` <- format.pval(tab3$`GPT-4`,digits = 1,eps = 0.001)
tab1$`GPT-4` <- ifelse(tab1$`GPT-4` == "1.000",">0.999",tab1$`GPT-4`)

tab1$`Llama 2` <- format.pval(tab3$`Llama 2`,digits = 1,eps = 0.001)
tab1$`Llama 2` <- ifelse(tab1$`Llama 2` == "1.000",">0.999",tab1$`Llama 2`)

tab <- kable(tab1, format = "latex", booktabs = TRUE)

tab <- tab %>%
  collapse_rows(latex_hline = "major", columns = 2)

# Frequency and Proportions table
writeLines(tab,"../tab/study3_pval.tex")



# Study 3 appendix tables
# # # # # # # # # # # # #

### Unique Jobs Table

jobs1 <- generate_unique_lists(BMdat, WMdat,BMfactor,WMfactor)

jobs2 <- generate_unique_lists(BFdat, WFdat,BFfactor,WFfactor)

jobs3 <- generate_unique_lists(BMdat, BFdat,BMfactor,BFfactor)

jobs4 <- generate_unique_lists(WMdat, WFdat,WMfactor,WFfactor)

JobList <- list(jobs1,jobs2,jobs3,jobs4)

Comp <- c("Black Male_White Male","Black Female_White Female","Black Male_Black Female","White Male_White Female")
Index <- unique(jobs1$top_shared_responses$Factor)
for(j in 1:length(Comp)){
  for(i in 1:length(Index)){
    Vec1Name <- str_split(Comp[j],"_",simplify = T)[1]
    Vec2Name <- str_split(Comp[j],"_",simplify = T)[2]
    jobs <- JobList[[j]]
    AI <- Index[i]
    shared <- adjust_rows(jobs$top_shared_responses[jobs$top_shared_responses$Factor == AI,])
    Vec2 <- adjust_rows(jobs$top_unique_responses_2[jobs$top_unique_responses_2$Factor == AI,])  
    Vec1 <- adjust_rows(jobs$top_unique_responses_1[jobs$top_unique_responses_1$Factor == AI,])  
    
    Vec2$Count.x <- rep(0,nrow(Vec2))
    Vec1$Count.y <- rep(0,nrow(Vec1)) 
    JobTab <- rbind(shared,Vec2,Vec1)  
    JobTab <- JobTab[,-1]
    
    colnames(JobTab) <- c("Recommendation", paste0("Count ",Vec1Name), paste0("Count ",Vec2Name)) 
    
    Caption <- paste0("Most Common Shared and Unique Recommendations by ",AI)
    
    JobTab <- kable(JobTab,format = "latex",booktabs = TRUE, caption = Caption,)
    JobTab <- group_rows(JobTab,group_label = "Shared",start_row = 1,end_row = 5)
    JobTab <- group_rows(JobTab,group_label = Vec2Name,start_row = 6,end_row = 10)
    JobTab <- group_rows(JobTab,group_label = Vec1Name,start_row = 11,end_row = 15)
    JobTab <- kable_styling(JobTab,font_size = 7)
    TabName <- paste0("../tab/study3/Study3_Unique_jobs_",AI,"_",str_remove(Comp[j], " "),".tex")
    writeLines(JobTab,TabName)
    
  }
  
}


# Study 4
# # # # # # # # #

EWdat <- unlist(c(dat4[dat4$name == "Emily Walsh", paste0("onet_soc_title_",1:10)]))
EWFactor <- rep(dat4[dat4$name == "Emily Walsh","ai"],10)

GBdat <- unlist(c(dat4[dat4$name == "Greg Baker", paste0("onet_soc_title_",1:10)]))
GBFactor <- rep(dat4[dat4$name == "Greg Baker","ai"],10)

LWdat <- unlist(c(dat4[dat4$name == "Lakisha Washington", paste0("onet_soc_title_",1:10)]))
LWFactor <- rep(dat4[dat4$name == "Lakisha Washington","ai"],10)

JJdat <- unlist(c(dat4[dat4$name == "Jamal Jones", paste0("onet_soc_title_",1:10)]))
JJFactor <- rep(dat4[dat4$name == "Jamal Jones","ai"],10)

# EW Vs White Female
overlaps1 <- calculate_overlap_metrics2(WFdat, EWdat,WFfactor,EWFactor,Vec1Name = "White Female",Vec2Name = "Emily Walsh",
                                        n_permutations = 10000)
overlaps1$plot_dat_prop$Comparison <- rep("Emily Walsh by White Female")
overlaps1$plot_dat_freq$Comparison <- rep("Emily Walsh by White Female")
overlaps1$plot_dat_pval$Comparison <- rep("Emily Walsh by White Female")

# GB Vs white Male
overlaps2 <- calculate_overlap_metrics2(GBdat, WMdat,GBFactor,WMfactor,Vec1Name = "Greg Baker",Vec2Name = "White Male",
                                        n_permutations = 10000)
overlaps2$plot_dat_prop$Comparison <- rep("Greg Baker by White Male")
overlaps2$plot_dat_freq$Comparison <- rep("Greg Baker by White Male")
overlaps2$plot_dat_pval$Comparison <- rep("Greg Baker by White Male")

# JJ Vs Black Male
overlaps3 <-calculate_overlap_metrics2(BMdat, JJdat,BMfactor,JJFactor,Vec1Name = "Black Male",Vec2Name = "Jamal Jones",
                                       n_permutations = 10000)
overlaps3$plot_dat_prop$Comparison <- rep("Jamal Jones by Black Male")
overlaps3$plot_dat_freq$Comparison <- rep("Jamal Jones by Black Male")
overlaps3$plot_dat_pval$Comparison <- rep("Jamal Jones by Black Male")


# LW Vs Black Female
overlaps4 <- calculate_overlap_metrics2(LWdat, BFdat,LWFactor,BFfactor,Vec1Name = "Lakisha Washington",Vec2Name = "Black Female",
                                        n_permutations = 10000)
overlaps4$plot_dat_freq$Comparison <- rep("Lakisha Washington by Black Female")
overlaps4$plot_dat_prop$Comparison <- rep("Lakisha Washington by Black Female")
overlaps4$plot_dat_pval$Comparison <- rep("Lakisha Washington by Black Female")


Pdat1 <- rbind(overlaps1$plot_dat_prop,
               overlaps2$plot_dat_prop,
               overlaps3$plot_dat_prop,
               overlaps4$plot_dat_prop)

Pdat1$Variable <- str_remove_all(string = Pdat1$Variable,pattern = "Prop")

# Hand removing the Emily Walsh Gemini Column and Lakisha Washington

Pdat1$Variable <- recode(Pdat1$Variable, "Jaccard" = "Overlap")

Pdat1[Pdat1$AI == "Gemini" & Pdat1$Comparison == "Emily Walsh by White Female","Value"] <- 0
Pdat1[Pdat1$AI == "Gemini" & Pdat1$Comparison == "Lakisha Washington by Black Female","Value"] <- 0

P1 <- ggplot(Pdat1, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
       labs(x = "Conversational AI System",title = "B.) Overlap and Uniqueness Proportions",
       y = "Proportion of Unique Recommendations",
       fill = "Comparisons") +
  theme_minimal() + facet_wrap(.~Comparison) + coord_flip()


# FREQ
Pdat2 <- rbind(overlaps1$plot_dat_freq,
               overlaps2$plot_dat_freq,
               overlaps3$plot_dat_freq,
               overlaps4$plot_dat_freq)

Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "Unique")
Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "_")

P2 <- ggplot(Pdat2, aes(x = AI, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  labs(x = "Conversational AI System",title = "       C.) Job Frequency Overlap and Uniqueness",
       y = "Count",
       fill = "Comparisons") +
  theme_minimal() +
  facet_wrap(.~Comparison) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


PP <- grid.arrange(P1,P2)


ggsave(plot = PP,"../fig/overlap/Study4_overlap.pdf",width = 7,height = 10, units = "in")


# LEgacy table before out of expectation comparisons were added.
# 
# tab1 <- Pdat1 %>% spread(key = AI,value = Value)
# tab1 <- tab1[order(tab1$Comparison),]
# 
# tab2 <- Pdat2 %>% spread(key = AI,value = Value)
# tab2 <- tab2[order(tab2$Comparison),-1]
# 
# 
# tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
# tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
# tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
# tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")
# 
# 
# 
# tab <- kable(tab1,format = "latex",booktabs = T,digits = 2)
# tab <- collapse_rows(tab,latex_hline = "major",columns = 2 )
# 
# writeLines(tab,"../tab/study4_tab.tex")


# Study 4 Unique Jobs Table
# # # # # # #

jobs1 <- generate_unique_lists(WFdat, EWdat,WFfactor,EWFactor)

jobs2 <- generate_unique_lists(GBdat, WMdat,GBFactor,WMfactor)

jobs3 <- generate_unique_lists(BMdat, JJdat,BMfactor,JJFactor)

jobs4 <- generate_unique_lists(LWdat, BFdat,LWFactor,BFfactor)

JobList <- list(jobs1,jobs2,jobs3,jobs4)

Comp <- c("White Female_Emily Walsh","Greg Baker_White Male","Black Male_Jamal jones","Lakisha Washington_Black Female")
Index <- unique(jobs1$top_shared_responses$Factor)
for(j in 1:length(Comp)){
  for(i in 1:length(Index)){
    Vec1Name <- str_split(Comp[j],"_",simplify = T)[1]
    Vec2Name <- str_split(Comp[j],"_",simplify = T)[2]
    jobs <- JobList[[j]]
    AI <- Index[i]
    shared <- adjust_rows(jobs$top_shared_responses[jobs$top_shared_responses$Factor == AI,])
    Vec2 <- adjust_rows(jobs$top_unique_responses_2[jobs$top_unique_responses_2$Factor == AI,])  
    Vec1 <- adjust_rows(jobs$top_unique_responses_1[jobs$top_unique_responses_1$Factor == AI,])  
    
    Vec2$Count.x <- rep(0,nrow(Vec2))
    Vec1$Count.y <- rep(0,nrow(Vec1)) 
    JobTab <- rbind(shared,Vec2,Vec1)  
    JobTab <- JobTab[,-1]
    
    colnames(JobTab) <- c("Recommendation", paste0("Count ",Vec1Name), paste0("Count ",Vec2Name)) 
    
    Caption <- paste0("Most Common Shared and Unique Recommendations by ",AI)
    
    JobTab <- kable(JobTab,format = "latex",booktabs = TRUE, caption = Caption,)
    JobTab <- group_rows(JobTab,group_label = "Shared",start_row = 1,end_row = 5)
    JobTab <- group_rows(JobTab,group_label = Vec2Name,start_row = 6,end_row = 10)
    JobTab <- group_rows(JobTab,group_label = Vec1Name,start_row = 11,end_row = 15)
    JobTab <- kable_styling(JobTab,font_size = 7)
    TabName <- paste0("../tab/study4/Study4_Unique_jobs_",AI,"_",str_remove(Comp[j], " "),".tex")
    writeLines(JobTab,TabName)
    
  }
  
}


# Overlap Comparison Across Non Comparisons.  
# Study 4 - additional comparisons
# # # # # # # # # # #
EWdat <- unlist(c(dat4[dat4$name == "Emily Walsh", paste0("onet_soc_title_",1:10)]))
EWFactor <- rep(dat4[dat4$name == "Emily Walsh","ai"],10)

GBdat <- unlist(c(dat4[dat4$name == "Greg Baker", paste0("onet_soc_title_",1:10)]))
GBFactor <- rep(dat4[dat4$name == "Greg Baker","ai"],10)

LWdat <- unlist(c(dat4[dat4$name == "Lakisha Washington", paste0("onet_soc_title_",1:10)]))
LWFactor <- rep(dat4[dat4$name == "Lakisha Washington","ai"],10)

JJdat <- unlist(c(dat4[dat4$name == "Jamal Jones", paste0("onet_soc_title_",1:10)]))
JJFactor <- rep(dat4[dat4$name == "Jamal Jones","ai"],10)

WMdat <- unlist(c(dat3[dat3$race == "White" & 
                         dat3$gender == "Male", paste0("onet_soc_title_",1:10)]))
WMfactor<- rep(dat3[dat3$race == "White" & 
                      dat3$gender == "Male","ai"],10)

WFdat <- unlist(c(dat3[dat3$race == "White" & 
                         dat3$gender == "Female", paste0("onet_soc_title_",1:10)]))
WFfactor<- rep(dat3[dat3$race == "White" & 
                      dat3$gender == "Female","ai"],10)

BMdat <- unlist(c(dat3[dat3$race == "Black" & 
                         dat3$gender == "Male", paste0("onet_soc_title_",1:10)]))
BMfactor<- rep(dat3[dat3$race == "Black" & 
                      dat3$gender == "Male","ai"],10)

BFdat <- unlist(c(dat3[dat3$race == "Black" & 
                         dat3$gender == "Female", paste0("onet_soc_title_",1:10)]))
BFfactor<- rep(dat3[dat3$race == "Black" & 
                      dat3$gender == "Female","ai"],10)


# Full overlap table and pval table
# # # # # # # #

# I apologize for this... 
# EW Vs White Female
overlaps1 <- calculate_overlap_metrics2(WFdat, EWdat,WFfactor,EWFactor,Vec1Name = "White Female",Vec2Name = "Emily Walsh",
                                       n_permutations = 10000)
overlaps1$plot_dat_prop$Comparison <- rep("Emily Walsh by White Female")
overlaps1$plot_dat_freq$Comparison <- rep("Emily Walsh by White Female")
overlaps1$plot_dat_pval$Comparison <- rep("Emily Walsh by White Female")

# EW Vs Others
overlaps1ew <- calculate_overlap_metrics2(EWdat,WMdat,EWFactor,WMfactor,Vec1Name = "Emily Walsh",Vec2Name = "White Male",
                                         n_permutations = 10000)
overlaps1ew$plot_dat_prop$Comparison <- rep("Emily Walsh by White Male")
overlaps1ew$plot_dat_freq$Comparison <- rep("Emily Walsh by White Male")
overlaps1ew$plot_dat_pval$Comparison <- rep("Emily Walsh by White Male")

overlaps2ew <- calculate_overlap_metrics2(EWdat,BMdat,EWFactor,BMfactor,Vec1Name = "Emily Walsh",Vec2Name = "Black Male",
                                         n_permutations = 10000)
overlaps2ew$plot_dat_prop$Comparison <- rep("Emily Walsh by Black Male")
overlaps2ew$plot_dat_freq$Comparison <- rep("Emily Walsh by Black Male")
overlaps2ew$plot_dat_pval$Comparison <- rep("Emily Walsh by Black Male")

overlaps3ew <- calculate_overlap_metrics2(EWdat,BFdat,EWFactor,BFfactor,Vec1Name = "Emily Walsh",Vec2Name = "Black Female",
                                         n_permutations = 10000)
overlaps3ew$plot_dat_prop$Comparison <- rep("Emily Walsh by Black Female")
overlaps3ew$plot_dat_freq$Comparison <- rep("Emily Walsh by Black Female")
overlaps3ew$plot_dat_pval$Comparison <- rep("Emily Walsh by Black Female")


# GB Vs white Male
overlaps2 <- calculate_overlap_metrics2(GBdat, WMdat,GBFactor,WMfactor,Vec1Name = "Greg Baker",Vec2Name = "White Male",
                                       n_permutations = 10000)
overlaps2$plot_dat_prop$Comparison <- rep("Greg Baker by White Male")
overlaps2$plot_dat_freq$Comparison <- rep("Greg Baker by White Male")
overlaps2$plot_dat_pval$Comparison <- rep("Greg Baker by White Male")

# GB Vs Others
overlaps1gb <- calculate_overlap_metrics2(GBdat,WFdat,GBFactor,WFfactor,Vec1Name = "Greg Baker",Vec2Name = "White Female",
                                         n_permutations = 10000)
overlaps1gb$plot_dat_prop$Comparison <- rep("Greg Baker by White Female")
overlaps1gb$plot_dat_freq$Comparison <- rep("Greg Baker by White Female")
overlaps1gb$plot_dat_pval$Comparison <- rep("Greg Baker by White Female")


overlaps2gb <- calculate_overlap_metrics2(GBdat,BMdat,GBFactor,BMfactor,Vec1Name = "Greg Baker",Vec2Name = "Black Male",
                                         n_permutations = 10000)
overlaps2gb$plot_dat_prop$Comparison <- rep("Greg Baker by Black Male")
overlaps2gb$plot_dat_freq$Comparison <- rep("Greg Baker by Black Male")
overlaps2gb$plot_dat_pval$Comparison <- rep("Greg Baker by Black Male")

overlaps3gb <- calculate_overlap_metrics2(GBdat,BFdat,GBFactor,BFfactor,Vec1Name = "Greg Baker",Vec2Name = "Black Female",
                                         n_permutations = 10000)
overlaps3gb$plot_dat_prop$Comparison <- rep("Greg Baker by Black Female")
overlaps3gb$plot_dat_freq$Comparison <- rep("Greg Baker by Black Female")
overlaps3gb$plot_dat_pval$Comparison <- rep("Greg Baker by Black Female")

# JJ Vs Black Male
overlaps3 <-calculate_overlap_metrics2(BMdat, JJdat,BMfactor,JJFactor,Vec1Name = "Black Male",Vec2Name = "Jamal Jones",
                                      n_permutations = 10000)
overlaps3$plot_dat_prop$Comparison <- rep("Jamal Jones by Black Male")
overlaps3$plot_dat_freq$Comparison <- rep("Jamal Jones by Black Male")
overlaps3$plot_dat_pval$Comparison <- rep("Jamal Jones by Black Male")

# JJ Vs Others
overlaps1jj <- calculate_overlap_metrics2(JJdat,WMdat,JJFactor,WMfactor,Vec1Name = "Jamal Jones",Vec2Name = "White Male",
                                         n_permutations = 10000)
overlaps1jj$plot_dat_prop$Comparison <- rep("Jamal Jones by White Male")
overlaps1jj$plot_dat_freq$Comparison <- rep("Jamal Jones by White Male")
overlaps1jj$plot_dat_pval$Comparison <- rep("Jamal Jones by White Male")

overlaps2jj <- calculate_overlap_metrics2(JJdat,WFdat,JJFactor,WFfactor,Vec1Name = "Jamal Jones",Vec2Name = "White Female",
                                         n_permutations = 10000)
overlaps2jj$plot_dat_prop$Comparison <- rep("Jamal Jones by White Female")
overlaps2jj$plot_dat_freq$Comparison <- rep("Jamal Jones by White Female")
overlaps2jj$plot_dat_pval$Comparison <- rep("Jamal Jones by White Female")

overlaps3jj <- calculate_overlap_metrics2(JJdat,BFdat,JJFactor,BFfactor,Vec1Name = "Jamal Jones",Vec2Name = "Black Female",
                                         n_permutations = 10000)
overlaps3jj$plot_dat_prop$Comparison <- rep("Jamal Jones by Black Female")
overlaps3jj$plot_dat_freq$Comparison <- rep("Jamal Jones by Black Female")
overlaps3jj$plot_dat_pval$Comparison <- rep("Jamal Jones by Black Female")

# LW Vs Black Female
overlaps4 <- calculate_overlap_metrics2(LWdat, BFdat,LWFactor,BFfactor,Vec1Name = "Lakisha Washington",Vec2Name = "Black Female",
                                       n_permutations = 10000)
overlaps4$plot_dat_freq$Comparison <- rep("Lakisha Washington by Black Female")
overlaps4$plot_dat_prop$Comparison <- rep("Lakisha Washington by Black Female")
overlaps4$plot_dat_pval$Comparison <- rep("Lakisha Washington by Black Female")

# LW Vs Others
overlaps1lw <- calculate_overlap_metrics2(LWdat,WMdat,LWFactor,WMfactor,Vec1Name = "Lakisha Washington",Vec2Name = "White Male",
                                         n_permutations = 10000)
overlaps1lw$plot_dat_prop$Comparison <- rep("Lakisha Washington by White Male")
overlaps1lw$plot_dat_freq$Comparison <- rep("Lakisha Washington by White Male")
overlaps1lw$plot_dat_pval$Comparison <- rep("Lakisha Washington by White Male")

overlaps2lw <- calculate_overlap_metrics2(LWdat,WFdat,LWFactor,WFfactor,Vec1Name = "Lakisha Washington",Vec2Name = "White Female",
                                         n_permutations = 10000)
overlaps2lw$plot_dat_prop$Comparison <- rep("Lakisha Washington by White Female")
overlaps2lw$plot_dat_freq$Comparison <- rep("Lakisha Washington by White Female")
overlaps2lw$plot_dat_pval$Comparison <- rep("Lakisha Washington by White Female")


overlaps3lw <- calculate_overlap_metrics2(LWdat,BMdat,LWFactor,BMfactor,Vec1Name = "Lakisha Washington",Vec2Name = "Black Male",
                                         n_permutations = 10000)
overlaps3lw$plot_dat_prop$Comparison <- rep("Lakisha Washington by Black Male")
overlaps3lw$plot_dat_freq$Comparison <- rep("Lakisha Washington by Black Male")
overlaps3lw$plot_dat_pval$Comparison <- rep("Lakisha Washington by Black Male")

Pdatew <- rbind(overlaps1$plot_dat_prop,
                overlaps1ew$plot_dat_prop,
                overlaps2ew$plot_dat_prop,
                overlaps3ew$plot_dat_prop)
              
Pdatgb <-rbind(overlaps2$plot_dat_prop,
               overlaps1gb$plot_dat_prop,
               overlaps2gb$plot_dat_prop,
               overlaps3gb$plot_dat_prop)
              
Pdatjj <-rbind(overlaps3$plot_dat_prop,
               overlaps1jj$plot_dat_prop,
               overlaps2jj$plot_dat_prop,
               overlaps3jj$plot_dat_prop)
              
Pdatlw <-rbind(overlaps4$plot_dat_prop,
               overlaps1lw$plot_dat_prop,
               overlaps2lw$plot_dat_prop,
               overlaps3lw$plot_dat_prop)

# # # # # # # Freq

Pdat2ew <- rbind(overlaps1$plot_dat_freq,
                 overlaps1ew$plot_dat_freq,
                 overlaps2ew$plot_dat_freq,
                 overlaps3ew$plot_dat_freq)

Pdat2gb <- rbind(overlaps2$plot_dat_freq,
                 overlaps1gb$plot_dat_freq,
                 overlaps2gb$plot_dat_freq,
                 overlaps3gb$plot_dat_freq)
               
Pdat2jj <- rbind(overlaps3$plot_dat_freq,
                 overlaps1jj$plot_dat_freq,
                 overlaps2jj$plot_dat_freq,
                 overlaps3jj$plot_dat_freq)
               
Pdat2lw <- rbind(overlaps4$plot_dat_freq,
                 overlaps1lw$plot_dat_freq,
                 overlaps2lw$plot_dat_freq,
                 overlaps3lw$plot_dat_freq)

# # # # # # # Pval

Pdat3ew <- rbind(overlaps1$plot_dat_pval,
                 overlaps1ew$plot_dat_pval,
                 overlaps2ew$plot_dat_pval,
                 overlaps3ew$plot_dat_pval)

Pdat3gb <- rbind(overlaps2$plot_dat_pval,
                 overlaps1gb$plot_dat_pval,
                 overlaps2gb$plot_dat_pval,
                 overlaps3gb$plot_dat_pval)

Pdat3jj <- rbind(overlaps3$plot_dat_pval,
                 overlaps1jj$plot_dat_pval,
                 overlaps2jj$plot_dat_pval,
                 overlaps3jj$plot_dat_pval)

Pdat3lw <- rbind(overlaps4$plot_dat_pval,
                 overlaps1lw$plot_dat_pval,
                 overlaps2lw$plot_dat_pval,
                 overlaps3lw$plot_dat_pval)

# # # # # # # Group

Propdat <- rbind(Pdatew,
                 Pdatgb, 
                 Pdatjj, 
                 Pdatlw)

Freqdat<- rbind(Pdat2ew, 
                Pdat2gb, 
                Pdat2jj, 
                Pdat2lw)

Pvaldat<- rbind(Pdat3ew, 
                Pdat3gb, 
                Pdat3jj, 
                Pdat3lw) 

# # # # # # # # # 
# Tabularize

tab1 <- Propdat %>%
  pivot_wider(names_from = AI, values_from = Value)

tab2 <- Freqdat %>%
  pivot_wider(names_from = AI, values_from = Value)

tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")

tab <- kable(tab1, format = "latex", booktabs = TRUE)

tab <- tab %>%
  collapse_rows(latex_hline = "major", columns = 2)

# Frequency and Proportions table
writeLines(tab,"../tab/study4.tex")

### Significance table
# Pval table

Pvaldat$Value <- format.pval(Pvaldat$Value,digits = 1,eps = 0.001)
Pvaldat$Value <- ifelse(Pvaldat$Value == "1.000",">0.999",Pvaldat$Value)

tab3 <- Pvaldat %>%
  pivot_wider(names_from = AI, values_from = Value)

tab3 <- data.frame(tab1$Variable,tab3[,2:ncol(tab3)])

tab <- kable(tab3, format = "latex", booktabs = TRUE)

tab <- tab %>%
  collapse_rows(latex_hline = "major", columns = 2)

# Frequency and Proportions table
writeLines(tab,"../tab/study4_pval.tex")



# Study 1: Comparison to dat0: null human Table
# # #
# Study 1
# # #
# Calculate overlaps
Fdat <- unlist(c(dat1[dat1$gender == "Female",paste0("onet_soc_title_",1:10)]))
Ffactor <- rep(dat1[dat1$gender == "Female","ai"],10)

Mdat <- unlist(c(dat1[dat1$gender == "Male",paste0("onet_soc_title_",1:10)]))
Mfactor <- rep(dat1[dat1$gender == "Male","ai"],10)

NNdat <- unlist(c(dat0[dat0$name == "", paste0("onet_soc_title_",1:10)]))
NNfactor <- rep(dat0[dat0$name == "","ai"],10)


# Stacked bar chart version
# # # # # # #
overlaps1 <- calculate_overlap_metrics(Fdat, NNdat,Ffactor,NNfactor,Vec1Name = "Female",Vec2Name = "Static")
overlaps1$plot_dat_prop$Variable <-  str_replace_all(overlaps1$plot_dat_prop$Variable, pattern = "Prop",replacement = "Prop. ")
overlaps1$plot_dat_prop$Variable <- factor(overlaps1$plot_dat_prop$Variable,
                                          levels = c("Prop. Static","Jaccard","Prop. Female"))

overlaps2 <- calculate_overlap_metrics(Mdat, NNdat,Mfactor,NNfactor,Vec1Name = "Male",Vec2Name = "Static")
overlaps2$plot_dat_prop$Variable <-  str_replace_all(overlaps2$plot_dat_prop$Variable, pattern = "Prop",replacement = "Prop. ")
overlaps2$plot_dat_prop$Variable <- factor(overlaps2$plot_dat_prop$Variable,
                                           levels = c("Prop. Male","Jaccard","Prop. Static"))


overlaps1$plot_dat_prop
overlaps2$plot_dat_prop
overlaps1$plot_dat_freq
overlaps2$plot_dat_freq


## table
#Props.
tab1 <- overlaps$plot_dat_prop
tab1 <- tab1 %>% spread(key = AI,value = Value)

# Freqs.
tab2 <- overlaps$plot_dat_freq
tab2 <- tab2 %>% spread(key = AI,value = Value)

tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")

tab1 <- kable(tab1,format = "latex",booktabs = T,digits = 2)


writeLines(tab1,"../tab/study1_tab.tex")



# Stars are broken FIX it XXXXXX




# Study 2: Comparison to dat0: null human Table
# # #





# Study 3: Comparison to dat0: null human Table
# # #

WMdat <- unlist(c(dat3[dat3$race == "White" & 
                         dat3$gender == "Male", paste0("onet_soc_title_",1:10)]))
WMfactor<- rep(dat3[dat3$race == "White" & 
                      dat3$gender == "Male","ai"],10)

WFdat <- unlist(c(dat3[dat3$race == "White" & 
                         dat3$gender == "Female", paste0("onet_soc_title_",1:10)]))
WFfactor<- rep(dat3[dat3$race == "White" & 
                      dat3$gender == "Female","ai"],10)

BMdat <- unlist(c(dat3[dat3$race == "Black" & 
                         dat3$gender == "Male", paste0("onet_soc_title_",1:10)]))
BMfactor<- rep(dat3[dat3$race == "Black" & 
                      dat3$gender == "Male","ai"],10)

BFdat <- unlist(c(dat3[dat3$race == "Black" & 
                         dat3$gender == "Female", paste0("onet_soc_title_",1:10)]))
BFfactor<- rep(dat3[dat3$race == "Black" & 
                      dat3$gender == "Female","ai"],10)

NNdat <- unlist(c(dat0[dat0$name == "", paste0("onet_soc_title_",1:10)]))
NNfactor <- rep(dat0[dat0$name == "","ai"],10)


# Black male Vs White male
overlaps1 <- calculate_overlap_metrics(BMdat, NNdat,BMfactor,NNfactor,Vec1Name = "Black Male",Vec2Name = "Static Profile")
overlaps1$plot_dat_prop$Comparison <- rep("Black Male by Static Profile")
overlaps1$plot_dat_freq$Comparison <- rep("Black Male by Static Profile")

# Black Female Vs White Female
overlaps2 <- calculate_overlap_metrics(BFdat, NNdat,BFfactor,NNfactor,Vec1Name = "Black Female",Vec2Name = "Static Profile")
overlaps2$plot_dat_prop$Comparison <- rep("Black Female by Static Profile")
overlaps2$plot_dat_freq$Comparison <- rep("Black Female by Static Profile")

# Black male Vs black Female
overlaps3 <- calculate_overlap_metrics(WFdat, NNdat,WFfactor,NNfactor,Vec1Name = "White Female",Vec2Name = "Static Profile")
overlaps3$plot_dat_prop$Comparison <- rep("White Female by Static Profile")
overlaps3$plot_dat_freq$Comparison <- rep("White Female by Static Profile")

# White male Vs white Female
overlaps4 <- calculate_overlap_metrics(WMdat, NNdat,WMfactor,NNfactor,Vec1Name = "White Male",Vec2Name = "Static Profile")
overlaps4$plot_dat_prop$Comparison <- rep("White Male by Static Profile")
overlaps4$plot_dat_freq$Comparison <- rep("White Male by Static Profile")

Pdat1 <- rbind(overlaps1$plot_dat_prop,
               overlaps2$plot_dat_prop,
               overlaps3$plot_dat_prop,
               overlaps4$plot_dat_prop)

Pdat1$Variable <- str_remove_all(string = Pdat1$Variable,pattern = "Prop")

# FREQ
Pdat2 <- rbind(overlaps1$plot_dat_freq,
               overlaps2$plot_dat_freq,
               overlaps3$plot_dat_freq,
               overlaps4$plot_dat_freq)

Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "Unique")

Pdat2$Variable <- ifelse(Pdat2$Variable == "_Overlap","Jaccard",Pdat2$Variable)

tab1 <- Pdat1 %>% spread(key = AI,value = Value)
tab1 <- tab1[order(tab1$Comparison),]

tab2 <- Pdat2 %>% spread(key = AI,value = Value)
tab2 <- tab2[order(tab2$Comparison),-1]


tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")

tab <- kable(tab1,format = "latex",booktabs = T,digits = 2)
tab <- collapse_rows(tab,latex_hline = "major",columns = 2 )

writeLines(tab,"../tab/study3_No_Name_tab.tex")


# Study 4: Comparison to dat0: null human Table
# # #

EWdat <- unlist(c(dat4[dat4$name == "Emily Walsh", paste0("onet_soc_title_",1:10)]))
EWFactor <- rep(dat4[dat4$name == "Emily Walsh","ai"],10)

GBdat <- unlist(c(dat4[dat4$name == "Greg Baker", paste0("onet_soc_title_",1:10)]))
GBFactor <- rep(dat4[dat4$name == "Greg Baker","ai"],10)

LWdat <- unlist(c(dat4[dat4$name == "Lakisha Washington", paste0("onet_soc_title_",1:10)]))
LWFactor <- rep(dat4[dat4$name == "Lakisha Washington","ai"],10)

JJdat <- unlist(c(dat4[dat4$name == "Jamal Jones", paste0("onet_soc_title_",1:10)]))
JJFactor <- rep(dat4[dat4$name == "Jamal Jones","ai"],10)

NNdat <- unlist(c(dat0[dat0$name == "", paste0("onet_soc_title_",1:10)]))
NNfactor <- rep(dat0[dat0$name == "","ai"],10)


# EW Vs Static
overlaps1 <- calculate_overlap_metrics(NNdat, EWdat,NNfactor,EWFactor,Vec1Name = "Static Profile",Vec2Name = "Emily Walsh")
overlaps1$plot_dat_prop$Comparison <- rep("Emily Walsh by Static Profile")
overlaps1$plot_dat_freq$Comparison <- rep("Emily Walsh by Static Profile")

# GB Vs Static
overlaps2 <- calculate_overlap_metrics(GBdat, NNdat,GBFactor,NNfactor,Vec1Name = "Greg Baker",Vec2Name = "Static Profile")
overlaps2$plot_dat_prop$Comparison <- rep("Greg Baker by Static Profile")
overlaps2$plot_dat_freq$Comparison <- rep("Greg Baker by Static Profile")

# JJ Vs Static
overlaps3 <-calculate_overlap_metrics(NNdat, JJdat,NNfactor,JJFactor,Vec1Name = "Static Profile",Vec2Name = "Jamal Jones")
overlaps3$plot_dat_prop$Comparison <- rep("Jamal Jones by Static Profile")
overlaps3$plot_dat_freq$Comparison <- rep("Jamal Jones by Static Profile")

# LW Vs Static
overlaps4 <- calculate_overlap_metrics(LWdat, NNdat,LWFactor,NNfactor,Vec1Name = "Lakisha Washington",Vec2Name = "Static Profile")
overlaps4$plot_dat_freq$Comparison <- rep("Lakisha Washington by Static Profile")
overlaps4$plot_dat_prop$Comparison <- rep("Lakisha Washington by Static Profile")


Pdat1 <- rbind(overlaps1$plot_dat_prop,
               overlaps2$plot_dat_prop,
               overlaps3$plot_dat_prop,
               overlaps4$plot_dat_prop)

Pdat1$Variable <- str_remove_all(string = Pdat1$Variable,pattern = "Prop")

# FREQ
Pdat2 <- rbind(overlaps1$plot_dat_freq,
               overlaps2$plot_dat_freq,
               overlaps3$plot_dat_freq,
               overlaps4$plot_dat_freq)

Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "Unique")

Pdat2$Variable <- ifelse(Pdat2$Variable == "_Overlap","Jaccard",Pdat2$Variable)

tab1 <- Pdat1 %>% spread(key = AI,value = Value)
tab1 <- tab1[order(tab1$Comparison),]

tab2 <- Pdat2 %>% spread(key = AI,value = Value)
tab2 <- tab2[order(tab2$Comparison),-1]


tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")

tab1

tab <- kable(tab1,format = "latex",booktabs = T,digits = 2)
tab <- collapse_rows(tab,latex_hline = "major",columns = 2 )

writeLines(tab,"../tab/study4_No_Name_tab.tex")




# LEGACY
# # Study 3 + 4 MANOVA
# # # # # # # # # #
# library(car)
# 
# moddat <- dat3[dat3$gender == "Female" & dat3$ai == "Gemini",]
# 
# 
# ManDat <- data.frame("ai" = moddat$ai,
#                      "race" = moddat$race)
# 
# DV <-  cbind("R" = moddat$R,
#              "I" = moddat$I,
#              "A" = moddat$A,
#              "S" = moddat$S,
#              "E" = moddat$E,
#              "C" = moddat$C)
# 
# 
# mod <- lm(DV ~ race, data = ManDat)
# 
# out <- Manova(mod, data = ManDat,)
# 
# summary.aov(mod)
# 
# 
# summary(out)
# 
# plot(hist(DV[,"C"]))



# Honing in on gender and race
# Gender
# Study4b
# # # # # # # # # # # # # # # 

# Study 4 B: Name discrimination Gender only
# # # # # # # # # # # # # # # # #
# Must link names to stereotypes: 
# "Emily Walsh" = Female (Fdat)
# "Greg Baker" = Male (Mdat)
# "Lakisha Washington" =  Female (Fdat)
# "Jamal Jones" = Male (Mdat)

# dat_long <- dat4 %>%
#   gather(key = "variable", value = "value", R, I, A, S, E, C) %>%
#   select(ai,name,variable,value) %>% 
#   group_by(ai,name,variable) %>%
#   summarise(mean = mean(value,na.rm = TRUE),SD = sd(value,na.rm = TRUE),N = sum(!is.nan(value)))
# 
# EWdat <- dat_long %>%
#   filter(name == "Emily Walsh")
# 
# JJdat <- dat_long %>%
#   filter(name == "Jamal Jones")
# 
# GBdat <- dat_long %>%
#   filter(name == "Greg Baker")
# 
# LWdat <- dat_long %>%
#   filter(name == "Lakisha Washington")
# 
# dat_long <- dat1 %>%
#   gather(key = "variable", value = "value", R, I, A, S, E, C) %>%
#   select(ai,gender,variable,value) %>% 
#   group_by(ai,gender,variable) %>%
#   summarise(mean = mean(value,na.rm = TRUE),SD = sd(value,na.rm = TRUE),N = sum(!is.nan(value)))
# 
# Fdat <- dat_long %>%
#   filter(gender == "Female")
# 
# Mdat <- dat_long %>%
#   filter(gender == "Male")
# 
# 
# # Emily Walsh
# # # # # # #
# 
# pdat <- EWdat %>% left_join(Fdat,by = c("ai","variable"))
# 
# # Adding 95% and 99% CI to pdat
# pdat <- pdat %>%
#   rowwise() %>%
#   mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
#          CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
#   ungroup()
# 
# # Splitting CI columns into lower and upper bounds
# pdat <- pdat %>%
#   mutate(CI_95_lower = map_dbl(CI_95, "lower"),
#          CI_95_upper = map_dbl(CI_95, "upper"),
#          CI_99_lower = map_dbl(CI_99, "lower"),
#          CI_99_upper = map_dbl(CI_99, "upper")) %>%
#   select(-CI_95, -CI_99)
# 
# # Difference, emily - white women
# pdat$Dif <- pdat$mean.x - pdat$mean.y
# 
# # Renames for plot
# pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
# pdat$AI <- pdat$ai
# pdat$`RIASEC Code` <- pdat$variable
# pdat$`Mean Difference` <- pdat$Dif
# 
# P1 <- pdat %>%
#   ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
#   geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
#   ylim(-2,2) +
#   geom_hline(yintercept = 0,lty = 2) +
#   geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "gray40") +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "black") +
#   coord_flip() +
#   annotate(geom = "text",x = 6.5,y = -1,label = "Female Higher") +
#   annotate(geom = "text",x = 6.5,y = 1,label = "Emily Walsh Higher") +
#   theme_minimal() + scale_fill_manual(values = palette) +
#   scale_color_manual(values = palette)
# 
# 
# #  Male  = (Mdat)
# # "Greg Baker" = (GBdat)
# # # # # # # # # # # # # # # # #
# 
# 
# pdat <- GBdat %>% left_join(Mdat,by = c("ai","variable"))
# 
# # Adding 95% and 99% CI to pdat
# pdat <- pdat %>%
#   rowwise() %>%
#   mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
#          CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
#   ungroup()
# 
# # Splitting CI columns into lower and upper bounds
# pdat <- pdat %>%
#   mutate(CI_95_lower = map_dbl(CI_95, "lower"),
#          CI_95_upper = map_dbl(CI_95, "upper"),
#          CI_99_lower = map_dbl(CI_99, "lower"),
#          CI_99_upper = map_dbl(CI_99, "upper")) %>%
#   select(-CI_95, -CI_99)
# 
# # Difference, greg - white men
# pdat$Dif <- pdat$mean.x - pdat$mean.y
# 
# # Renames for plot
# pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
# pdat$AI <- pdat$ai
# pdat$`RIASEC Code` <- pdat$variable
# pdat$`Mean Difference` <- pdat$Dif
# 
# P2 <- pdat %>%
#   ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
#   geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
#   ylim(-2,2) +
#   geom_hline(yintercept = 0,lty = 2) +
#   geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "gray40") +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "black") +
#   coord_flip() +
#   annotate(geom = "text",x = 6.5,y = -1,label = "Male Higher") +
#   annotate(geom = "text",x = 6.5,y = 1,label = "Greg Baker Higher") +
#   theme_minimal() + scale_fill_manual(values = palette) +
#   scale_color_manual(values = palette)
# 
# #  Male  = (Mdat)
# # "Jamal Jones" = Black Male (JJdat)
# # # # # # # # # # # # # # # # #
# 
# 
# 
# pdat <- JJdat %>% left_join(Mdat,by = c("ai","variable"))
# 
# # Adding 95% and 99% CI to pdat
# pdat <- pdat %>%
#   rowwise() %>%
#   mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
#          CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
#   ungroup()
# 
# # Splitting CI columns into lower and upper bounds
# pdat <- pdat %>%
#   mutate(CI_95_lower = map_dbl(CI_95, "lower"),
#          CI_95_upper = map_dbl(CI_95, "upper"),
#          CI_99_lower = map_dbl(CI_99, "lower"),
#          CI_99_upper = map_dbl(CI_99, "upper")) %>%
#   select(-CI_95, -CI_99)
# 
# # Difference, Jamal - black men
# pdat$Dif <- pdat$mean.x - pdat$mean.y
# 
# # Renames for plot
# pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
# pdat$AI <- pdat$ai
# pdat$`RIASEC Code` <- pdat$variable
# pdat$`Mean Difference` <- pdat$Dif
# 
# 
# P3 <- pdat %>%
#   ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
#   geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
#   ylim(-2,2) +
#   geom_hline(yintercept = 0,lty = 2) +
#   geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "gray40") +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "black") +
#   coord_flip() +
#   annotate(geom = "text",x = 6.5,y = -1,label = "Male Higher") +
#   annotate(geom = "text",x = 6.5,y = 1,label = "Jamal Jones Higher") +
#   theme_minimal() + scale_fill_manual(values = palette) +
#   scale_color_manual(values = palette)
# 
# # Female Fdat
# # "Lakisha Washington" = Black Female (LWdat)
# # # # # # # # # # # # # # # # #
# 
# pdat <- LWdat %>% left_join(Fdat,by = c("ai","variable"))
# 
# # Adding 95% and 99% CI to pdat
# pdat <- pdat %>%
#   rowwise() %>%
#   mutate(CI_95 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.95)),
#          CI_99 = list(calculate_ci(mean.x, SD.x, N.x, mean.y, SD.y, N.y, 0.99))) %>%
#   ungroup()
# 
# # Splitting CI columns into lower and upper bounds
# pdat <- pdat %>%
#   mutate(CI_95_lower = map_dbl(CI_95, "lower"),
#          CI_95_upper = map_dbl(CI_95, "upper"),
#          CI_99_lower = map_dbl(CI_99, "lower"),
#          CI_99_upper = map_dbl(CI_99, "upper")) %>%
#   select(-CI_95, -CI_99)
# 
# # Difference, Lakisha - black women
# pdat$Dif <- pdat$mean.x - pdat$mean.y
# 
# # Renames for plot
# pdat$variable <- factor(pdat$variable,levels = c("C","E","S","A","I","R"))
# pdat$AI <- pdat$ai
# pdat$`RIASEC Code` <- pdat$variable
# pdat$`Mean Difference` <- pdat$Dif
# 
# 
# P4 <- pdat %>%
#   ggplot(aes(y = `Mean Difference`,x = `RIASEC Code`, group = AI, color = AI, fill = AI)) +
#   geom_bar(stat = "identity",alpha = 0.4, position = position_dodge()) +
#   ylim(-2,2) +
#   geom_hline(yintercept = 0,lty = 2) +
#   geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5), color = "gray45",lty = 3) +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_95_lower, ymax = CI_95_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "gray40") +
#   # 95% CI Error Bars
#   geom_errorbar(aes(ymin = CI_99_lower, ymax = CI_99_upper,y = `Mean Difference`),
#                 width = 0.35, position = position_dodge(0.9), color = "black") +
#   coord_flip() +
#   annotate(geom = "text",x = 6.5,y = -1,label = "Female Higher") +
#   annotate(geom = "text",x = 6.5,y = 1,label = "Lakisha Washington Higher") +
#   theme_minimal() + scale_fill_manual(values = palette) +
#   scale_color_manual(values = palette)
# 
# ggarrange(P2,P3,P1,P4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
# 
# ggsave("../fig/Study4B.pdf",width = 14,height = 14)

# # Study 4B overlap: Raceless gender
# # # # # # # # # # #
# # EWdat EWFactor
# # GBdat GBFactor
# # JJdat JJFactor
# # LWdat LWFactor
# #
# # Fdat Ffactor
# # Mdat Mfactor
# 
# 
# 
# 
# # Names
# EWdat <- unlist(c(dat4[dat4$name == "Emily Walsh", paste0("onet_soc_title_",1:10)]))
# EWFactor <- rep(dat4[dat4$name == "Emily Walsh","ai"],10)
# 
# GBdat <- unlist(c(dat4[dat4$name == "Greg Baker", paste0("onet_soc_title_",1:10)]))
# GBFactor <- rep(dat4[dat4$name == "Greg Baker","ai"],10)
# 
# LWdat <- unlist(c(dat4[dat4$name == "Lakisha Washington", paste0("onet_soc_title_",1:10)]))
# LWFactor <- rep(dat4[dat4$name == "Lakisha Washington","ai"],10)
# 
# JJdat <- unlist(c(dat4[dat4$name == "Jamal Jones", paste0("onet_soc_title_",1:10)]))
# JJFactor <- rep(dat4[dat4$name == "Jamal Jones","ai"],10)
# 
# 
# # Gender
# Fdat <- unlist(c(dat1[dat1$gender == "Female",paste0("onet_soc_title_",1:10)]))
# Ffactor <- rep(dat1[dat1$gender == "Female","ai"],10)
# 
# Mdat <- unlist(c(dat1[dat1$gender == "Male",paste0("onet_soc_title_",1:10)]))
# Mfactor <- rep(dat1[dat1$gender == "Male","ai"],10)
# 
# 
# # EW Vs White Female
# overlaps1 <- calculate_overlap_metrics(Fdat, EWdat,Ffactor,EWFactor,Vec1Name = "Female",Vec2Name = "Emily Walsh")
# overlaps1$plot_dat_prop$Comparison <- rep("Female by Emily Walsh")
# overlaps1$plot_dat_freq$Comparison <- rep("Female by Emily Walsh")
# 
# # GB Vs white Male
# overlaps2 <- calculate_overlap_metrics(GBdat, dat,GBFactor,WMfactor,Vec1Name = "Greg Baker",Vec2Name = "White Male")
# overlaps2$plot_dat_prop$Comparison <- rep("Greg Baker by White Male")
# overlaps2$plot_dat_freq$Comparison <- rep("Greg Baker by White Male")
# 
# # JJ Vs Black Male
# overlaps3 <-calculate_overlap_metrics(BMdat, JJdat,BMfactor,JJFactor,Vec1Name = "Black Male",Vec2Name = "Jamal Jones")
# overlaps3$plot_dat_prop$Comparison <- rep("Jamal Jones by Black Male")
# overlaps3$plot_dat_freq$Comparison <- rep("Jamal Jones by Black Male")
# 
# # LW Vs Black Female
# overlaps4 <- calculate_overlap_metrics(LWdat, BFdat,LWFactor,BFfactor,Vec1Name = "Lakisha Washington",Vec2Name = "Black Female")
# overlaps4$plot_dat_freq$Comparison <- rep("Lakisha Washington by Black Female")
# overlaps4$plot_dat_prop$Comparison <- rep("Lakisha Washington by Black Female")
# 
# 
# Pdat1 <- rbind(overlaps1$plot_dat_prop,
#                overlaps2$plot_dat_prop,
#                overlaps3$plot_dat_prop,
#                overlaps4$plot_dat_prop)
# 
# Pdat1$Variable <- str_remove_all(string = Pdat1$Variable,pattern = "Prop")
# 
# # Hand removing the Emily Walsh Gemini Column and Lakisha Washington
# 
# 
# Pdat1[Pdat1$AI == "Gemini" & Pdat1$Comparison == "Emily Walsh by White Female","Value"] <- 0
# Pdat1[Pdat1$AI == "Gemini" & Pdat1$Comparison == "Lakisha Washington by Black Female","Value"] <- 0
# 
# P1 <- ggplot(Pdat1, aes(x = AI, y = Value, fill = Variable)) +
#   geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
#   labs(x = "Conversational AI System",title = "B.) Jaccard Similarity and Uniqueness Proportions",
#        y = "Proportion of Unique Recommendations",
#        fill = "Comparisons") +
#   theme_minimal() + facet_wrap(.~Comparison) + coord_flip()
# 
# 
# # FREQ
# Pdat2 <- rbind(overlaps1$plot_dat_freq,
#                overlaps2$plot_dat_freq,
#                overlaps3$plot_dat_freq,
#                overlaps4$plot_dat_freq)
# 
# Pdat2$Variable <- str_remove_all(string = Pdat2$Variable,pattern = "Unique")
# 
# P2 <- ggplot(Pdat2, aes(x = AI, y = Value, fill = Variable)) +
#   geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
#   labs(x = "Conversational AI System",title = "       C.) Job Frequency Overlap and Uniqueness",
#        y = "Count",
#        fill = "Comparisons") +
#   theme_minimal() +
#   facet_wrap(.~Comparison) +
#   theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
# 
# 
# PP <- grid.arrange(P1,P2)
# 
# 
# ggsave(plot = PP,"../fig/overlap/Study4_overlap.pdf",width = 7,height = 10, units = "in")
# 
# 
# tab1 <- Pdat1 %>% spread(key = AI,value = Value)
# tab1 <- tab1[order(tab1$Comparison),]
# 
# tab2 <- Pdat2 %>% spread(key = AI,value = Value)
# tab2 <- tab2[order(tab2$Comparison),-1]
# 
# 
# tab1$Gemini <- paste0(round(tab1$Gemini,2)," (",tab2$Gemini,")")
# tab1$`GPT-3.5` <- paste0(round(tab1$`GPT-3.5`,2)," (",tab2$`GPT-3.5`,")")
# tab1$`GPT-4` <- paste0(round(tab1$`GPT-4`,2)," (",tab2$`GPT-4`,")")
# tab1$`Llama 2` <- paste0(round(tab1$`Llama 2`,2)," (",tab2$`Llama 2`,")")
# 
# 
# 
# tab <- kable(tab1,format = "latex",booktabs = T,digits = 2)
# tab <- collapse_rows(tab,latex_hline = "major",columns = 2 )
# 
# writeLines(tab,"../tab/study4_tab.tex")


