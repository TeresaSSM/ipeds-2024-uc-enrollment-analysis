#Script that runs and cleans IPEDS data, handles missing values, and makes the figures for the report
#Secondly, a 2 page report with an intro to the dataset and the questions, a findings section
#and a conclusion w/ recommendations

setwd("C:/Users/teres/Desktop/Data work")
library(pacman)
p_load(tidyverse, janitor,skimr,broom)

ipeds_raw <- read_csv("C:\\Users\\teres\\Desktop\\Data work\\CSV_6132026-557.csv", show_col_types = FALSE)
glimpse(ipeds_raw)
skim(ipeds_raw)

theme_set(theme_classic())

### 1 Data Cleaning ###
ipeds <- ipeds_raw %>%
  clean_names() %>%
  rename(
    inst = institution_name,
    total_enroll = drvef2024_total_enrollment,
    fte_fall = drvef2024_full_time_equivalent_fall_enrollment,
    full_time = drvef2024_full_time_enrollment,
    part_time = drvef2024_part_time_enrollment,
    undergrad = drvef2024_undergraduate_enrollment,
    grad = drvef2024_graduate_enrollment,
    pct_ugr_white = drvef2024_percent_of_undergraduate_enrollment_that_are_white,
    pct_ugr_black = drvef2024_percent_of_undergraduate_enrollment_that_are_black_or_african_american,
    pct_ugr_hispanic = drvef2024_percent_of_undergraduate_enrollment_that_are_hispanic_latino,
    pct_ugr_nonresident = drvef2024_percent_of_undergraduate_enrollment_that_are_u_s_nonresident,
    pct_ugr_multiracial = drvef2024_percent_of_undergraduate_enrollment_that_are_two_or_more_races,
    pct_ugr_women = drvef2024_percent_of_undergraduate_enrollment_that_are_women,
    pct_grad_white = drvef2024_percent_of_graduate_enrollment_that_are_white,
    pct_grad_black = drvef2024_percent_of_graduate_enrollment_that_are_black_or_african_american,
    pct_grad_hispanic = drvef2024_percent_of_graduate_enrollment_that_are_hispanic_latino,
    pct_grad_multiracial = drvef2024_percent_of_graduate_enrollment_that_are_two_or_more_races,
    pct_grad_nonresident = drvef2024_percent_of_graduate_enrollment_that_are_u_s_nonresident,
    pct_grad_women = drvef2024_percent_of_graduate_enrollment_that_are_women
  ) %>%
  mutate(
    pct_ugr_aanhpi = coalesce(
      drvef2024_percent_of_undergraduate_enrollment_that_are_asian_native_hawaiian_pacific_islander,
      drvef2024_percent_of_undergraduate_enrollment_that_are_asian+ 
      drvef2024_percent_of_undergraduate_enrollment_that_are_native_hawaiian_or_other_pacific_islander),
    pct_grad_aanhpi = coalesce(
      drvef2024_percent_of_graduate_enrollment_that_are_asian_native_hawaiian_pacific_islander,
      drvef2024_percent_of_graduate_enrollment_that_are_asian + 
      drvef2024_percent_of_graduate_enrollment_that_are_native_hawaiian_or_other_pacific_islander),
    pct_undergrad = undergrad / total_enroll * 100,
    pct_grad = grad / total_enroll * 100) %>%
  select(!contains("drvef2024"))%>%
  filter(inst != "University of California-San Francisco")

skim(ipeds)
view(ipeds)

#ipeds dataset for enrollment figures
ipeds_enrollment<-ipeds%>%
  pivot_longer(cols= c("grad","undergrad"), names_to = "level", values_to = "enrolled")%>%
  select(inst, level, enrolled)
#ipeds dataset for demographics figures
ipeds_demographics <-ipeds%>%
  pivot_longer(cols= contains("pct"), names_to = "demographic", values_to = "percent")%>%
  select(inst, demographic, percent)%>%
  mutate(demographic = sub("pct_", "", demographic))%>%
  filter(!demographic %in% c("undergrad", "grad"))%>%
  separate(demographic, into = c("level", "group"), sep = "_")%>%
  filter(!group == "women")

### 2 Institutional Profile ###

## Figure 2a: Enrollment total pie chart student mix by level for UCSD (undergrad/grad)

ucsd_enrollment_pie<-ipeds_enrollment %>%
  filter(inst == "University of California-San Diego") %>%
  ggplot(aes(x = "", y = enrolled, fill = level)) +
  geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
  geom_text(aes(label = paste0(ifelse(level == "undergrad", "Undergraduate", "Graduate"),
                             ": ", enrolled)), 
  position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c("undergrad" = "#172C49", "grad" = "#03629C")) +
  labs(title = "Enrollment at UC San Diego") +
  coord_polar("y", start = 0) +
  theme_void()

#2b: Demographics bar plot for UC SD (race/ethnicity)
ucsd_demographics<-ipeds_demographics%>%
  filter(inst == "University of California-San Diego")%>%
  ggplot(aes(x=percent, y=group, fill=level))+
  geom_bar(stat="identity", show.legend= FALSE)+
  scale_y_discrete(limits= rev)+
  scale_fill_manual(values = c("ugr" = "#172C49", "grad" = "#03629C"))+
  labs(x = "Percentage", y = "", title = "Demographics at UC San Diego")+
  geom_text(aes(label = paste0(percent, "%")),
            hjust = -0.2, vjust = 0.5, color = "black") +
  facet_wrap(~ level, labeller = labeller(level = c(
    "ugr" = "Undergraduate Demographics","grad" = "Graduate Demographics")),
    nrow=2 )


### 3 Peer comparison figures ###

#3a:compare the enrollment of focal UC campus to other UC campuses
uc_enrollment_fig<-ipeds_enrollment %>%
  mutate(fill_group = case_when(
      inst == "University of California-San Diego" & level == "undergrad" ~ "ucsd_undergrad",
      inst == "University of California-San Diego" & level == "grad" ~ "ucsd_grad",
      level == "undergrad" ~ "other_undergrad",
      level == "grad" ~ "other_grad")) %>%
  ggplot(aes(x = inst, y = enrolled, fill = fill_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = c("other_undergrad" = "#172C49",
      "other_grad" = "#03629C",
      "ucsd_undergrad" = "#C69214",
      "ucsd_grad" = "#FFCD00"),
      guide = "none") +
  labs( x = "Institution", y = "Number of Enrolled Students", 
    title = "Enrollment Across UC Campuses" )

#3b: Undergrad demographics
uc_undergrad_demographics_fig<-ipeds_demographics %>%
filter(level == "ugr")%>%
  mutate(highlight = ifelse(inst == "University of California-San Diego",
   "ucsd", "other")) %>%
  ggplot(aes(x = percent, y = group, fill = highlight)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values =c(
      "other" = "#172C49",
      #"other.grad" = "#03629C",
      "ucsd" = "#C69214"
      #"ucsd.grad" = "#E5D39A"
    )) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ inst, scales = "free", nrow = 3) +
  labs(x = "Percentage", y = "", 
  title = "Undergraduate Demographics Across UC Campuses (Percentages)")

#3c: Grad demographics fig
uc_graduate_demographics_fig<-ipeds_demographics %>%
filter(level == "grad")%>%
  mutate(highlight = ifelse(inst == "University of California-San Diego",
   "ucsd", "other")) %>%
  ggplot(aes(x = percent, y = group, fill = highlight)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values =c(
      "other" = "#172C49",
      #"other.grad" = "#03629C",
      "ucsd" = "#C69214"
      #"ucsd.grad" = "#E5D39A"
    )) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ inst, scales = "free", nrow = 3) +
  labs(x = "Percentage", y = "", 
  title = "Graduate Demographics Across UC Campuses (Percentages)")


### 4 Benchmarking

#4a: Percent undergrad and grad numbers:
ucsd_percent_table<- ipeds%>% 
 filter(inst == "University of California-San Diego")%>%
  mutate(Percent_Undergrad = undergrad / total_enroll * 100,
         Percent_Grad = grad / total_enroll * 100)  %>%
         select(Percent_Undergrad, Percent_Grad)

#4b: ranked enrollments: table with 2 columns, campus name and total enrollment
ranked_enrollments<-ipeds%>%
  select(inst, total_enroll)%>%
  arrange(desc(total_enroll))%>%
  rename("Institution" = inst, "Total_Enrollment" = total_enroll)

#4c: grad and undergrad enrollment shares
enrollment_shares<-ipeds%>%
  mutate(Percent_Undergrad = undergrad / total_enroll * 100,
         Percent_Grad = grad / total_enroll * 100) %>%
  select(inst, Percent_Grad, Percent_Undergrad)%>%
  arrange(desc(Percent_Grad))

#4d: table showing avg % of each demographic, the UCSD %, and the difference
#4d1: undergrads
undergrad_demographics_table<-ipeds %>%
  select(inst, contains("pct_ugr")) %>%
  select(!pct_ugr_women) %>%
  pivot_longer(cols = -inst,
    names_to = "demographic",
    values_to = "value") %>%
  group_by(demographic) %>%
  summarise(avg = mean(value[inst != "University of California-San Diego"], na.rm = TRUE),
    ucsd = value[inst == "University of California-San Diego"][1],
    diff = ucsd - avg,
    .groups = "drop")%>%
    mutate(demographic = recode(demographic,
    pct_ugr_aanhpi = "AANHPI",
    pct_ugr_white = "White",
    pct_ugr_black = "Black",
    pct_ugr_hispanic = "Hispanic",
    pct_ugr_asian = "Asian",
    pct_ugr_nonresident = "Nonresident",
    pct_ugr_multiracial = "Multiracial"))%>%
  rename(Demographic = demographic,
    UC_Average = avg,
    UCSD = ucsd,
    Difference = diff)

#4d2:grads
grad_demographics_table<-ipeds%>%
  select(inst, contains("pct_grad_"))%>%
  select(!pct_grad_women) %>%
  pivot_longer(cols = -inst,
    names_to = "demographic",
    values_to = "value") %>%
  group_by(demographic) %>%
  summarise(avg = mean(value[inst != "University of California-San Diego"], na.rm = TRUE),
    ucsd = value[inst == "University of California-San Diego"][1],
    diff = ucsd - avg,
    .groups = "drop")%>%
    mutate(demographic = recode(demographic,
    pct_grad_aanhpi = "AANHPI",
    pct_grad_white = "White",
    pct_grad_black = "Black",
    pct_grad_hispanic = "Hispanic",
    pct_grad_asian = "Asian",
    pct_grad_nonresident = "Nonresident",
    pct_grad_multiracial = "Multiracial"))%>%
  rename(Demographic = demographic,
    UC_Average = avg,
    UCSD = ucsd,
    Difference = diff)

### 5: Saving Figures and tables

saveRDS(
  list(enrollment_shares = enrollment_shares, #4c
       ranked_enrollments = ranked_enrollments, #4b
       ucsd_percent_table =ucsd_percent_table, #4a
       ucsd_enrollment_pie = ucsd_enrollment_pie, #2a
       ucsd_demographics = ucsd_demographics, #2b
       uc_enrollment_fig = uc_enrollment_fig, #3a
       uc_undergrad_demographics_fig = uc_undergrad_demographics_fig, #3b
       uc_graduate_demographics_fig = uc_graduate_demographics_fig, #3c
    undergrad_demographics_table = undergrad_demographics_table, # 4d1
       grad_demographics_table = grad_demographics_table), #4d2
  "C:\\Users\\teres\\Desktop\\Data work\\IPEDS report\\report_objects.rds")

