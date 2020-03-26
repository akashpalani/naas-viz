
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(choroplethrMaps)
library(stringr)

# Geographic Distribution -----------------------------------------------------------
#Rename data. Copy made to preserve original data in case something goes wrong. 
owo <- readRDS("data.rda")
owo
all_data_raw <- da37024.0001

#Respondents per state 
respondents_per_state <- all_data_raw %>%
  group_by(RSTATE) %>%
  count()

#Find missing states
states <- tibble(state.abb)
anti_join(states, respondents_per_state, by = c("state.abb" = "RSTATE"))
#South Dakota and Vermont did not have any respondents in the survey. 


# Demographics ------------------------------------------------------------

#Investigate RETHNIC
all_data_raw %>%
  group_by(RACEETH) %>%
  count() %>% View()
#limit scope to Asian American and Pacific Islander ethnicities. 

#Drop White, Black, and Latino from ethnicities to prepare data for visualization. 
ethnicity_data <- all_data_raw %>%
  #disinclude non-Asian races and ethnicites
  filter(!RACEETH %in% c("(03) White", "(04) Black", "(06) Latino")) %>%
  mutate(ethnicity = as.factor(str_sub(RACEETH, 6))) %>%
  #rename ethnicities for higher quality plots
  mutate(ethnicity = fct_recode(ethnicity, 
                                "Vietnamese" = "vietnamese", 
                                "Korean" = "korean", 
                                "NHPI" = "NHPI",
                                "Japaense" = "japanese", 
                                "Hmong" = "hmong", 
                                "Filipino" = "filipino", 
                                "Chinese" = "chinese", 
                                "Cambodian" = "cambodian", 
                                "Asian Indian" = "asnindian")) %>%
  #Black, White, and Latino have become NA values. Drop them. 
  drop_na(ethnicity)

#Bar graph of ethnicites
ggplot(data = ethnicity_data) + geom_bar(aes(x = ethnicity, fill = ethnicity), na.rm = FALSE) + coord_flip()

#Data are not proportional to national data, which makes sense because that's not the point of the survey!

#Function to speed up creation of stacked bar charts in this section
ethnicity_stacked_bar <- function(x, fill, x_lab, y_lab, fill_lab) {
  ggplot(data = ethnicity_data) + 
    geom_bar(aes(x = x, fill = fill), position = "stack") +
    coord_flip() + 
    xlab(x_lab) + 
    ylab(y_lab) +
    labs(fill = fill_lab)
}


#Education by Ethnicity
#Clean up education factor
ethnicity_data <- ethnicity_data %>%
  #I will be repeating this substring operation on almost every plot. The data have a "(##)" prefix with each entry, a numerical indicator of the response. I don't need this in my plots, so I do this substring operation to remove it. 
  mutate(EDUC3 = as.factor(str_sub(EDUC3, 5)))

ethnicity_stacked_bar(ethnicity_data$ethnicity, ethnicity_data$EDUC3, "Ethnicity", "Count", "Highest Education Obtained")

#Birthplace by ethnicity
ethnicity_data <- ethnicity_data %>%
  mutate(FORBORN = as.factor(str_sub(FORBORN, 5)))

ethnicity_stacked_bar(ethnicity_data$ethnicity, ethnicity_data$FORBORN, "Ethnicity", "Count", "Birthplace")  

#Gender by ethnicity
ethnicity_data <- ethnicity_data %>%
  mutate(S7 = as.factor(str_sub(S7, 5)))

ethnicity_stacked_bar(ethnicity_data$ethnicity, ethnicity_data$S7, "Ethnicity", "Count", "Gender")


# Issues ------------------------------------------------------------------

#Asian Americans on the Issues
#Most important issues personally-61d

#missing value analysis
ethnicity_data %>%
  count(Q6_1D = factor(Q6_1D)) %>%
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Q6_1D, y = pct, label = scales::percent(pct))) + geom_col() + coord_flip() + scale_y_continuous(labels = scales::percent)
#About a fifth of our data is "NA". We can also group the "Other" categories together, and modify our labels to make this more accurate. According to the codebook, "NA" means the respondent didn't know, or refused to answer. 

#Clean and replot:
ethnicity_data %>%
  mutate(Q6_1D = as.character(Q6_1D)) %>%
  #Group other categories, the taxes thing is scratch work and can be ignored
  mutate(Q6_1D = fct_recode(Q6_1D, 
                            "Taxes" = "(12) ASLDF", 
                            "(88) Other" = "(16) OTHQ6_1C:", 
                            "(88) Other" = "(15) OTHQ6_1B:", 
                            "(88) Other" = "(14) OTHQ6_1A:", 
                            "(88) Other" = "(17) Other  SPECIFY")) %>%
  #convert back to factor
  mutate(Q6_1D = as.factor(str_sub(Q6_1D, 5))) %>%
  count(Q6_1D = factor(Q6_1D)) %>%
  #calculate percentages for each response
  mutate(pct = prop.table(n)) %>% 
  #drop don't know/refused answers. don't really affect this, and there are not that many
  drop_na(Q6_1D) %>%
  ggplot(aes(x = Q6_1D, y = pct, label = scales::percent(pct))) +
  geom_col(fill = "blue") + 
  xlab("Issues") +
  ylab("Percentage") +
  coord_flip() + 
  #rescale y axis for percentages
  scale_y_continuous(labels = scales::percent)

#Most Important by Ethnicity
ethnicity_data <- ethnicity_data %>%
  mutate(Q6_1D = as.character(Q6_1D)) %>%
  mutate(Q6_1D = fct_recode(Q6_1D, 
                            "Taxes" = "(12) ASLDF", 
                            "(88) Other" = "(16) OTHQ6_1C:", 
                            "(88) Other" = "(15) OTHQ6_1B:", 
                            "(88) Other" = "(14) OTHQ6_1A:", 
                            "(88) Other" = "(17) Other  SPECIFY")) %>%
  drop_na(Q6_1D)

#plot
ggplot(ethnicity_data, aes(x = ethnicity, fill = Q6_1D)) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  labs(fill = "Issue") +
  scale_y_continuous(name = "Percentage", labels = scales::percent)

#There's a lot going on here. Let's break it down by some of the major issues to see how ethnicities feel. I'll include one for all asian americans to compare how groups differ from the whole. 

#Healthcare: Obamacare
ethnicity_data %>% 
  mutate(Q6_5_1 = as.character(Q6_5_1)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_1 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_1 = as.factor(str_sub(Q6_5_1, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_1)) + 
  geom_bar(position = "fill") +
  ggtitle("Healthcare: Obamacare") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Healthcare followup: How would you rate your overall health in the past year?
ethnicity_data %>% 
  mutate(Q8_8 = as.character(Q8_8)) %>%
  #According to the codebook, random sample of half of respondents were surveyed, so drop missing values and understand that this is only half of the respondents. 
  drop_na(Q8_8) %>%
  mutate(Q8_8 = as.factor(str_sub(Q8_8, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q8_8)) + 
  geom_bar(position = "fill", alpha = 0.9) +
  ggtitle("Healthcare: Rate your overall health in the past year") +
  labs(fill = "Response") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Education: Federal Student Loan Relief 
ethnicity_data %>% 
  mutate(Q6_5_2 = as.character(Q6_5_2)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_2 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_2 = as.factor(str_sub(Q6_5_2, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_2)) + 
  geom_bar(position = "fill") +
  ggtitle("Education: Federal Student Loan Relief") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Immigration: Syrian Refugees Entering US
ethnicity_data %>% 
  mutate(Q6_5_3 = as.character(Q6_5_3)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_3 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_3 = as.factor(str_sub(Q6_5_3, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_3)) + 
  geom_bar(position = "fill") +
  ggtitle("Immigration: Syrian Refugees Entering US") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Criminal Justice: Marijuana Posession Demcriminalization (Small Amounts)
ethnicity_data %>% 
  mutate(Q6_5_4 = as.character(Q6_5_4)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_4 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_4 = as.factor(str_sub(Q6_5_4, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_4)) + 
  geom_bar(position = "fill") +
  ggtitle("Criminal Justice: Marijuana Posession Demcriminalization (Small Amounts)") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Immigration: Trump Muslim Ban
ethnicity_data %>% 
  mutate(Q6_5_5 = as.character(Q6_5_5)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_5 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_5 = as.factor(str_sub(Q6_5_5, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_5)) + 
  geom_bar(position = "fill") +
  ggtitle("Immigration: Trump Muslim Ban") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Environment: Stricter Emissions Limits on Power Plants
ethnicity_data %>% 
  mutate(Q6_5_6 = as.character(Q6_5_6)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_6 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_6 = as.factor(str_sub(Q6_5_6, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_6)) + 
  geom_bar(position = "fill") +
  ggtitle("Environment: Stricter Emissions Limits on Power Plants") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Race: Government Promotion of Black-White Equality
ethnicity_data %>% 
  mutate(Q6_5_7 = as.character(Q6_5_7)) %>%
  #According to the codebook, NA means don't know/refused
  replace_na(list(Q6_5_7 = "(3) Don't Know/Refused")) %>%
  mutate(Q6_5_7 = as.factor(str_sub(Q6_5_7, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q6_5_7)) + 
  geom_bar(position = "fill") +
  ggtitle("Race: Government Promotion of Black-White Equality") +
  labs(fill = "Position") + 
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()


# Political and Civic Engagement ------------------------------------------

#Political Party Contact by Ethnicity
ethnicity_data %>%
  mutate(Q5_5 = as.character(Q5_5)) %>%
  #missing values are don't know refused, and there are relatively few. dropped. 
  drop_na(Q5_5) %>%
  ggplot(aes(x = ethnicity, fill = Q5_5)) + 
  geom_bar(position = "fill") + 
  ggtitle("Have you been contacted by a major political party?") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  labs(fill = "Response") +
  coord_flip()

#Contact by Party
ethnicity_data %>%
  mutate(Q5_5A = as.character(Q5_5A)) %>%
  mutate(Q5_5A = as.factor(str_sub(Q5_5A, 5))) %>%
  #missing values are people that have not been contacted 
  drop_na(Q5_5A) %>%
  ggplot(aes(x = Q5_5A, fill = Q5_5A)) + 
  geom_bar() + 
  xlab("Party") +
  labs(fill = "Part") +
  theme(legend.position = "none") +
  ggtitle("Contact by Party") +
  coord_flip()

#Disengagement
#Politics/Government too Complicated
#11% of missing values from don't know/refused. 28% total missing values--a lot

ethnicity_data %>%
  ggplot(aes(ethnicity, fill = Q6_6A)) + geom_bar()
#This is left in because to show that there is a large number of missing values, so take the next with a grain of salt. 

ethnicity_data %>%
  mutate(Q6_6A = as.character(Q6_6A)) %>%
  mutate(Q6_6A = as.factor(str_sub(Q6_6A, 5))) %>%
  drop_na(Q6_6A) %>%
  ggplot(aes(x = ethnicity, fill = Q6_6A)) + 
    geom_bar(position = "fill") + 
    labs(fill = "Response") +
    ggtitle("Too Complicated by Ethnicity") +
    scale_y_continuous(name = "Percentage", labels = scales::percent) + 
    coord_flip()

#Politicians Don't Care About Someone Like Me
ethnicity_data %>%
  ggplot(aes(ethnicity, fill = Q6_6A)) + geom_bar()
#This is left in because to show that there is a large number of missing values, so take the next with a grain of salt. 

ethnicity_data %>%
  mutate(Q6_6B = as.character(Q6_6B)) %>%
  mutate(Q6_6B = as.factor(str_sub(Q6_6B, 5))) %>%
  drop_na(Q6_6B) %>%
  ggplot(aes(x = ethnicity, fill = Q6_6B)) + 
  geom_bar(position = "fill") + 
  labs(fill = "Response") +
  ggtitle("Politicians Don't Care About Someone Like Me by Ethnicity") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Voting Behavior
#Data based on REGISTERED VOTERS

#Voting in General Election by Ethnicity
ethnicity_data %>%
  mutate(Q4_2 = as.character(Q4_2)) %>%
  #replace missing values with don't know/refused (codebook says that accounts for NA values)
  replace_na(list(Q4_2 = "(88)Don't Know/Refused")) %>%
  mutate(Q4_2 = as.factor(str_sub(Q4_2, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q4_2)) + 
  geom_bar(position = "fill") + 
  labs(fill = "Response") +
  ggtitle("Planning to Vote in General Election by Ethnicity") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Voting in Primary/Caucus by Ethnicity
ethnicity_data %>%
  mutate(Q4_6 = as.character(Q4_6)) %>%
  #codebook
  replace_na(list(Q4_6 = "(88)Don't Know/Refused")) %>%
  mutate(Q4_6 = as.factor(str_sub(Q4_6, 5))) %>%
  ggplot(aes(x = ethnicity, fill = Q4_6)) + 
  geom_bar(position = "fill", alpha = 0.8) + 
  labs(fill = "Response") +
  ggtitle("Planning to Vote in Primary/Caucus by Ethnicity") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#engagement
#Q5_1_01 through 9
ethnicity_data %>%
  #the response to each category was in a different column, so first select the relevant columns
  select(c(ethnicity, Q5_1_01, Q5_1_02, Q5_1_03, Q5_1_04,Q5_1_05,Q5_1_06, Q5_1_08, Q5_1_09)) %>% 
  #gather responses into one column so that they can be plotted
  gather("Question", ethnicity) %>%
  #codebook
  replace_na(list(ethnicity = "(3) Don't Know/Refused")) %>%
  #standard cleaning, described at beginning
  mutate(ethnicity = as.factor(str_sub(ethnicity, 5))) %>%
  #convert question to factor, levels imputed from data work well
  mutate(Question = as.factor(Question)) %>%
  #appropriately label each level so that they can be interpreted without the codebook
  mutate(Question = fct_recode(Question, 
                               "Discussed politics w/ \nfamily & friends" = "Q5_1_01",  
                               "Contributed money to a candidate, political party, \nor some other campaign organization" = "Q5_1_02", 
                               "Contacted your representative \nor a government official" = "Q5_1_03", 
                               "Worked with others in your community \nto solve a problem" = "Q5_1_04", 
                               "Attended a protest march, \ndemonstration, or rally" = "Q5_1_05", 
                               "Signed a petition" = "Q5_1_06", 
                               "Attended a public meeting, \nsuch as for school board \nor city council" = "Q5_1_08", 
                               "Donated money to a religious \nor charitable cause " = "Q5_1_09")) %>%
  ggplot(mapping = aes(Question, fill = ethnicity)) + 
    geom_bar() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #i picked gray for NA, green for yes, and red for no to make legibility higher
    scale_fill_manual(values=c("#808080", "#ff0000", "#00ff00")) +
    labs(fill = "Response") +
    ggtitle("Engagement") 


# Party Affiliation -------------------------------------------------------
#Heat map expressing covariation between personal party identification and ethnicity
ethnicity_data %>%
  count(ethnicity, Q7_4A) %>%  
  drop_na(Q7_4A) %>%
  mutate(Q7_4A = fct_recode(Q7_4A, 
                            "(2) Closer to neither party" = "(3) Closer to neither party", 
                            "(3) Closer to the Democratic party" = "(2) Closer to the Democratic party")) %>%
  ggplot(mapping = aes(y = ethnicity, x = Q7_4A)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") + 
  ggtitle("Covariation: Party Affiliation and Ethnicity")

#Party Affiliation by Ethnicity
ethnicity_data %>% 
  mutate(Q7_1 = as.character(Q7_1)) %>%
  #codebook
  replace_na(list(Q7_1 = "(88) Don't Know/Refused")) %>%
  mutate(Q7_1 = as.factor(Q7_1)) %>%
  mutate(Q7_1 = fct_recode(Q7_1, 
                           "(05) Don't think in terms of parties" = "(05) DO NOT READ  Do not think in terms of political parties", 
                           "(04) Other party" = "(04) Other party  SPECIFY")) %>%
  mutate(Q7_1 = as.character(Q7_1)) %>%
  mutate(Q7_1 = str_sub(Q7_1, 5)) %>%
  ggplot(aes(x = ethnicity, fill = Q7_1)) + 
  geom_bar(position = "fill", alpha = 0.8) + 
  labs(fill = "Response") +
  ggtitle("Party Affiliation") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Party affiliation by ethnicity and Gender
ethnicity_data %>% 
  mutate(Q7_1 = as.character(Q7_1)) %>%
  #codebook
  replace_na(list(Q7_1 = "(88) Don't Know/Refused")) %>%
  mutate(Q7_1 = as.factor(Q7_1)) %>%
  mutate(Q7_1 = fct_recode(Q7_1, 
                           "(05) Don't think in terms of parties" = "(05) DO NOT READ  Do not think in terms of political parties", 
                           "(04) Other party" = "(04) Other party  SPECIFY")) %>%
  mutate(Q7_1 = as.character(Q7_1)) %>%
  mutate(Q7_1 = str_sub(Q7_1, 5)) %>%
  ggplot(aes(x = S7, fill = Q7_1)) + 
  geom_bar(position = "fill", alpha = 0.8) + 
  #facet wrap by ethnicity so we can look at gender varation within each group
  facet_wrap(~ethnicity) +
  labs(fill = "Response") +
  ggtitle("Party Affiliation by Ethnicity and Gender") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Party affiliation by ethnicity and birthplace
ethnicity_data %>% 
  mutate(Q7_1 = as.character(Q7_1)) %>%
  #codebook
  replace_na(list(Q7_1 = "(88) Don't Know/Refused")) %>%
  mutate(Q7_1 = as.factor(Q7_1)) %>%
  mutate(Q7_1 = fct_recode(Q7_1, 
                           "(05) Don't think in terms of parties" = "(05) DO NOT READ  Do not think in terms of political parties", 
                           "(04) Other party" = "(04) Other party  SPECIFY")) %>%
  mutate(Q7_1 = as.character(Q7_1)) %>%
  mutate(Q7_1 = str_sub(Q7_1, 5)) %>%
  ggplot(aes(x = FORBORN, fill = Q7_1)) + 
  geom_bar(position = "fill", alpha = 0.8) + 
  #facet by ethnicity so we can look at birthplace variation within each group
  facet_wrap(~ethnicity) +
  labs(fill = "Response") +
  ggtitle("Party Affiliation by Ethnicity and Birthplace") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()

#Party Affiliation by level of education 
ethnicity_data %>% 
  mutate(Q7_1 = as.character(Q7_1)) %>%
  #codebook
  replace_na(list(Q7_1 = "(88) Don't Know/Refused")) %>%
  mutate(Q7_1 = as.factor(Q7_1)) %>%
  mutate(Q7_1 = fct_recode(Q7_1, 
                           "(05) Don't think in terms of parties" = "(05) DO NOT READ  Do not think in terms of political parties", 
                           "(04) Other party" = "(04) Other party  SPECIFY")) %>%
  mutate(Q7_1 = as.character(Q7_1)) %>%
  mutate(Q7_1 = str_sub(Q7_1, 5)) %>%
  ggplot(aes(x = EDUC3, fill = Q7_1)) + 
  geom_bar(position = "fill", alpha = 0.8) + 
  #facet by ethnicity so we can look at variation between levels of education within each group
  facet_wrap(~ethnicity) +
  labs(fill = "Response") +
  ggtitle("Party Affiliation by Level of Education and Ethnicity") +
  scale_y_continuous(name = "Percentage", labels = scales::percent) + 
  coord_flip()



