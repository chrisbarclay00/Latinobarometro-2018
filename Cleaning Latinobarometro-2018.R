#Cleaning Latinobarometro - 2018
#Professor Austin Hart, PhD.
#Christopher Barclay

library(haven)
library(knitr)
library(tidyverse)
library(Hmisc)
library(expss)


#Loads Data from https://www.latinobarometro.org/latContents.jsp?Idioma=0&CMSID=Datos&Idioma=0&CMSID=Datos
df <- read_dta("Latinobarometro_2018_Eng_Stata_v20190303.dta")

#Limit the sample to for: Colombia Ecuador El Salvador Guatemala Honduras Nicaragua Paraguay Peru

##Codebook of 2018 data may be seen here https://www.latinobarometro.org/latContents.jsp
##IDENPA Country Numbers: Argentina (32) Bolivia (68) Brasil (76) Chile (152) Colombia (170) Costa Rica (188) Rep. Dominicana (214) Ecuador (218) El Salvador (222) Guatemala (320) Honduras (340) México (484) Nicaragua (558) Panamá (591) Paraguay (600) Perú (604) España (724) Uruguay (858) Venezuela (862)

df1 <- df |>
  filter(
    IDENPA == 170 | #Colombia
      IDENPA == 218 | #Ecuador
      IDENPA == 222 | #El Salvador
      IDENPA == 320 | #Guatemala
      IDENPA == 340 | #Honduras
      IDENPA == 558 | #Nicaragua
      IDENPA == 600 | #Paraguay
      IDENPA == 604 #Perú
  )


#A quick table to assess how many responses from each country we got
tab =
  df1 %>%
  count(IDENPA)
tab


#Deliver the following variables, in this order: 
## c_name: country name 
## source: “Latinobarometro” 
## r_id: respondent id 
## r_year: year of interview 
## r_month: month of interview 
## r_swt: survey weight 
## r_female: binary indicator for female
## p_ideology: left-right ideological self-placement **will be created after df2
## p_interest: interest in politics
## p_economy: evaluation of national economy, past year 
## 4 measures of satisfaction with and/or preference for democracy

df2 <- df1 |>
  select(
    c_name = IDENPA, #IDENPA = Country
    r_id = NUMENTRE, #NUMENTRE = Interview number
    r_year = NUMINVES, #NUMINVES = Year of survey
    r_month = MESREAL, #MESREAL = Month of interview
    r_swt = WT, #WT = Weight,
    p_ideology = P22ST, #P22ST = Self-positioning in Left-Right scale (0 = Left, 1 = 1)
    p_politics_informed_by_family = P19ST.A,
    p_politics_informed_by_friends = P19ST.B,
    p_politics_informed_by_coworkers = P19ST.C,
    p_politics_informed_by_classmates = P19ST.D,
    p_politics_informed_by_radio = P19ST.E,
    p_politics_informed_by_newspapers_magazines = P19ST.F,
    p_politics_informed_by_electronic_media_internet = P19ST.G,
    p_politics_informed_by_television = P19ST.H,
    p_politics_informed_by_facebook = P19NC.I,
    p_politics_informed_by_twitter = P19NC.J,
    p_politics_informed_by_youtube = P19F.K,
    p_politics_informed_by_other = P19ST.L,
    p_politics_informed_by_none = P19ST.M, #P19. A-M values are Binary values that measure "How do you stay informed of politics (family, friends, etc.), (0 = Not mentioned, 1 = Mentioned, -4- = Not Asked)
    p_economy = P6STGBSC, #Current economic situation of the country (1 = very good, 5 = very bad, -1- = Don't know, -2- = No answer/Refused, -3- = Not applicable, -4- = Not asked)
    p_democracy_preferable = P12STGBS, #Support for democracy (1 = Democracy is preferable to any other kind of government, 3 = For people like me, it does not matter whether we have a democratic or a non-democratic regime, -1- = Don't know, -2- = No answer/Refused, -3- = Not applicable, -4- = Not asked)
    p_democracy_satisfaction = P13STGBS.A, #Satisfaction with democracy, (1 = Very satisfied, 4 = Not satisfied at all, 5 = I don't understand what a democracy is, -1- = Don't know, -2- = No answer/Refused, -3- = Not applicable, -4- = Not asked)
    p_democracy_level = P18GBS, #How would you say is Democracy in your country?, (1 = A full democracy, 4 = Not a democracy, -1- = Don't know, -2- = No answer/Refused, -4- = Not asked)
    p_democracy_best = P24ST #Agreement/Disagreement: Democracy may have problems but it is the best system of government (1 = Strongly agree, 4 = Strongly disagree, -1- = Don't know, -2- = No answer/Refused, -3- = Not applicable, -4- = Not asked)
  ) |> #selects variables already found within the Latinobarometro dataset (df1)
  mutate(
    source = "Latinobarometro",
    r_female = if_else(
      df1$SEXO == 2, # sexo = Respondent's sex (1 = Man,  2 = Woman)
      1,  # output if Female
      0   # output if Male 
    )
  ) |> #mutates variables not already found within the Latinobarometro dataset
  relocate(
    source, .after = c_name
  ) |> #relocates source
  relocate(
    r_female, .after = r_swt
  ) #relocates r_female


#Creating the variable p_interest
#NOTE: I checked / counted each P19 A-M variable; there are no instances of -4-. This is therefore possible without mutating new variables replacing -4- with NA using na_if()

df3 <-df2 |>
    mutate(
      across(
        starts_with('p_politics_informed_by_'), ~
          if_else(
              df2$. == 1,
              1,
              0
              ) #the if_else takes the variables and converts them to a numeric binary that will be used to create the p_interest variable. Again, this is only possible because there were no -4- NA values..
            ) #across creates a function / applies the if_else to all variables starting with p_politics_informed_by
    ) |> 
  mutate(
    p_interest = 
      p_politics_informed_by_family +
      p_politics_informed_by_friends +
      p_politics_informed_by_coworkers +
      p_politics_informed_by_classmates +
      p_politics_informed_by_radio +
      p_politics_informed_by_newspapers_magazines +
      p_politics_informed_by_electronic_media_internet +
      p_politics_informed_by_television +
      p_politics_informed_by_facebook +
      p_politics_informed_by_twitter +
      p_politics_informed_by_youtube +
      p_politics_informed_by_other #sum of all p_politics (except for p_politics_informed_by_none)
    ) |> #p_interest is a value that represents how many of the p_politics_informed_by variables (family, friends, newspapers/magazines, etc.) the person learns about politics through. This could act as a very rough proxy indicator for interest in politics; people who are more interested in politics may be more likely to be informed via more avenues of political information
  select(-starts_with('p_politics_informed_by_')) |> #selects out the variables I created in order to create p_interest
  relocate(
    p_interest, .after = p_ideology
  ) |>
  arrange(
    c_name, source, r_id, r_year, r_month, r_swt, r_female, p_ideology, p_interest, p_economy, p_democracy_preferable, p_democracy_satisfaction, p_democracy_level, p_democracy_best
  ) #arranges the data in this order



#Cleaning all of the variables, in order of columns:

df4 <- df3 |>
  mutate(
    c_name = as.character(c_name)
  ) |> #changes the c_name values (from IDENPA) to a character, so that case_match works
  mutate(
    c_name = case_when(
      c_name == 170 ~ "Colombia",
      c_name == 218 ~ "Ecuador",
      c_name == 222 ~ "El Salvador",
      c_name == 320 ~ "Guatemala",
      c_name == 340 ~ "Honduras",
      c_name == 558 ~ "Nicaragua",
      c_name == 600 ~ "Paraguay",
      c_name == 604 ~ "Peru"
    ), #renames all observations in c_name from the IDENPA number to the country's name (ex. 170 --> Colombia)
    r_id = case_when(
      c_name == "Colombia" ~ (r_id - 1700000),
      c_name == "Ecuador" ~ (r_id - 2180000),
      c_name == "El Salvador" ~ (r_id - 2220000),
      c_name == "Guatemala" ~ (r_id - 3200000),
      c_name == "Honduras" ~ (r_id - 3400000),
      c_name == "Nicaragua" ~ (r_id - 5580000),
      c_name == "Paraguay" ~ (r_id - 6000000),
      c_name == "Peru" ~ (r_id - 6040000),
    ), #cleans the variable so that each country has its own r_id that doesn't include the original IDENPA number tacked onto the beginning
    r_year = as.numeric(r_year), #makes r_year a numeric variable (I looked it up, hard to say whether it is better as numeric or factor/categorical..., but the professional datasets I looked at had it as an integer...)
    r_month = as.numeric(r_month), #makes r_month a numeric variable
    r_swt = as.numeric(r_swt), #makes r_swt a numeric variable
    #I decided not to rename the months from 6-8 to June-August, as it will be easier for folks using the dataset to use as a numeric value
  )



#Recoding for universal NA missing values
df5 <- df4 |>
  mutate(
        r_id = na_if(r_id, 0)
  ) |> #recodes all of r_id's 0s to NA
  mutate(
        p_ideology = na_if(p_ideology, -8)
  ) |> #recodes all of p_ideology's -8s to NA (p_ideology is only variable with -8s; couldn't get function w/ everything() to work with na_if() - Error Message: "Can't convert `y` <double> to match type of `x` <character>.")
  mutate(
    across(
      starts_with("p_democracy_"), ~
        na_if(df4$., -5)
    )
  ) #turns all of the p_democracy_ variables' 5s (missing) into NAs


#Adding variable labels:

df6 = apply_labels(df5,
                      c_name = "Country Name",
                      source = "Source of Data",
                      r_id = "Respondent ID (Per Country)",
                      r_year = "Year of Interview",
                      r_month = c("June" = 6,
                                  "July" = 7,
                                  "August" = 8),
                      r_swt = "Survey Weight",
                      r_female = c("Non-Female" = 0,
                                   "Female" = 1),
                      p_ideology = c("Left" = 0,
                                    "1" = 1,
                                    "2" = 2,
                                    "3" = 3,
                                    "4" = 4,
                                    "5" = 5,
                                    "6" = 6,
                                    "7" = 7,
                                    "8" = 8,
                                    "9" = 9,
                                    "Right" = 10,
                                    "Don't Know" = -1,
                                    "No Answer/Refused" = -2,
                                    "Not Applicable" = -3,
                                    "Not Asked" = -4
                                    ),
                      p_interest = c("1 Source of Political Information" = 1,
                                     "2 Sources of Political Information" = 2,
                                     "3 Sources of Political Information" = 3,
                                     "4 Sources of Political Information" = 4,
                                     "5 Sources of Political Information" = 5,
                                     "6 Sources of Political Information" = 6,
                                     "7 Sources of Political Information" = 7,
                                     "8 Sources of Political Information" = 8,
                                     "9 Sources of Political Information" = 9,
                                     "10 Sources of Political Information" = 10,
                                     "11 Sources of Political Information" = 11,
                                     "12 Sources of Political Information" = 12
                                    ), #Level of interest in politics is based on the number of political sources of information (P19ST.A through P19ST.M variables) through which the person learns about politics. There are a total of 12 variables from the Latinobarometro data: P19ST.A, P19ST.B, P19ST.C, P19ST.D, P19ST.E, P19ST.F, P19ST.G, P19ST.H, P19NC.I, P19NC.J, P19F.K, P19ST.L, and P19ST.M
                      p_economy = c("Very Good" = 1,
                                    "Good" = 2,
                                    "About Average" = 3,
                                    "Bad" = 4,
                                    "Very Bad" = 5,
                                    "Don't Know" = -1,
                                    "No Answer/Refused" = -2,
                                    "Not Applicable" = -3,
                                    "Not Asked" = -4
                                    ),
                      p_democracy_preferable = c("Democracy is preferable to any other kind of government" = 1,
                                                 "Under some circumstances, an authoritarian government can be preferable to a democratic one" = 2,
                                                 "For people like me, it does not matter whether we have a democratic or a non-democratic regime" = 3,
                                                 "Bad" = 4,
                                                 "Don't Know" = -1,
                                                 "No Answer/Refused" = -2,
                                                 "Not Applicable" = -3,
                                                 "Not Asked" = -4
                                                ),
                      p_democracy_satisfaction = c("Very Satisfied" = 1,
                                                   "Rather Satisfied" = 2,
                                                   "Not Very Satisfied" = 3,
                                                   "Not At All Satisfied" = 4,
                                                   "Very Bad" = 5,
                                                   "Don't Know" = -1,
                                                   "No Answer/Refused" = -2,
                                                   "Not Applicable" = -3,
                                                   "Not Asked" = -4
                                                    ),
                      p_democracy_level = c("A Full Democracy" = 1,
                                            "A Democracy With Little Problems" = 2,
                                            "A Democracy With Big Problems" = 3,
                                            "Not A Democracy" = 4,
                                            "Very Bad" = 5,
                                            "Don't Know" = -1,
                                            "No Answer/Refused" = -2,
                                            "Not Applicable" = -3,
                                            "Not Asked" = -4
                                            ),
                      p_democracy_best = c("Strongly Agree" = 1,
                                           "Agree" = 2,
                                           "Disagree" = 3,
                                           "Strongly Disagree" = 4,
                                           "Don't Know" = -1,
                                           "No Answer/Refused" = -2,
                                           "Not Applicable" = -3,
                                           "Not Asked" = -4
                                          )
                  ) |>
      mutate(
          r_month = labelled(r_month, labels = c("June" = 6, "July" = 7, "August" = 8))
      ) # I messed up with the apply_labels, which does work for many. I'm going back and re-labeling those that do not work



#Renaming df6 to latinobarometro_2018
latinobarometro_2018 <- df6

#Writing final df6 data into publishable .csv and .RData

write.csv(latinobarometro_2018, "latinobarometro_2018.csv", row.names=FALSE)

save(latinobarometro_2018, file = "latinobarometro_2018.RData")
