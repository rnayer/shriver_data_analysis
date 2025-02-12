library(tidyverse)
library(dplyr)
library(tibble)
library(spData)
library(sf)
library(scales)
library(stargazer)
library(purrr)
library(rvest)
library(tidytext)
library(ggplot2)
library(shiny)
library(plotly)
library(spData)
library(sf)
library(RColorBrewer)

setwd("/Users/radhanayer/Desktop/Shriver Poverty Law Internship/Data Analysis/data")

# Step 1: Data wrangling to create a csv dataset which shows the DI of children in foster care by race 
# and state in 2021. 

# Download the 2021 Child Welfare Outcomes Report Data from the Children's Bureau 
# These datasets include child population data and foster care data. 

# Dataset 1: Total Child Population 
total_child_pop <- read_csv("Total Child Population.csv")

# Dataset 2: Child Population by Race and Ethnicity (Traditional)
pct_child_pop_by_race <- read_csv("Child Population by Race.csv")

# Dataset 3: Children in Care on the Last Day of FY by Race and Ethnicity (Traditional) 
pct_in_care_by_race <- read_csv("Children in Care by Race.csv")


# Create a function to calculate the state-wise DI of children in foster care by race. The DI calculation 
# involves dividing the percentage of children in care for a particular race by the percentage of the total 
# child population for the same race in a particular state 

calculate_di_in_care_by_state <- function(pct_in_care_by_race, pct_child_pop_by_race) {
  # Merge the two dataframes by 'State' and 'Year' to ensure matching rows
  merged_df <- merge(pct_in_care_by_race, pct_child_pop_by_race, by=c("State", "Year"))
  
  # Get the column names for the race percentages; exclude 'State' and 'Year' columns
  race_columns <- colnames(pct_in_care_by_race)[-c(1, 2)]  
  
  # Initialize an empty dataframe to store the results
  result_df <- merged_df[, c("State", "Year")]
  
  # Loop over each race column and perform the division
  for (race in race_columns) {
    # Define the corresponding column names from each dataframe with suffixes
    race_in_care <- paste(race, ".x", sep = "")  
    race_pop <- paste(race, ".y", sep = "")      
    
    # Perform the division
    result_column <- round(merged_df[[race_in_care]] / merged_df[[race_pop]], 2)
    
    # Handle division by zero or NA values if necessary
    result_column[is.infinite(result_column) | is.na(result_column)] <- NA
    
    # Add the result to the result dataframe
    result_column_name <- paste(race, "_DI", sep = "")
    result_df[[result_column_name]] <- result_column
  }
  
  return(result_df)
}

di_in_care_by_state <- calculate_di_in_care_by_state(pct_in_care_by_race, pct_child_pop_by_race)

di_in_care_by_state %>% 
  group_by(`Black-NH (%)_DI`) %>% 
  arrange(`Black-NH (%)_DI`) %>% 
  head(10)

top_10_states_american_indian <- di_in_care_by_state %>% 
  group_by(`Alaska Native / American Indian-NH (%)_DI`) %>% 
  arrange(desc(`Alaska Native / American Indian-NH (%)_DI`)) %>% 
  head(10)

# Output di_in_care_by_state to a csv document named "di_in_care_by_state.csv"
write.csv(di_in_care_by_state, "di_in_care_by_state.csv")


################################################################################
# Create Static Choropleths
# Load Boundaries - State shapefile from the US Census Bureau 
# Note: This step will need to be done again in creating the Shiny App in the shinyapp.R file
zippath <- "/Users/radhanayer/Desktop/Shriver Poverty Law Internship/Data Analysis/data"
zipF <- paste0(zippath, "cb_2018_us_state_500k.zip")
unzip(zipF,exdir=zippath)

zipcodes_shapefile <- st_read(file.path(zippath, 
                                        "/cb_2018_us_state_500k/cb_2018_us_state_500k.shp"))

zipcodes_shapefile$zip <- as.character(zipcodes_shapefile$zip)

# Merge di_in_care_by_state with the shapefile by State
merged_df <- merge(di_in_care_by_state, zipcodes_shapefile, by.x = "State", by.y = "NAME", all.x = TRUE)

# Filter out Hawaii, Alaska, Puerto Rico as they are geographically distant from the contiguous United States. This allows for a more 
# focused and appropriately sized visualization of the remaining states.
merged_df_sf <- st_sf(merged_df) %>% 
  filter(!State %in% c('Hawaii', 'Alaska', 'Puerto Rico'))

# Filter based on DI > 1 for Black children
# Assuming "Black-NH (%)_DI" is the column for Black children's DI
black_di <- ifelse(merged_df_sf$`Black-NH (%)_DI` > 1, "DI > 1", "DI <= 1")

# Add the black_di to the data frame
merged_df_sf$black_di <- black_di

# Generate the static plot for DI Black Children by State
ggplot(merged_df_sf) +
  geom_sf(aes(fill = black_di)) +
  scale_fill_manual(values = c("DI > 1" = "#27273F", "DI <= 1" = "white"), 
                    labels = c("DI > 1" = expression(DI > 1), "DI <= 1" = expression(DI <= 1)), 
                    limits = c("DI > 1", "DI <= 1")) +
  labs(title = "2021 Disproportionality Index (DI) for Black Children in Foster Care by State",
       fill = "Disproportionality Index",
       caption = "Source: Children’s Bureau. (2021). “Child Welfare Outcomes Report Data”.") +
  theme_minimal() +
  theme(plot.background = element_blank(), # This will set the plot background to white
        panel.border = element_blank(),   # This will remove panel borders
        panel.grid.major = element_blank(), # This will remove major grid lines
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), # Removes longitude labels
        axis.text.y = element_blank(), # Removes latitude labels
        axis.ticks = element_blank(), # Removes axis ticks
        axis.title.x = element_blank(), # Removes x-axis title
        axis.title.y = element_blank(),
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0, vjust = 0, margin = margin(t = 10, b = 10), colour = "grey50", size = 6))


# Filter based on DI > 1 for American Indian children
# Assuming "Alaska Native / American Indian-NH (%)_DI" is the column for Black children's DI
american_indian_di <- ifelse(merged_df_sf$`Alaska Native / American Indian-NH (%)_DI` > 1, "DI > 1", "DI <= 1")

# Add the black_di to the data frame
merged_df_sf$american_indian_di <- american_indian_di

# Generate the static plot for DI American Indian Children by State
ggplot(merged_df_sf) +
  geom_sf(aes(fill = american_indian_di)) +
  scale_fill_manual(values = c("DI > 1" = "#27273F", "DI <= 1" = "white"), 
                    labels = c("DI > 1" = expression(DI > 1), "DI <= 1" = expression(DI <= 1)), 
                    limits = c("DI > 1", "DI <= 1")) +
  labs(title = "2021 Disproportionality Index (DI) for American Indian Children in Foster Care by State",
       fill = "Disproportionality Index",
       caption = "Source: Children’s Bureau. (2021). “Child Welfare Outcomes Report Data”.") +
  theme_minimal() +
  theme(plot.background = element_blank(), # This will set the plot background to white
        panel.border = element_blank(),   # This will remove panel borders
        panel.grid.major = element_blank(), # This will remove major grid lines
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), # Removes longitude labels
        axis.text.y = element_blank(), # Removes latitude labels
        axis.ticks = element_blank(), # Removes axis ticks
        axis.title.x = element_blank(), # Removes x-axis title
        axis.title.y = element_blank(),
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0, vjust = 0, margin = margin(t = 10, b = 10), colour = "grey50", size = 6))


# ShinyApp: Interactive map of the disproportionality index by race and state 
# url to Shiny app:https://rnayer.shinyapps.io/DIAPPFinal/ 

# Define the mapping outside of the UI and server functions so that it's accessible to both
race_columns <- c(
  "American Indian" = "Alaska Native / American Indian-NH (%)_DI", 
  "Asian" = "Asian-NH (%)_DI", 
  "Black" = "Black-NH (%)_DI",
  "Native Hawaiian" = "Native Hawaiian / Other Pacific Islander-NH (%)_DI", 
  "Hispanic" = "Hispanic (%)_DI", 
  "White" = "White-NH (%)_DI", 
  "Multi-race" = "Two or More Races-NH (%)_DI"
)

# Define user interface
ui_2 <- fluidPage(
  selectInput(inputId = "race",
              label = "Race",
              choices = c(race_columns )),
  
  selectInput(inputId = "di_option",
              label = "Select Disproportionality Index (DI) option",
              choices = c("DI > 1" = "greater",
                          "DI = 1" = "equal",
                          "DI < 1" = "lesser")),
  
  selectInput(inputId = "state",
              label = "State",
              choices = NULL), 
  
  plotlyOutput("choropleth"),
  
  tags$div(style = "margin-top: 20px; font-size: 0.8em;",
           "Disproportionality Index (DI) refers to the presence of child groups in the welfare system compared to the general population. DI of 1.0 indicates no disproportionality, DI > 1.0 indicates overrepresentation, and DI < 1.0 indicates underrepresentation.")
)


# Define server
server_2 <- function(input, output, session) {
  path <- "/Users/radhanayer/Desktop/Shriver Poverty Law Internship/Data Analysis/data"
  di_in_care_by_state <- read_csv(file.path(path, "di_in_care_by_state.csv"))
  
  #Load Boundaries - States shapefile
  zipF <- paste0(path, "cb_2018_us_state_500k.zip")
  unzip(zipF,exdir=path)
  
  zipcodes_shapefile <- st_read(file.path(path, 
                                          "/cb_2018_us_state_500k/cb_2018_us_state_500k.shp"))
  
  # Merge di_in_care_by_state with the shapefile by State
  merged_df <- merge(di_in_care_by_state, zipcodes_shapefile, by.x = "State", by.y = "NAME", all.x = TRUE)
  
  merged_df_sf <- st_sf(merged_df) %>% 
    filter(!State %in% c('Hawaii', 'Alaska', 'Puerto Rico'))
  
  # Reactive data filtered by DI option and selected race
  filtered_data <- reactive({
    req(input$di_option, input$race) # Make sure input values are available
    selected_race_di <- input$race # Get the selected race's DI column name
    
    # Filter based on selected DI option
    df_with_hover <- merged_df_sf %>%
      mutate(
        selected_fill = case_when(
          get(selected_race_di) > 1 & input$di_option == "greater" ~ "DI > 1",
          get(selected_race_di) == 1 & input$di_option == "equal" ~ "DI = 1",
          get(selected_race_di) < 1 & input$di_option == "lesser" ~ "DI < 1",
          TRUE ~ "Not in selected DI category"
        ),
        hover_text = paste(State, "\nDI:", get(selected_race_di)) # Create hover text
      )
    return(df_with_hover)
  })
  
  # Reactive function for state-specific data
  state_data <- reactive({
    req(input$state) 
    filtered_data() %>%
      filter(State == input$state)
  })
  
  # Update state selectInput based on DI option
  observe({
    filtered_states <- unique(filtered_data() %>%
                                # Filter for states that match the selected DI option
                                filter(selected_fill %in% c("DI > 1", "DI = 1", "DI < 1")) %>%
                                # Pull the State names for those states
                                pull(State))
    # Update the state selectInput with the names of these states
    updateSelectInput(session, "state", choices = filtered_states)
  })
  
  # Render the choropleth map
  output$choropleth <- renderPlotly({
    req(filtered_data()) # Make sure the filtered data is available
    choropleth_data <- filtered_data()
    
    # Prepare the hover text for the selected state
    selected_state_hover_text <- if (!is.null(input$state) && input$state != "") {
      paste(input$state, "\nDI:", state_data()$hover_text)
    } else {
      NULL
    }
    
    race_title <- names(race_columns)[race_columns == input$race] # Get the proper race title
    
    # Generate the plot using the reactive data
    plot <- ggplot(choropleth_data) +
      geom_sf(aes(fill = selected_fill, text = hover_text)) +
      scale_fill_manual(values = c("DI > 1" = "#27273F",    
                                   "DI = 1" = "#27273F",    
                                   "DI < 1" = "#27273F",    
                                   "Not in selected DI category" = "white")) + 
      labs(title = paste("2021 Disproportionality Index for", race_title, "Children in Foster Care by State"), 
           fill = "Disproportionality Index",
           caption = "Source: Children’s Bureau. (2021). “Child Welfare Outcomes Report Data”.") +
      theme_void() +
      theme(legend.position = "bottom") # Adjust legend position if needed
    
    # Check if a state is selected and add an outline
    if (!is.null(input$state) && input$state != "") {
      plot <- plot + 
        geom_sf(data = state_data(), 
                fill = NA, 
                color = "#FBB200", 
                size = 2, 
                aes(text = selected_state_hover_text)) # Add hover text here
    }
    
    ggplotly(plot)
  })
  
}

# Run App 2
shinyApp(ui = ui_2, server = server_2)

################################################################################

# Static Plot 1: Plotting the distribution of reasons for removal by race

# Step 1: Data wrangling to create a csv dataframe which shows the distribution of children removed from 
# their homes by removal reason across racial groups 

# Download AFCARS data 
# Note: this raw data is not provided in the repository because the AFCARS file requires terms of use to be 
# signed. The file can be ordered free of charge from NDACAN
afcars_data <- read.table("FC2021v1.tab", sep = '\t', header = TRUE)

# Calculate the count of removals for each reason by race
reason_counts_by_race <- afcars_data %>%
  group_by(RaceEthn) %>%
  summarise(
    PHYABUSE = sum(PHYABUSE, na.rm = TRUE),
    SEXABUSE = sum(SEXABUSE, na.rm = TRUE),
    NEGLECT = sum(NEGLECT, na.rm = TRUE),
    AAPARENT = sum(AAPARENT, na.rm = TRUE),
    DAPARENT = sum(DAPARENT, na.rm = TRUE),
    AACHILD = sum(AACHILD, na.rm = TRUE),
    DACHILD = sum(DACHILD, na.rm = TRUE),
    CHILDIS = sum(CHILDIS, na.rm = TRUE),
    CHBEHPRB = sum(CHBEHPRB, na.rm = TRUE),
    PRTSDIED = sum(PRTSDIED, na.rm = TRUE),
    PRTSJAIL = sum(PRTSJAIL, na.rm = TRUE),
    NOCOPE = sum(NOCOPE, na.rm = TRUE),
    ABANDMNT = sum(ABANDMNT, na.rm = TRUE),
    RELINQSH = sum(RELINQSH, na.rm = TRUE),
    HOUSING = sum(HOUSING, na.rm = TRUE)
  )


# Create a new column which includes the sum of each of the categories for reasons of removal 
cat_sums <- reason_counts_by_race %>% 
  mutate(total_children = rowSums(.))

# Calculate the percentage of children who were removed from their homes by removal reason for all racial 
# groups 
percentages_removal_reason_by_race <- cat_sums %>%
  mutate(
    Percent_PHYABUSE = (PHYABUSE  / cat_sums$total_children) * 100,
    Percent_SEXABUSE = (SEXABUSE  / cat_sums$total_children) * 100,
    Percent_NEGLECT = (NEGLECT   / cat_sums$total_children) * 100,
    Percent_AAPARENT = (AAPARENT  / cat_sums$total_children) * 100,
    Percent_DAPARENT = (DAPARENT  / cat_sums$total_children) * 100,
    Percent_AACHILD = (AACHILD  / cat_sums$total_children) * 100,
    Percent_DACHILD = (DACHILD  / cat_sums$total_children) * 100,
    Percent_CHILDIS = (CHILDIS  / cat_sums$total_children) * 100,
    Percent_CHBEHPRB = (CHBEHPRB  / cat_sums$total_children) * 100,
    Percent_PRTSDIED = (PRTSDIED  / cat_sums$total_children) * 100,
    Percent_PRTSJAIL = (PRTSJAIL  / cat_sums$total_children) * 100,
    Percent_NOCOPE = (NOCOPE  / cat_sums$total_children) * 100,
    Percent_ABANDMNT = (ABANDMNT  / cat_sums$total_children) * 100,
    Percent_RELINQSH = (RELINQSH  / cat_sums$total_children) * 100,
    Percent_HOUSING = (HOUSING   / cat_sums$total_children) * 100
  )

# Only Select the relevant columns needed to show the distribution of removal reasons across race
percentages_removal_reason_by_race <- percentages_removal_reason_by_race %>%
  select(RaceEthn, starts_with("Percent")) %>% 
  mutate(others = rowSums(select(., -c("RaceEthn", "Percent_NEGLECT", "Percent_PHYABUSE", "Percent_NOCOPE", "Percent_HOUSING", "Percent_DAPARENT")), na.rm = TRUE))

# Reshape data to long format for ggplot2
long_percentages_removal_reason_by_race <- percentages_removal_reason_by_race %>% 
  select(c("RaceEthn", "Percent_NEGLECT", "Percent_PHYABUSE", "Percent_NOCOPE", "Percent_HOUSING", "Percent_DAPARENT", "others")) %>% 
  pivot_longer(!RaceEthn, names_to = 'Reason', values_to = 'percentage')

# Output long_percentages_removal_reason_by_race to a csv document named "distribution_removal_reason.csv"
write.csv(long_percentages_removal_reason_by_race, "distribution_removal_reason.csv")

# Download the distribution_removal_reason csv file
distribution_removal_reason <- read_csv("distribution_removal_reason.csv")

# Define race labels to replace coded race information in the dataset with descriptive text, making the 
# plot more readable

# Reorder the levels of the RaceEthn factor based on your specified order
distribution_removal_reason$RaceEthn <- factor(distribution_removal_reason$RaceEthn, 
                                               levels = c('3', '6', '2', '5', '7', '1', '4', '99'))

# Now adjust the race labels based on the new factor level order
race_labels <- c(
  '3' = 'American Indian',
  '6' = 'Multi-race',
  '2' = 'Black',
  '5' = 'Native Hawaiian',
  '7' = 'Hispanic',
  '1' = 'White',
  '4' = 'Asian', 
  '99' = 'Unknown'
)

#race_labels <- c(
 # '1' = 'White',
 # '2' = 'Black',
 # '3' = 'American Indian',
 # '4' = 'Asian',
 # '5' = 'Native Hawaiian',
 #  '6' = 'Multi-race',
 # '7' = 'Hispanic', 
#  '99' = 'Unknown'
#)

# Custom labels for the legend to improve clarity of the legend in the plot
custom_legend_labels <- c(
  "others" = "Other Reasons", 
  "Percent_NEGLECT" = "Neglect",
  "Percent_PHYABUSE" = "Physical Abuse",
  "Percent_NOCOPE" = "Caretaker Inability Cope",
  "Percent_HOUSING" = "Inadequate Housing",
  "Percent_DAPARENT" = "Drug Abuse Parent"
)

# Set color palette using the brewer.pal function from the RColorBrewer package. Select  6 colors from the 
# "Set2" palette, which will be used to differentiate between the reasons for removal in the plot
color_scheme <- brewer.pal(6, "Set2")  # Source: https://r-graph-gallery.com/209-the-options-of-barplot.html

# Use the cleaned distribution_removal_reason data to plot the distribution of children removed from home 
# by removal reason across racial groups for 2021
ggplot(distribution_removal_reason , aes(x = factor(RaceEthn), y = percentage, fill = Reason)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels = race_labels, expand = c(0, 0)) + 
  scale_fill_manual(values = c("#FBB200", "#F7F0BE", "#6BB58E", "#575C6B", "#27273F", "#BEBEC5"), 
                    name = "Reason",  labels = custom_legend_labels) + 
  labs(
    title = "Distribution of Children Removed from Home by Removal Reason Across Racial/Ethnic Groups", 
    subtitle = "United States, 2021",
    x = "Race/Ethnicity", 
    y = "Percentage of Children (%)", 
    caption = "Neglect refers to alleged or substantiated negligent treatment or maltreatment, including failure to provide adequate food, clothing, shelter or care. 
    \nInadequate Housing refers to conditions where housing facilities were substandard, overcrowded, unsafe or otherwise inadequate resulting in their not being\nappropriate for the parents and child to reside together. Also includes homelessness.
    \nOther Reasons include sexual abuse, alcohol abuse parent, alcohol abuse child, drug abuse child, child disability, child behavior problem, parent death,\nincarceration, caretaker inability cope, abandonment, and relinquishment.
    
    \nSource: National Data Archive on Child Abuse and Neglect (NDACAN). (2021). “Adoption and Foster Care Analysis and Reporting (AFCARS), Foster Care File”."
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(hjust = 0, vjust = 0, margin = margin(t = 30, b = 10), colour = "grey50", size = 6),
        plot.title = element_text(hjust = 0.5, size = 10),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

################################################################################

# Static Plot 2: Plotting the Disproportionality Index (DI) of children in the foster care system for 2021

# Data wrangling to create a csv dataset which shows the Disproportionality Index (DI) of children 
# in, entering, and existing foster care by race in 2021

# Download the 2021 Child Welfare Outcomes Report Data from the Children's Bureau 
# These datasets include child population data and foster care data. 

# Dataset 1: Total Child Population 
total_child_pop <- read_csv("Total Child Population.csv")

# Dataset 2: Child Population by Race and Ethnicity (Traditional)
pct_child_pop_by_race <- read_csv("Child Population by Race.csv")

# Merge dataset 1 and dataset 2 first to get all the statewise population data in one dataframe
merged_pop_by_race <- total_child_pop %>%
  inner_join(pct_child_pop_by_race, by = c("State", "Year"))

# Dataset 3: Children in Care on the Last Day of FY by Race and Ethnicity (Traditional) 
pct_in_care_by_race <- read_csv("Children in Care by Race.csv")

# Dataset 4: Number of Children in Care 
number_in_care <- read_csv("Number In Foster Care on the Last Day of FY.csv")

# Merge dataset 3 and dataset 4 to get all the statewise 'in care' data in one dataframe 
merged_in_care_by_race <- number_in_care %>% 
  inner_join(pct_in_care_by_race, by = c("State", "Year"))

# Dataset 5: Children Entering Care During FY by Race and Ethnicity (Traditional) 
pct_entering_care_by_race <- read_csv("Children Entering Care by Race.csv")

# Dataset 6: Number of Children Entering Care 
number_entering_care <- read_csv("Number Entered Foster Care During FY.csv")

# Merge dataset 5 and dataset 6 to get all the statewise 'entering care' data in one dataframe 
merged_entering_care_by_race <- number_entering_care %>% 
  inner_join(pct_entering_care_by_race, by = c("State", "Year"))

# Dataset 7: Children Exiting Care During FY by Race and Ethnicity (Traditional)
pct_exiting_care_by_race <- read_csv("Children Exiting Care by Race.csv")

# Dataset 8: Number of Children Exiting Care 
number_exiting_care <- read_csv("Number Exited Foster Care During FY.csv")

# Merge dataset 7 and dataset 8 to get all the statewise 'exiting care' data in one dataframe 
merged_exiting_care_by_race <- number_exiting_care %>% 
  inner_join(pct_exiting_care_by_race, by = c("State", "Year"))

# Create a function to calculate the DI of the status of children within foster care by race
calculate_di_within_care <- function(race_column_name, merged_pop_by_race, df_2) {
  
  # Compute the nationwide ratio of children in the total population, broken down by race, by calculating 
  # a weighted measure for each state
  weighted_pop_by_race <- merged_pop_by_race %>%
    mutate(
      weighted_race = .[[race_column_name]] * (.[['Total Children Under 18']] / 100)
    ) %>%
    summarise(total_weighted_race = sum(weighted_race, na.rm = TRUE)/sum(.[['Total Children Under 18']], na.rm=TRUE))
  
  
  # Calculate the nationwide weighted proportion of children in foster care by race, 
  # serving as the numerator in calculating the DI
  weighted_numerator <- df_2 %>%
    mutate(
      weighted_care = .[[race_column_name]] * .[['Number']] / 100
    ) %>%
    summarise(total_weighted_care = sum(weighted_care, na.rm = TRUE)/sum(.[['Number']], na.rm = TRUE))
  
  
  # Calculate the DI for each racial group by dividing the proportion of children of a specific race in 
  # foster care by their proportion in the general population
  di_within_care <- weighted_numerator$total_weighted_care / weighted_pop_by_race$total_weighted_race
  
  return(di_within_care)
}

# Define race categories before calling the function to ensure accurate calculations by using the 
# appropriate columns in the dataset that correspond to each racial group
race_categories <- c(
  "Alaska Native / American Indian-NH (%)",
  "White-NH (%)", 
  "Black-NH (%)", 
  "Hispanic (%)",
  "Asian-NH (%)",
  "Native Hawaiian / Other Pacific Islander-NH (%)",
  "Two or More Races-NH (%)"
)

# Create empty lists to store DI values. Source: https://www.r-bloggers.com/2023/08/the-unlist-function-in-r/
in_care_di <- list()
entering_care_di <- list()
exiting_care_di <- list()

# Calculate DI for each race and each care status
for (race in race_categories) {
  in_care_di[[race]] <- calculate_di_within_care(race, merged_pop_by_race, merged_in_care_by_race)
  entering_care_di[[race]] <- calculate_di_within_care(race, merged_pop_by_race, merged_entering_care_by_race)
  exiting_care_di[[race]] <- calculate_di_within_care(race, merged_pop_by_race, merged_exiting_care_by_race)
}

# Combine the DI values into a single dataframe with a row for each racial category and columns for each 
# care status
di_within_care_df <- data.frame(
  InCare = unlist(in_care_di),
  EnteringCare = unlist(entering_care_di),
  ExitingCare = unlist(exiting_care_di),
  row.names = race_categories 
) %>%
  rownames_to_column(var = "Race") %>% 
  as_tibble() # Source: https://hbctraining.github.io/Intro-to-R/lessons/08_intro_tidyverse.html 


# Reshape data to long format for ggplot2
di_within_care_long <- pivot_longer(di_within_care_df, cols = c("InCare", "EnteringCare", "ExitingCare"), 
                                    names_to = "CareStatus", values_to = "DI")

# Output di_within_care_df to a csv document named "di_within_care.csv"
write.csv(di_within_care_long, "di_within_care.csv")

# Download the di_within_care csv file
di_within_care <- read_csv("di_within_care.csv")

# Convert the CareStatus column in the di_within_care_df dataframe to a factor, to ensure this categorical
# data is treated correctly in plotting. 
di_within_care$CareStatus <- factor(di_within_care$CareStatus)

# Set the order of the factor levels to "In Care", "Entering Care", and "Exiting Care", ensuring that these 
# levels are used in the specified order when plotting.
levels(di_within_care$CareStatus) <- c("In Care", "Entering Care", "Exiting Care")


# Set color palette using the brewer.pal function from the RColorBrewer package. Select  5 colors from the 
# "Set2" palette, which will be used to differentiate between the status of children within foster care in 
# the plot
color_scheme <- brewer.pal(5, "Set2") 


# Use the cleaned di_within_care data to plot the DI of the Status of Children Within Foster Care System by 
# race in 2021
ggplot(di_within_care, aes(x = Race, y = DI, fill = CareStatus)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#FBB200", "#6BB58E", "#575C6B")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Disproportionality Index: Status of Children Within Foster Care System",
    subtitle = "A Comparative Analysis Across Racial/Ethnic Groups in the United States, 2021",
    x = "Race/Ethnicity",
    y = "Disproportionality Index (DI)", 
    caption = "Disproportionality Index (DI) refers to the presence of child groups in the welfare system compared to the general population. DI of 1.0 indicates no\ndisproportionality, DI > 1.0 indicates overrepresentation, and DI < 1.0 indicates underrepresentation.
    \n'In Care' refers to children in foster care at the end of the fiscal year, 'Entering Care' and 'Exiting Care' refer to those who entered or exited during it.
    \nMulti-race refers to combinations of two or more of the any of the race categories
    
    \nSource: Children’s Bureau. (2021). “Child Welfare Outcomes Report Data”."
  ) + 
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, vjust = 0, margin = margin(t = 30, b = 10), colour = "grey50", size = 6), 
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.title = element_blank(), 
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, b = 10), angle = 0, vjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(), 
    panel.background = element_blank(),
    legend.background = element_blank(),
  ) +
  # Specify the exact set and order of the racial groups that should appear on the x-axis of the plot and 
  # simplify the labels of the racial groups
  scale_x_discrete(
    limits = c(
      "Alaska Native / American Indian-NH (%)", "Two or More Races-NH (%)",
      "Black-NH (%)", "Native Hawaiian / Other Pacific Islander-NH (%)",
      "Hispanic (%)", "White-NH (%)", "Asian-NH (%)"
    ),
    labels = c(
      "American Indian", "Multi-race", "Black",
      "Native Hawaiian", "Hispanic", "White", "Asian"
    ), 
    expand = c(0, 0)
  ) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))

################################################################################

# Static Plot 3: Plotting the distribution of reasons for exiting foster care by race

# Data wrangling to create a csv dataset which shows the distribution of exits from care by 
# discharge type and race

# Download the "Exits from Foster Care by Race & Ethnicity (Traditional)" data from the 2021 Child Welfare 
# Outcomes Report Data, Children's Bureau  

# Dataset 1: American Indian Dataset 
exits_from_care_american_indian <- read_csv("Exits American Indian.csv")

# Dataset 2: Asian Dataset 
exits_from_care_asian <- read_csv("Exits Asian.csv")

# Dataset 3: Black Dataset 
exits_from_care_black <- read_csv("Exits Black.csv")

# Dataset 4: Native Hawaiian Dataset 
exits_from_care_native_hawaiian <- read_csv("Exits Native Hawaiian.csv")

# Dataset 5: White Dataset 
exits_from_care_white <- read_csv("Exits White.csv")

# Dataset 6: Hispanic Dataset 
exits_from_care_hispanic <- read_csv("Exits Hispanic.csv")

# Dataset 7: Multi Race Dataset 
exits_from_care_multi_race <- read_csv("Exits Multi Race.csv")

# Create a function to calculate the distribution of exits from care by discharge type
calculate_discharge_distribution <- function(df_2, race_category) {
  
  # Calculate the total number of children in care (Total_Children_In_Care) by summing up the Number of Children column. For each 
  # discharge type (Adoption, Guardianship, Reunification, Other, and Missing Data), calculate the percentage of children discharged 
  # by that type relative to the total number of children in care. 
  discharge_distribution <- df_2 %>% 
    summarise(
      Race = race_category, 
      Total_Children_In_Care = sum(`Number of Children`, na.rm = TRUE),
      Pct_Adoption = (sum(`Adoption (%)` / 100 * `Number of Children`, na.rm = TRUE) / Total_Children_In_Care) * 100,
      Pct_Guardianship = (sum(`Guardianship (%)` / 100 * `Number of Children`, na.rm = TRUE) / Total_Children_In_Care) * 100,
      Pct_Reunification = (sum(`Reunification (%)` / 100 * `Number of Children`, na.rm = TRUE) / Total_Children_In_Care) * 100,
      Pct_Other = (sum(`Other (%)` / 100 * `Number of Children`, na.rm = TRUE) / Total_Children_In_Care) * 100,
      Pct_Missing_Data = (sum(`Missing Data (%)` / 100 * `Number of Children`, na.rm = TRUE) / Total_Children_In_Care) * 100
    )
  
  return(discharge_distribution)
}


# Corresponding dataframes for each race category
exits_from_care_dataframes <- list(
  exits_from_care_american_indian,
  exits_from_care_white,
  exits_from_care_black,
  exits_from_care_hispanic,
  exits_from_care_asian,
  exits_from_care_native_hawaiian,
  exits_from_care_multi_race
)

# Initialize an empty list to store results
discharge_distribution_results <- list()

# Loop over the dataframes and calculate discharge distribution for each one based on a given race category
for (i in seq_along(exits_from_care_dataframes)) {
  current_df = exits_from_care_dataframes[[i]]
  current_race = race_categories[i]
  current_results <- calculate_discharge_distribution(current_df, current_race)
  discharge_distribution_results[[i]] <- current_results
}

# Combine all results into a single dataframe
combined_discharge_distribution_df <- bind_rows(discharge_distribution_results)

# Reshape data to long format for ggplot2
long_discharge_distribution_df <- pivot_longer(combined_discharge_distribution_df, 
                                               cols = c("Pct_Adoption",	"Pct_Guardianship",	"Pct_Reunification",	"Pct_Other",	"Pct_Missing_Data"), 
                                               names_to = "Discharge_Type", values_to = "Percentage")


# Output di_within_care_df to a csv document named "di_within_care.csv"
write.csv(long_discharge_distribution_df, "combined_discharge_distribution.csv", row.names = FALSE)

# Download the combined_discharge_distribution csv file
discharge_type_distribution <- read_csv("combined_discharge_distribution.csv") %>%
  filter(Discharge_Type != "Pct_Missing_Data")

# Convert 'Race' column to a factor with simplified labels
discharge_type_distribution$Race <- factor(discharge_type_distribution$Race,
                                           levels = c(
                                             "Alaska Native / American Indian-NH (%)",
                                             "Two or More Races-NH (%)",
                                             "Black-NH (%)",
                                             "Native Hawaiian / Other Pacific Islander-NH (%)",
                                             "Hispanic (%)",
                                             "White-NH (%)", 
                                             "Asian-NH (%)"
                                           ),
                                           labels = c(
                                             "American Indian",
                                             "Multi-race",
                                             "Black",
                                             "Native Hawaiian",
                                             "Hispanic",
                                             "White",
                                             "Asian"
                                           )
)

# Reorder the 'Discharge_Type' column
discharge_type_distribution$Discharge_Type <- factor(discharge_type_distribution$Discharge_Type, 
                                                     levels = c("Pct_Reunification", "Pct_Adoption", "Pct_Guardianship", "Pct_Other", "Pct_Missing_Data"))

# Define labels for the legend
labels_scheme <- c("Pct_Adoption" = "Adoption", "Pct_Guardianship" = "Guardianship", "Pct_Reunification" = "Reunification", "Pct_Other" = "Other", "Pct_Missing_Data" = "Missing Data")

# Use the cleaned combined_discharge_distribution data to plot the distribution of foster care exits by 
# discharge type across racial groups for 2021
ggplot(discharge_type_distribution, aes(x = Race, y = Percentage, fill = Discharge_Type)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = c("#FBB200", "#F7F0BE", "#6BB58E", "#575C6B"), labels = labels_scheme) + 
  labs(
    title = "Distribution of Foster Care Exits by Discharge Type Across Racial/Ethnic Groups",
    subtitle = "United States, 2021",
    x = "Race/Ethnicity",
    y = "Percentage of Children (%)", 
    caption = "\nSource: Children’s Bureau. (2021). “Child Welfare Outcomes Report Data”."
  ) + 
  theme_minimal() + 
  theme(
    plot.caption = element_text(hjust = 0, vjust = 0, margin = margin(t = 10, b = 10), colour = "grey50"), 
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.title = element_blank(), 
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10, b = 10), angle = 0, vjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(), 
    panel.background = element_blank(),
    legend.background = element_blank()
  )

