#### Pull Survey Item Data from Qualtrics ####

# load packages
library(tidyverse) # for a bunch of things
library(qualtRics) # to pull qualtrics data https://github.com/ropensci/qualtRics
library(dplyr) # to pipe and combine columns
library(purrr) # to map data
library(stringr) # to work with text data
library(writexl) # to save out xlsx documents
library(rlang) # to use special piping ||


#### Setup ####
# go to Qualtrics account settings to find the API key + base url
# base url should be: yourdatacenterid.qualtrics.com

qualtrics_api_credentials(api_key = "",
                          base_url = "",
                          install = TRUE)

# resets the R environment now that the API is saved to our underlying environment thus you can now delete out your API key + base url to hide it.
readRenviron("~/.Renviron")

# pull a list of all surveys
surveys <- all_surveys()

# or get a specific survey from the list e.g., surey #8
mysurvey <- fetch_survey(surveyID = surveys$id[8],
                         verbose = TRUE)

# or to pull a specific survey by survey ID e.g., "SV_abc123"
# mysurvey <- fetch_survey("SV_eghQ9ayDENL9z3o")

# pull questions, col, meta data in single dataframes
questions<-survey_questions(surveys$id[8])

col<-extract_colmap(surveys$id[8])

# extract meta data
meta<-metadata(surveys$id[8])


### Single Survey MetaData Extraction  ####

### pull the meta data into a df ###
meta_df <- tibble::as_tibble(meta$metadata) |>
  pivot_longer(
    cols = everything(),
    names_to = "metadata_variable",
    values_to = "metadata_value",
    values_transform = as.character  # Ensures all values are character
  )


### pull the questions into a df ###
# Generate a dataframe by looping over all questions in meta$questions
questions_df <- map_dfr(meta$questions, function(q) {

  # Ensure all fields are handled gracefully even if missing
  base_fields <- tibble(
    questionName = q$questionName %||% NA,
    questionText = as.character(q$questionText %||% NA),
    questionLabel = as.character(q$questionLabel %||% NA),

    # Handle missing fields in questionType
    questionType_type = as.character(q$questionType$type %||% NA),
    questionType_selector = as.character(q$questionType$selector %||% NA),
    questionType_subSelector = as.character(q$questionType$subSelector %||% NA),

    # Handle validation
    validation_doesForceResponse = as.character(q$validation$doesForceResponse %||% NA)
  )

  # If there are choices, expand them, otherwise return an empty tibble
  choices_expanded <- if(length(q$choices) > 0) {
    map_dfr(q$choices, function(c) {
      tibble(
        choice_recode = as.character(c$recode %||% NA),
        choice_description = as.character(c$description %||% NA),
        choice_choiceText = as.character(c$choiceText %||% NA),
        choice_imageDescription = as.character(c$imageDescription %||% NA),
        choice_variableName = as.character(c$variableName %||% NA),
        choice_analyze = as.character(c$analyze %||% NA)
      )
    })
  } else {
    tibble(  # Return an empty tibble with the same column structure
      choice_recode = NA_character_,
      choice_description = NA_character_,
      choice_choiceText = NA_character_,
      choice_imageDescription = NA_character_,
      choice_variableName = NA_character_,
      choice_analyze = NA_character_
    )
  }

  # Combine both question details and expanded choices (ensure all rows match)
  bind_cols(base_fields, choices_expanded)
})

# Coerce all columns to character type to avoid issues with differing types
questions_df <- questions_df %>%
  mutate(across(everything(), as.character))

# clean column to remove html coding e.g., <br>
questions_df$questionText <- questions_df$questionText %>%
  str_replace_all("<[^>]*>", " ") %>%
  str_squish()  # removes extra whitespace and trims


### pull the response data into a df ###
response_df <- tibble::as_tibble(meta$responsecounts) |>
  pivot_longer(
    cols = everything(),
    names_to = "responsedata_variable",
    values_to = "responsedata_value",
    values_transform = as.character  # Ensures all values are character
  )


### merge the meta, question, and response dfs ###
combined_df <- cross_join(response_df, meta_df)
combined_df <- cross_join(questions_df, combined_df)


### save out ###
write_xlsx(combined_df, "pathway.xlsx")



#### Loop through All Surveys MetaData in Qualtrics ####
# pull a list of all surveys
surveys <- all_surveys()


### pull the meta data into a df ###
# Loop over each survey ID and extract metadata
meta_df_all <- map_dfr(surveys$id, function(survey_id) {
  meta <- metadata(survey_id)

  tibble::as_tibble(meta$metadata) |>
    pivot_longer(
      cols = everything(),
      names_to = "metadata_variable",
      values_to = "metadata_value",
      values_transform = as.character
    ) |>
    mutate(survey_id = survey_id)
})



### pull the questions into a df ###
# Helper to safely extract fields
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Loop over survey IDs and extract questions
questions_df_all <- map_dfr(surveys$id, function(survey_id) {
  meta <- metadata(survey_id)

  map_dfr(meta$questions, function(q) {
    # Basic fields
    base_fields <- tibble(
      questionName = q$questionName %||% NA,
      questionText = as.character(q$questionText %||% NA),
      questionLabel = as.character(q$questionLabel %||% NA),
      questionType_type = as.character(q$questionType$type %||% NA),
      questionType_selector = as.character(q$questionType$selector %||% NA),
      questionType_subSelector = as.character(q$questionType$subSelector %||% NA),
      validation_doesForceResponse = as.character(q$validation$doesForceResponse %||% NA)
    )

    # Expand choices if they exist
    choices_expanded <- if (length(q$choices) > 0) {
      map_dfr(q$choices, function(c) {
        tibble(
          choice_recode = as.character(c$recode %||% NA),
          choice_description = as.character(c$description %||% NA),
          choice_choiceText = as.character(c$choiceText %||% NA),
          choice_imageDescription = as.character(c$imageDescription %||% NA),
          choice_variableName = as.character(c$variableName %||% NA),
          choice_analyze = as.character(c$analyze %||% NA)
        )
      })
    } else {
      tibble(
        choice_recode = NA_character_,
        choice_description = NA_character_,
        choice_choiceText = NA_character_,
        choice_imageDescription = NA_character_,
        choice_variableName = NA_character_,
        choice_analyze = NA_character_
      )
    }

    # Combine and tag with survey ID
    bind_cols(base_fields, choices_expanded) |>
      mutate(survey_id = survey_id)
  })
})



# clean column to remove html coding e.g., <br>
questions_df_all$questionText <- questions_df_all$questionText %>%
  str_replace_all("<[^>]*>", " ") %>%
  str_squish()  # removes extra whitespace and trims


### pull the response data into a df ###
response_df_all <- map_dfr(surveys$id, function(survey_id) {
  meta <- metadata(survey_id)

  tibble::as_tibble(meta$responsecounts) |>
    pivot_longer(
      cols = everything(),
      names_to = "responsedata_variable",
      values_to = "responsedata_value",
      values_transform = as.character
    ) |>
    mutate(survey_id = survey_id)
})



### merge the meta, question, and response dfs ###
combined_df <- meta_df_all |>
  full_join(questions_df_all, by = "survey_id") |>
  full_join(response_df_all, by = "survey_id")



### save out ###
write_xlsx(combined_df, "pathway.xlsx")


#### End ####