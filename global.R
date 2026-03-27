# ============================================================
# global.R
# ============================================================
# PURPOSE:
# This file runs ONCE when the app starts, before any user
# sees anything. It loads packages, reads data, and defines
# helper functions that are shared across the whole app.
#
# THINK OF IT AS: the prep kitchen. Everything gets ready here.
#
# TO UPDATE DATA:
# Just edit the CSV files — no code changes needed here.
# ============================================================


# ------------------------------------------------------------
# Packages
# These are the tools our app needs. Think of them like
# apps you download — they add capabilities R doesn't have
# built in. We load them once here so ui.R and server.R
# can use them without loading again.
# ------------------------------------------------------------

library(shiny)   # the framework that makes web apps in R
library(readr)   # reads CSV files cleanly
library(dplyr)   # filters and transforms data
library(DT)      # makes interactive tables
library(bslib)   # modern styling and layout tools


# ------------------------------------------------------------
# Data
# read_csv() reads a CSV file and stores it as a "data frame"
# in R. A data frame is like a spreadsheet in memory —
# rows are observations, columns are variables.
#
# The <- symbol is R's assignment operator.
# It means "take what's on the right and store it as the
# name on the left". So courses <- read_csv(...) means
# "read this file and call it 'courses'".
# ------------------------------------------------------------

courses   <- read_csv("course_audit.csv")
themes    <- read_csv("theme.csv")
subthemes <- read_csv("subtheme.csv")


# ------------------------------------------------------------
# Quick data check
# message() prints to the console (not visible to users).
# nrow() counts the number of rows in a data frame.
# paste0() glues text and values together into one string.
# This just confirms our data loaded correctly on startup.
# ------------------------------------------------------------

message("Courses loaded: ",   nrow(courses),   " rows")
message("Themes loaded: ",    nrow(themes),    " rows")
message("Subthemes loaded: ", nrow(subthemes), " rows")


# ------------------------------------------------------------
# Helper Function 1: make_detail_html()
#
# A "function" in R is a reusable block of code that takes
# inputs, does something, and returns an output.
# This function takes ONE row of course data and builds
# the HTML block that appears when you click to expand a row.
#
# We define it HERE in global.R because server.R needs it
# when building the table, and it has nothing to do with
# layout (ui.R) or filtering logic. It's a utility.
# ------------------------------------------------------------

make_detail_html <- function(row) {
  
  # Handle missing prerequisites gracefully
  prereq_text <- if (is.na(row$prereq) || row$prereq == "") {
    "None listed"
  } else {
    row$prereq
  }
  
  # paste0() glues all these strings together into one
  # big HTML string. The result is valid HTML that the
  # browser renders when a row is expanded.
  paste0('
    <div style="
      border-left: 4px solid #0054a4;
      background: #f8f9fa;
      padding: 18px 24px;
      margin: 4px 24px 8px 48px;
      border-radius: 0 4px 4px 0;
      font-size: 0.93em;
    ">
      <div style="font-weight:700; font-size:1.05em; margin-bottom:4px;">',
         row$title,
         '</div>
      <div style="color:#666; margin-bottom:16px;">',
         row$department, ' &nbsp;·&nbsp; ', row$credit_hours, ' credit hours
      </div>
      <div style="font-weight:700; font-size:0.75em; text-transform:uppercase;
        letter-spacing:0.06em; color:#888; margin-bottom:2px;">Theme</div>
      <div style="margin-bottom:12px;">', row$theme, '</div>
      <div style="font-weight:700; font-size:0.75em; text-transform:uppercase;
        letter-spacing:0.06em; color:#888; margin-bottom:2px;">Subtheme</div>
      <div style="margin-bottom:12px;">', row$subtheme, '</div>
      <div style="font-weight:700; font-size:0.75em; text-transform:uppercase;
        letter-spacing:0.06em; color:#888; margin-bottom:2px;">Description</div>
      <div style="margin-bottom:12px; line-height:1.6;">',
         row$course_description,
         '</div>
      <div style="font-weight:700; font-size:0.75em; text-transform:uppercase;
        letter-spacing:0.06em; color:#888; margin-bottom:2px;">Prerequisites</div>
      <div style="line-height:1.6;">', prereq_text, '</div>
    </div>
  ')
}


# ------------------------------------------------------------
# Helper Function 2: build_themes_tab()
#
# This function builds the entire Themes & Definitions tab
# content. It takes the themes and subthemes data frames
# and returns a list of UI elements (HTML blocks).
#
# lapply() is an R function that loops over a list or
# sequence and applies a function to each item.
# Think of it like "for each theme, do this..."
# ------------------------------------------------------------

build_themes_tab <- function(themes_df, subthemes_df) {
  
  # Loop over each theme (1 through however many there are)
  theme_blocks <- lapply(1:nrow(themes_df), function(i) {
    
    this_theme     <- themes_df[i, ]     # grab row i from themes
    this_subthemes <- subthemes_df |>    # filter subthemes that
      filter(theme == this_theme$theme)  # belong to this theme
    
    # For each subtheme: bold name + plain definition, indented
    # No grey box, no border — just clean text inside theme's box
    subtheme_entries <- lapply(1:nrow(this_subthemes), function(j) {
      sub <- this_subthemes[j, ]
      tags$div(
        style = paste(
          "padding-left: 16px;",
          "margin-top: 14px;",
          "background: transparent;",
          "border: none;",
          "border-left: none;"
        ),
        tags$div(
          style = paste(
            "font-weight: 700;",
            "font-size: 0.92em;",
            "color: #222;",
            "margin-bottom: 3px;",
            "background: transparent;"
          ),
          sub$subtheme
        ),
        tags$div(
          style = paste(
            "font-size: 0.87em;",
            "color: #555;",
            "line-height: 1.55;",
            "background: transparent;"
          ),
          sub$subtheme_definition
        )
      )
    })
    
    # One grey callout box per theme
    # ONLY this outer div gets the grey + blue bar
    tags$div(
      style = "margin-bottom: 20px;",
      tags$div(
        style = paste(
          "background: #f8f9fa;",
          "border-left: 4px solid #0054a4;",
          "border-radius: 0 4px 4px 0;",
          "padding: 18px 22px 20px 22px;"
        ),
        
        # Theme name — bold
        tags$div(
          style = paste(
            "font-weight: 700;",
            "font-size: 1.05em;",
            "color: #111;",
            "margin-bottom: 4px;",
            "background: transparent;"
          ),
          this_theme$theme
        ),
        
        # Theme definition — normal weight
        tags$div(
          style = paste(
            "font-size: 0.9em;",
            "color: #555;",
            "line-height: 1.5;",
            "background: transparent;"
          ),
          this_theme$theme_definition
        ),
        
        # Divider between theme and its subthemes
        tags$hr(style = "margin: 12px 0 4px 0; border-color: #ddd;"),
        
        # All subthemes for this theme
        tagList(subtheme_entries)
      )
    )
  })
  
  tagList(theme_blocks)
}