# Movie Recommender System - R Shiny App

This project is a movie recommender system built as part of UIUC CS 598 PSL using the MovieLens dataset. The app demonstrates both **content-based** and **collaborative filtering** recommendation strategies implemented in R and deployed through a Shiny web application.

## Author
**Anthony Petrotte**  
University of Illinois Urbana-Champaign

[Link to deployed app](https://adp12.shinyapps.io/PSL_app/)

---

## Table of Contents
- [Overview](#overview)
- [Tech Stack](#tech-stack)
- [Recommender Systems Implemented](#recommender-systems-implemented)
  - [System 1: Genre-Based Filtering](#system-1-genre-based-filtering)
  - [System 2: Collaborative Filtering](#system-2-collaborative-filtering)
- [Key Algorithms](#key-algorithms)
- [App Features](#app-features)
- [Acknowledgments](#acknowledgments)

---

## Overview
The recommender system supports two types of recommendations:
1. **Genre-based filtering** using user-selected genres and historical average ratings.
2. **Collaborative filtering** based on user rating input, leveraging both **User-Based CF (UBCF)** and **Item-Based CF (IBCF)** approaches with cosine similarity.

The application demonstrates preprocessing of the MovieLens dataset, building custom recommender models, and providing interactive recommendations in a visually appealing UI.

---

## Tech Stack
- **R** for data processing and model building
- **Shiny** for the interactive web application
- **recommenderlab** for building UBCF and IBCF models
- **Matrix**, **tidyverse**, **data.table**, **shinyjs**, **shinydashboard**, **ShinyRatingInput** for auxiliary features and enhancements

---

## Recommender Systems Implemented

### System 1: Genre-Based Filtering
If only a user's favorite genre is known, the app recommends movies using:
- **Average Ratings**
- **Year-Based Filtering** (to emphasize recent releases)
- **Trending Analysis** (using average timestamp of ratings)

### System 2: Collaborative Filtering
Users can rate movies to generate personalized recommendations using:
- **UBCF (User-Based Collaborative Filtering)**
- **IBCF (Item-Based Collaborative Filtering)**

The models are trained using the first 500 users of the dataset, and new users are predicted against this base.

---

## Key Algorithms
- Cosine similarity for similarity metrics
- Normalization of rating matrices
- Custom implementations of UBCF and IBCF for educational comparison against recommenderlab predictions
- Use of sparse matrices and realRatingMatrix objects for efficient storage and processing

---

## App Features
- Shiny Dashboard UI with collapsible panels
- Genre-based recommendations with image and rating previews
- Ratings-based predictions using custom and recommenderlab models
- Toggle between UBCF and IBCF
- Loading indicators and animated transitions
- Optionally enriched movie metadata (year, genres, average rating)

---

## Acknowledgments
- MovieLens dataset from GroupLens research
- R Shiny community for UI inspiration and libraries
- UIUC CS 598 PSL course and instructors for guidance and support

---

## License
This project is for academic use and demonstration purposes only.

---

