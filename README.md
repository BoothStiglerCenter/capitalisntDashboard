# capitalisntDashboard

## About
This repo contains all of the files necessary to generate and run the *Capitalisn't* performance-tracking dashboard. This app should outlive me (Joshua) and should be modifiable for future investigations.


## How to Use
The broad idea of this app is that the workflow works in three parts. The first collects data using the Simplecast API, and the second presents that data in an interactive format using Shiny. That Shiny app is then deployed to and hosted by shiny.io.


## File Structure
```
├── capitalisntDashboardApp
│   ├── about_panel.md
│   ├── global.R
│   ├── server.R
│   ├── ui.R
├── apiCollect.py
├── README.md
├── .Renviron
└── .gitignore
```


### 1. Collection

The Simplecast API is quite comprehensive and can return data for a number of our indicators of interest. Collection is done by a Python 3.8 script: `apiCollect.py` Documentation for that API can be found [here](https://apidocs.simplecast.com/#intro). **N.B.** This API does require a generated token. This API is used to collect:
- a "core" database of episodes;
  - This contains an episode's publication date, its unique ID, the number of downloads at time of API call and its season-episode number.
  - !!! This is a critical database. This database is invoked (or, if it cannot be found is re-generated) by every other function to collected episode IDs that are used to query other API endpoints.
  - This info is collected from the `analytics/episodes` API endpoint
- daily downloads data;
  - This contains daily-resolution data for every episode. For each day-episode obervation, this contains total downloads in that period (day), the share of that episode's historical downloads that day makes up, the episodes ID, and title. 
  - This info is collected from the `analytics/downloads` API endpoint.
- keywords data;
  - This contains the "keywords" that are assigned to each episode and episodes' IDs.
    - These keywords are used by other services to filter/categorized *Capitalisn't* in various curated lists/to algorithmically suggest the podcast to others.
    - This is a "long" dataset with episode-keyword level observations.
    - This info is collected from the `episodes/:episode_id/keywords` API endpoint
- listening-methods data*†;
  - This contains the various platforms listeners use to listen to the podcast and episode IDs. Observations are at the episode-platform level. 
  - This info is collected from the `analytics/technology/listening_methods` API endpoint.
- device-class data*†;
  - This contains the various devices listeners use to listen to the podcast and episode IDs. Observations are at the episode-device-class level.
  - This info is collected from the `analytics/technology/device_class` API endpoint.
- listener-location data*†;
  - This contains the geographic locations where listeners download the episode. This data is available at the nation-level for all countries and the state- and city-levels for the United States.
  - This info is collected from the `analytics/location` API endpoint.

\* indicates that this data is available at the podcast- and episode-level. 

† indicates that the data returned by the API is at-time-of-call cross-sectional data. That is, it returns only the most up-to-date over-time aggregate for the variable of interest. Thus, to generate a time-series of these variables, the API must called at regular intervals. At present, we do this only for location data as we are less interested in how the device-class and listening method data evolves over time.

Only the listener-location data is formatted as "wide" data. This is because this is far and away the largest data set. 


### 2. Presentation

After data collection is complete, all data processing and presentation is done in R and Shiny. The files that make up the app are described below:

- `global.R`
  - All data reading and most standardized processing is done here.
  - Variables defined in this file are accessible in all files but any changes are not reflected in "hot-reloading" functions. That is, if changes are made in this file, the Shiny app must be completely re-started for the changes to be properly reflected in functionality.
- `server.R`
  - This files defines most of the major plots/figures/tables etc.
  - This file takes advantage of the generic data-reading and pre-processing done in `global.R` and modifies those dataframes for specific uses/figures/tables etc.
- `ui.R`
  - This file defines the layout of the dashboard.
  - It invokes many of the variables/objects defined in `server.R`



### 3. Hosting


## Contact

