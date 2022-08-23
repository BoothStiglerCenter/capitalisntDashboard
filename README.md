# capitalisntDashboard

## About
This repo contains all of the files necessary to generate and run the *Capitalisn't* performance-tracking dashboard. This app should outlive me (Joshua) and should be modifiable for future investigations.


## How It Works
The broad idea of this app is that the workflow works in three parts. The first collects data using the Simplecast API, and the second presents that data in an interactive format using Shiny. That Shiny app is then deployed to and hosted by shiny.io.


## File Structure
```
├── capitalisntDashboardApp
│   ├── about_panel.md
│   ├── global.R
│   ├── server.R
│   ├── ui.R
│   ├── drop_token_rds_ENCRYPTED.rds
├── apiCollect.py
├── localDevTools.R
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
  - The deployed version of this file also handles decrypting a DropBox OAuth 2.0 authorization token.
- `server.R`
  - This files defines most of the major plots/figures/tables etc.
  - This file takes advantage of the generic data-reading and pre-processing done in `global.R` and modifies those dataframes for specific uses/figures/tables etc.
- `ui.R`
  - This file defines the layout of the dashboard.
  - It invokes many of the variables/objects defined in `server.R`



### 3. Hosting
At present the process of hosting is complicated because we are trying to do everything for free. The app itself is hosted in Shiny with Joshua's personal shiny account. This can be made an institutional account trivially.

The deploying and hosting process is multi-step to ensure that no secret API or OAuth tokens are exposed *and* to allow for features to be continuously added to the dashboard by any number of developers. 

**shinyapps.io**
- shinyapps.io is the platform that hosts the Shiny App itself. Because we are using a free tier for hosting. Part of using the free tier of hosting is that there is limited up-time per month (1500 minutes/month). Additionally, each instance of the app runs with limited system resources (1GB of RAM and storage?). This means that most file storage and process should take place off-Shiny.
- 

**DropBox**
- In order to streamline the Shiny process as much as possible (and syncing data collection with live presentation) files/data are collected on a local instance and then placed in a specific DropBox folder. 
- This data-collection pipeline requires that the ShinyApp instance has an authenticated token to access to the DropBox folder. Since 2021, DropBox has only issued short-lifespan tokens that expire 4 hours after issuance. In order to get around this, we customize a function from `rdrop2`, a package to programatically access DropBox from R: (`drop_auth()`). The customized function is available in `localDevTools.R` and is called `custom_drop_auth()`. This function adds an extra parameter to the original function that requests a token that can auto-refresh itself. Authenticating this token requires human interaction/accessing a browser upon calling `custom_drop_auth()` so must be done locally. This token must then be passed to the Shiny instance.
- This token allows anyone with access to it to have access to the Stigler center DropBox. Consequently, ensuring that other people on the internet cannot access it is very important. When `custom_drop_auth()` generates a token, it caches a version of the token in a hidden file called `.httr-oauth`. This file must be .gitignored by new developers. An explicit version of the token is encrypted and then saved by `localDevTools.R`. The encryption process is handled by a local passphrase that is also saved as a secret in GitHub actions. The encrypted token is then uploaded to GitHub so that it can be repackaged with every re-deployment of the app to Shiny.
- All data is currently being stored in Joshua's Stigler Center Dropbox folder. For others to start running this collection an deployment pipeline, they will need to modify their `env_keys.json` files as `apiCollect.py` looks at that file to identify the DropBox destination folder to copy up-to-date data into.

**GitHub Actions**
- When a new feature branch is pulled into the liveProduction branch a GitHub action fires, executing a Docker file that deploys those new features/versions to the live ShinyApp.
- The DockerFile is responsible for running a script that automatically deploys the contents of the updated liveProduction branch to shinyapps.io. It uses the `deployApp.R` script which itself reads various GitHub secrets to connect to, and then securely deploy the new app files to shinyapps.io
- **IMPORTANT!!!** At present this file is responsible for token en/decryption. This process will eventually be moved to global.R
- When a new version of the app is deploy to shinyapps.io, shinyapps.io clears the cache and removes the secret DropBox authentication token. That DropBox authentication token (now encrypted and saved inside the GitHub repo) must be deployed with the new version of dashboard.
- In order to minimize the chance that this token is ever exposed to the internet, the .rds file is only decrypted on the shinyapps.io server/app instance. The symmetric en/decryption passphrase is stored as a GitHub secret and is passed to shinyapps.io as an environment variable. It is of critical importance that this passphrase is never uploaded to a website in an unecrypted fashion.


## Contact

