library(rsconnect)


error_on_missing_name <- function(name){
    var <- Sys.getenv(name, unset = NA)
    if (is.na(var)) {
        stop(paste0("CANNOT FIND", name, "!", sep = " "), call. = FALSE)
    }
    gsub("\"", "", var)
    print(var)
}

setAccountInfo(
    name = error_on_missing_name("SHINY_ACC_NAME"),
    token = error_on_missing_name("TOKEN"),
    secret = error_on_missing_name("SECRET")
)

deployApp(
  appName = error_on_missing_name("MASTERNAME"),
  appDir = c("capitalisntDashboardApp/"),
  appTitle = "Capitalisn't Dashboard",
  upload = FALSE
)
