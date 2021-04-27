##' Download Reported Diversion Data from WRUDS link
##' in eWRIMS
##' 
##' If save_file = TRUE, will save a date-stamped copy of downloaded report
##' to wruds_downloads. This directory is created if it doesn't exist
##' in project home directory
##'
##'Function returns file path to downloaded reporting compliance file.

getReporteDivsCSV <- function(cred_file = "credentials.csv",
                              save_file = TRUE) {
    
    ## Initialization. ----
    
    # Load required packages if not already loaded.
    if (!("package:dplyr" %in% search())) {
        suppressMessages(library(dplyr))
    }
    if (!("package:readr" %in% search())) {
        suppressMessages(library(readr))
    }
    if (!("package:openxlsx" %in% search())) {
        suppressMessages(library(openxlsx))
    }
    if (!("package:stringr" %in% search())) {
        suppressMessages(library(stringr))
    }
    if (!("package:RSelenium" %in% search())) {
        suppressMessages(library(RSelenium))
    }
    if (!("package:netstat" %in% search())) {
        suppressMessages(library(netstat))
    }
    
    # Load login credentials from file.
    creds <- read_csv(cred_file)
    
    # Define Chrome options.
    eCaps <- list(
        chromeOptions =
            list(prefs = list("profile.default_content_settings.popups" = 0L,
                              "download.prompt_for_download" = FALSE,
                              "directory_upgrade" = TRUE,
                              "download.default_directory" = normalizePath(getwd())))
    )
    
    # Create Selenium server.
    driver <- rsDriver(port = free_port(),
                       browser= "chrome",
                       chromever = "90.0.4430.24",
                       extraCapabilities = eCaps)
    remDr <- driver[["client"]]
    
    # Navigate to CIWQS Login page.
    remDr$navigate("https://ciwqs.waterboards.ca.gov/ciwqs/index.jsp")
    
    # Enter login credentials.
    username <- remDr$findElement(using = "name", value = "accountname")
    username$sendKeysToElement(list(creds$u_name))
    passwd <- remDr$findElement(using = "name", value = "password")
    passwd$sendKeysToElement(list(creds$p_word, "\uE007"))
    
    # Navigate to WRUDS Data Download page.
    remDr$navigate("https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/wruds_data_download.jsp")
    
    # Select Diversion Amounts from dropdown.
    dd_string <- paste0("//*/option[@value = '", "DIV","']")
    option <- remDr$findElement(using = "xpath", dd_string)
    option$clickElement()
    Sys.sleep(3)
    
    # Download the data file.
    download <- remDr$findElement("xpath", '//input[@type="submit"]')
    download$clickElement()
    Sys.sleep(240)
    
    # Close connection to Selenium server.
    remDr$close()
    driver$server$stop()
    driver$server$process
    
    # Save downloaded file if flagged to do so.
    if(save_file){
        
        # Create "compliance_reports" directory if it doesn't exist.
        new_dir <- "./wruds_downloads/"
        if(!dir.exists(new_dir)) dir.create(new_dir)
        
        pull_date <- as.Date(file.info("wruds_diversion_data.csv")[[4]])
        old_file_path <- "./wruds_diversion_data.csv"
        new_file_path <- paste0(new_dir,
                                "reported_diversions_",
                                pull_date,
                                ".csv")
        file.rename(from = old_file_path,
                    to = new_file_path)
        
        # Return path to saved file.
        return(new_file_path)
    }
    
    
}
