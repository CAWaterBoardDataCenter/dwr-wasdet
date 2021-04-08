##' 2019 Diversion and Use Reporting Compliance Plot
##' 
##' After downloading the compliance report from eWRIMS, prepend the csv file
##' name with the reporting year followed by and underscore, and append it with 
##' and undescore then the date you pulled the report in a YYYMMDD format. The 
##' file name of a reporting compliance report for 2016 pulled on August 8, 2018,
##' would look like:
##' 
##'           2016_rms_reporting_compliance_20180808.csv
##'           
##'           

# Load packages ----
if(!("package:tibble" %in% search())) {
  suppressMessages(library(tibble))
}
if(!("package:tidyr" %in% search())) {
  suppressMessages(library(tidyr))
}
if(!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
if(!("package:readr" %in% search())) {
  suppressMessages(library(readr))
}
if(!("package:purrr" %in% search())) {
  suppressMessages(library(purrr))
}
if(!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if(!("package:gridExtra" %in% search())) {
  suppressMessages(library(gridExtra))
}
if(!("package:gtable" %in% search())) {
  suppressMessages(library(gtable))
}
if(!("package:grid" %in% search())) {
  suppressMessages(library(grid))
}
if(!("package:stringr" %in% search())) {
  suppressMessages(library(stringr))
}

## User-defined functions and variables. ----

source("f_fillNAgaps.R")

make_pdf <- TRUE

# Load Reporting Compliance data file.

data_files <- file.info(list.files("./compliance_reports", full.names = T))
source_file <- rownames(data_files)[which.max(data_files$mtime)]

# source_file <- "./compliance_reports/2019_rms_reporting_compliance_20200317.zip"

reporting <- 
  read_csv(source_file,
           col_types = cols(DATE_REPORT_FILED = col_date(),
                            REGISTRATION_RENEWAL_DATE = col_date(),
                            GROUNDWATER_FEE_PAID = col_character(),
                            GROUNDWATER_ACTION_REQUESTED = col_character(),
                            STATEMENTS_FILED_COUNT = col_integer(),
                            CEASED_DIVERSION = col_character()))

# Get Reporting Year and data pull date from file name.

reporting_year <- as.integer(str_extract(source_file, "\\d{4}"))
data_pull_date <- as.Date(str_extract(source_file, "\\d{8}"), format = "%Y%m%d")

# Remove unneeded columns, filter for active rights.

reporting <- reporting %>%
  select(wr_id = APPLICATION_NUMBER, 
         wr_type = WATER_RIGHT_TYPE, 
         wr_status = WATER_RIGHT_STATUS,
         owner = PRIMARY_OWNER_NAME,
         due_date = REPORT_DUE_DATE,
         file_date = DATE_REPORT_FILED) %>% 
  filter(wr_status %in% c("Licensed",
                          "Permitted",
                          "Certified",
                          "Registered",
                          "Claimed"))
last_submit_date <- max(reporting$file_date, na.rm = TRUE)

# Add missing due dates to table.

reporting <- reporting %>%
  mutate(due_date = if_else(is.na(due_date), 
                            if_else(wr_status == "Claimed", 
                                    as.Date("2020-07-01"), 
                                    as.Date("2020-04-01")), 
                            due_date)) %>% 
  arrange(file_date)

# Create vectors of water right types by due date.

april_rights <- reporting %>% 
  filter(due_date == as.Date("2019-04-01")) %>% 
  select(wr_type) %>% 
  distinct() %>% 
  deframe()
july_rights <- reporting %>%
  select(wr_type) %>% 
  filter(!wr_type %in% april_rights) %>% 
  distinct() %>% 
  deframe()

# Total count of each water right type and count by due date.

wr_type_counts <- reporting %>% 
  group_by(wr_type) %>% 
  summarize(type_count = format(n(), big.mark = ","))
due_date_counts <- reporting %>% 
  group_by(due_date) %>% 
  summarize(type_count = format(n(), big.mark = ","))

## Calculate Cumulative compliance rates by water right type. ----

wr_type_cum <- reporting %>% 
  group_by(file_date, wr_type) %>% 
  summarize(report_count = n()) %>% 
  ungroup()
wr_type_cum <- wr_type_cum %>% 
  group_by(wr_type) %>% 
  mutate(cum_report = cumsum(report_count),
         rept_pct = 100 * (cum_report / sum(report_count))) %>% 
  ungroup()

# Current compliance rates by water right type.

wr_type_cr <- wr_type_cum %>% 
  filter(!is.na(file_date)) %>% 
  group_by(wr_type) %>% 
  summarize(comp_rate = round(max(rept_pct), digits = 1))

# Generate data frame for plotting.

wr_type_cum <- wr_type_cum %>% 
  select(file_date, wr_type, rept_pct) %>% 
  pivot_wider(names_from = wr_type, values_from = rept_pct)
wr_type_cum <- wr_type_cum %>% 
  mutate_at(.vars = vars(-file_date), .funs = fillNAgaps)
wr_type_cum <- wr_type_cum %>% 
  pivot_longer(names_to = "wr_type", 
               values_to = "rept_pct", 
               values_drop_na = TRUE,
               -file_date)

# Update legend entries with count and current reporting percentage data.

wr_type_cum <- reduce(list(wr_type_cum, 
                           wr_type_cr, 
                           wr_type_counts), 
                      left_join, by = "wr_type")
wr_type_cum <- wr_type_cum %>%
  mutate(due_date = ifelse(wr_type %in% april_rights, "4/1/2020", "7/1/2020"))
wr_type_cum <- wr_type_cum %>% 
  mutate(wr_type = paste0(wr_type, " due ", due_date, " (", 
                          comp_rate, "% of ", 
                          type_count, " reports submitted)"),
         line_group = "water right type") %>% 
  rename(line_group_name = wr_type) %>% 
  select(-due_date)

## Calculate Cumulative compliance rates by due date. ----

due_date_cum <- reporting %>% 
  arrange(file_date) %>%
  group_by(file_date, due_date) %>% 
  summarize(report_count = n())
due_date_cum <- due_date_cum %>% 
  group_by(due_date) %>% 
  mutate(cum_report = cumsum(report_count),
         rept_pct = 100 * (cum_report / sum(report_count))) %>% 
  ungroup()

# Current compliance rates by water right type.

due_date_cr <- due_date_cum %>% 
  filter(!is.na(file_date)) %>% 
  group_by(due_date) %>% 
  summarize(comp_rate = round(max(rept_pct), digits = 1))

# Generate data frame for plotting.

due_date_cum <- due_date_cum %>% 
  select(file_date, due_date, rept_pct) %>% 
  pivot_wider(names_from = due_date, values_from = rept_pct)
due_date_cum <- due_date_cum %>% 
  mutate_at(.vars = vars(-file_date), .funs = fillNAgaps) 
due_date_cum <- due_date_cum %>% 
  pivot_longer(names_to = "due_date",
               values_to = "rept_pct",
               cols = -file_date,
               values_drop_na = TRUE)
due_date_cum$due_date <- as.Date(due_date_cum$due_date)

# Update legend entries with count and current reporting percentage data.

due_date_cum <- reduce(list(due_date_cum, 
                            due_date_cr, 
                            due_date_counts), 
                       left_join, by = "due_date")
due_date_cum <- due_date_cum %>% 
  mutate(due_date = paste0(format(due_date, "%B %d, %Y"), 
                           " (", comp_rate, "% of ", 
                           type_count, " reports submitted)"),
         line_group = "due date") %>% 
  rename(line_group_name = due_date)

# Combine the two data frames for plotting.

plot_data <- rbind(wr_type_cum, due_date_cum)
plot_data <- plot_data %>% 
  arrange(desc(line_group), line_group_name, file_date)
plot_data$line_group_name <- ordered(plot_data$line_group_name, 
                                     levels = unique(plot_data$line_group_name))

## Build the plot.----

# Define plot color palette.

pal1 <- c( "red", "blue")
pal2 <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
          "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD")

#group_breaks <- unique(plot_data$line_group_name)

# Create dummy plots to extract single legends for due_date and wr_type.

p1 <- ggplot(data = subset(plot_data, line_group == "due date"),
             aes(x = file_date, y = rept_pct, color = line_group_name)) +
  geom_line(size = 2) +
  scale_color_manual(name = "Due Date", values = pal1) +
  theme_minimal()
leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")

p2 <- ggplot(data = subset(plot_data, line_group == "water right type"),
             aes(x = file_date, y = rept_pct, color = line_group_name)) +
  geom_line(size = 1) +
  scale_color_manual(name = "Water Right Type", values = pal2) +
  theme_minimal()
leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p2)), "guide-box")

leg3 <- nullGrob()

# Set the width ofthe narrow legend to the widest legend to left-align them.
# 
# leg1$widths <- leg2$widths
# leg3$widths <- leg2$widths

# Create plot. 

g <- ggplot(data = plot_data, aes(x = file_date, y = rept_pct, 
                                  color = line_group_name,
                                  size = line_group_name,
                                  alpha = line_group_name))
g <- g + geom_line()
g <- g + scale_color_manual(values = c(pal2, pal1))
g <- g + scale_size_manual(values = c(rep(1, 10), rep(2, 2)))
g <- g + scale_alpha_manual(values = c(rep(0.8, 10), rep(1.0, 2)))
g <- g + scale_x_date(date_breaks = "2 months",
                      date_minor_breaks = "1 month",
                      date_labels = "%m/%d/%Y")
g <- g + scale_y_continuous(breaks = seq(0, 100, 10),
                            limits = c(0, 100))
g <- g + labs(x = "Date", 
              y = "Report Submittal Compliance Rate (Percent)",
              title = paste0(reporting_year, " Annual Diversion and Use Reports\n",
                             "Report Submittal Compliance Rates Over Time", 
                             " by Water Right Type and Due Date Through ",
                             format(last_submit_date, "%B %d, %Y")),
              caption = paste0("Reporting compliance data retrieved from eWRIMS on ", 
                               format(data_pull_date, "%B %d, %Y")))

## Add vertical lines to indicate milestones. ----

# Appropriative Reports Due.

g <- g + geom_vline(aes(xintercept = as.Date("2020-04-01")),
                    color = "red",
                    linetype = "dashed",
                    size = 0.5)
g <- g + annotate("text",
                  x = as.Date("2020-04-03"),
                  y = 100,
                  label = "Appropriative Reports Due",
                  size = 3,
                  color = "black",
                  angle = -90,
                  hjust = 0)

# Statement Reports Due.

g <- g + geom_vline(aes(xintercept = as.Date("2020-07-01")),
                    color = "red",
                    linetype = "dashed",
                    size = 0.5)
g <- g + annotate("text",
                  x = as.Date("2020-07-03"),
                  y = 100,
                  label = "Statement Reports Due",
                  size = 3,
                  color = "black",
                  angle = -90,
                  hjust = 0)

# Initial Reminder letters sent.

g <- g + geom_vline(aes(xintercept = as.Date("2020-02-21")),
                    color = "orange",
                    linetype = "dashed",
                    size = 0.5)
g <- g + annotate("text",
                  x = as.Date("2020-02-19"),
                  y = 100,
                  label = "Initial Reminder Letters Mailed",
                  size = 3,
                  color = "black",
                  angle = -90,
                  hjust = 0)

# # Postcards to permits and licenses that have not filed.
# 
# g <- g + geom_vline(aes(xintercept = as.Date("2019-04-22")),
#                     color = "orange",
#                     linetype = "dashed",
#                     size = 0.5)
# g <- g + annotate("text",
#                   x = as.Date("2019-04-24"),
#                   y = 100,
#                   label = "Postcards to Permits & Licenses",
#                   size = 3,
#                   color = "black",
#                   angle = -90,
#                   hjust = 0)
# g <- g + theme_minimal() + theme(legend.position = "none")
# 
# # Lyris reminder to appropriative water right holders.
# 
# g <- g + geom_vline(aes(xintercept = as.Date("2019-05-10")),
#                     color = "orange",
#                     linetype = "dashed",
#                     size = 0.5)
# g <- g + annotate("text",
#                   x = as.Date("2019-05-12"),
#                   y = 100,
#                   label = "Lyris Reminder to Approp. Right Holders",
#                   size = 3,
#                   color = "black",
#                   angle = -90,
#                   hjust = 0)
# g <- g + theme_minimal() + theme(legend.position = "none")
# 
# # Final Warning letters to appropriative water right holders, face_value > 10 AF.
# 
# g <- g + geom_vline(aes(xintercept = as.Date("2019-06-12")),
#                     color = "orange",
#                     linetype = "dashed",
#                     size = 0.5)
# g <- g + annotate("text",
#                   x = as.Date("2019-06-14"),
#                   y = 100,
#                   label = "Final Warning Letter to Approp. Right Holders",
#                   size = 3,
#                   color = "black",
#                   angle = -90,
#                   hjust = 0)

g <- g + theme_minimal() + theme(legend.position = "none")


# Arrange plot parts.

plot <- arrangeGrob(leg3, leg1, leg2, ncol = 1, heights = c(2,1,2))
plot <- arrangeGrob(g, plot, ncol = 2, widths = c(4,2))
grid.newpage()
grid.draw(plot)

if (make_pdf) {
  f_name <- paste0("./pdfs/",
                   "2018 Reporting Compliance Plot Combo ",
                   format(Sys.Date(), "%Y-%m-%d"),
                   ".pdf")
  pdf(f_name, width = 15, height = 9)
  grid.draw(plot)
  dev.off()
  f_name <- paste0("./pdfs/",
                   "2018 Reporting Compliance Plot Combo ",
                   format(Sys.Date(), "%Y-%m-%d"),
                   ".png")
  png(f_name, width = 1500, height = 900)
  grid.draw(plot)
  dev.off()
}
