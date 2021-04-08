makePlot <- function(i, plot_data, plot_variables, y_max) {
    # Create demand color palette:
    plot_variables <- ordered(plot_variables, levels = plot_variables)
    my_colors <- c("#F0E442", "#E69f00", "#D55E00", "steelblue1",
                   "blue", "green", "orange", "red")
    names(my_colors) <- levels(plot_variables)

    g <- ggplot(plot_data, aes(x = date, y = value))

    # Define axis formatting:
    g <- g + scale_y_continuous(labels = comma,
                                breaks= seq(0, y_max, y_max / 10))
    g <- g + scale_x_date(date_breaks = "1 month", date_labels = "%m/%d/%y",
                          minor_breaks = NULL)

    # Set plot area:
    g <- g + coord_cartesian(xlim = c(as.Date("2016-03-01"),
                                      as.Date("2016-09-30")),
                             ylim = c(0, y_max))

    # Demand data:
    g <- g + geom_area(data = subset(plot_data, variable %in%
                                         c("Riparian", "Pre-14",
                                           "Appropriative") & area == i),
                       position = "stack",
                       aes(fill = variable))
    # Daily FNF Line
    # g <- g + geom_line(data = subset(plot_data,
    #                                  area == i &
    #                                      variable == "daily_fnf"),
    #                    aes(color = variable))
    # # Current Monthly FNF:
    # g <- g + geom_point(data = subset(plot_data, area == i &
    #                                       variable == "monthly_fnf"),
    #                     aes(color = variable), size = 4, shape = 18)
    # g <- g + geom_line(data = subset(plot_data, area == i &
    #                                      variable == "monthly_fnf"),
    #                    aes(color = variable), linetype = 2, alpha = 0.3)
    # FNF Forecast Points:
    # g <- g + geom_point(data = subset(plot_data,
    #                                   variable %in% c("16_05_fnf.50",
    #                                                   "16_05_fnf.90",
    #                                                   "16_05_fnf.99") &
    #                                       area == i),
    #                     aes(color = variable), size = 4)
    # g <- g + geom_line(data = subset(plot_data,
    #                                  variable %in% c("16_05_fnf.50",
    #                                                  "16_05_fnf.90",
    #                                                  "16_05_fnf.99") &
    #                                      area == i),
    #                    aes(color = variable),
    #                    linetype = 2,
    #                    alpha = 0.3)
    # Demand Legend:
    # g <- g + scale_fill_manual(name="Demand",
    #                            breaks=c("Riparian",
    #                                     "Pre-14",
    #                                     "Appropriative"),
    #                            labels=c("Riparian Demand",
    #                                     "Pre-1914 Demand",
    #                                     "Post-1914 Demand"),
    #                            values=my_colors)
    # # Legend:
    # g <- g + scale_color_manual(name="Supply",
    #                             breaks = c("daily_fnf",
    #                                        "monthly_fnf",
    #                                        "16_05_fnf.50",
    #                                        "16_05_fnf.90",
    #                                        "16_05_fnf.99"),
    #                             labels=c("Daily Full Natural Flow (FNF)",
    #                                      "Water Year 2016 Monthly FNF",
    #                                      "Adjusted 50% Exceedance Monthly FNF Forecast",
    #                                      "Adjusted 90% Exceedance Monthly FNF Forecast",
    #                                      "Adjusted 99% Exceedance Monthly FNF Forecast"),
    #                             values = my_colors)
    # g <- g + guides(color = guide_legend(order = 1, override.aes =
    #                                          list(shape=c(NA, 18, 16, 16, 16),
    #                                               linetype=c(1, 0, 0, 0, 0))),
    #                 shape = guide_legend(order = 2))
    # g <- g + theme(legend.justification=c(1,1),
    #                legend.position=c(1,1),
    #                legend.box.just = "right")
    # # Axis Labels:
    # g <- g + labs(x = "", y = "Time-Averaged Flow in Cubic Feet per Second (cfs)")

    return(g)
}
