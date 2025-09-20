plot_bar <- function(select_taxon, df, ...) {
    # filter to only include selected species
    plot_data <- df %>% filter(taxon == select_taxon)
    ggplot() +
        # bar chart coloured by sex and whether lemurs are currently at DLC
        geom_col(
            data = plot_data,
            mapping = aes(
                x = sex,
                y = n,
                fill = sex,
                alpha = current),
            width = 1) +
        # add label with total number of males and females
        geom_text(
            data = plot_data,
            mapping = aes(
                x = sex,
                y = n_tot,
                label = n_tot),
            hjust = -0.3,
            colour = "black",
            size = 8) +
        # remove labels and set colour choices
        labs(x = "", y = "") +
        scale_fill_manual(values = c("#a1b70d", "#8a8d8f")) +
        scale_alpha_manual(values = c(0.5, 1)) +
        # fix limits to be the same for all plots (across all species)
        scale_y_continuous(limits = c(0, plyr::round_any(max(df$n_tot), 50, ceiling))) +
        coord_flip() +
        ggplot2::theme_void() +
        theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            plot.margin = margin(5.5, 15, 5.5, 5.5))
}


plot_line <- function(select_taxon, df, ...) {
    # filter to only chosen taxonomy code
    plot_data <- df %>% filter(taxon == select_taxon)

    ggplot(
        data = plot_data,
        mapping = aes(
            x = age_group,
            y = avg_wt,
            colour = sex,
            group = sex)) +
        # draw semi-transparent line with points on top
        geom_line(linewidth = 1.2, alpha = .5) +
        geom_point(size = 2, alpha = .5) +
        # labels
        labs(x = "", y = "Weight (g)") +
        # set colours and axis break points
        scale_colour_manual(values = c("#a1b70d", "#8a8d8f")) +
        scale_x_discrete(
            breaks = c("(96,120]", "(216,240]", "(336,360]"),
            labels = c("10 years", "20 years", "30 years"),
            drop = FALSE) +
        scale_y_continuous(
            limits = c(0, plyr::round_any(max(df$avg_wt), 50, ceiling)),
            breaks = c(0, 250, 500)) +
        ggplot2::theme_minimal() +
        theme(
            legend.position = "none",
            panel.grid = element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            axis.title.y = element_text(size = 10)
        )
}
