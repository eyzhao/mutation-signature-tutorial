#' plot_snv_signature
#' 
#' @param signature_table     Data frame with columns mutation_type and proportion.
#'                            mutation_type has 96 values with format C[C>A]T for
#'                            a C>A mutation in CCT context.
#'                        
#' @param signature_name      String name of signature. Will be used as the title of plot.
#' 
#' @return ggplot object of signature plot

plot_snv_signature <- function(signature_table, signature_name = 'Signature') {
    signature_colours <- hsv(h = sort(rep(c(0, 0.3, 0.6, 0.9), 4)),
			     s = rep(c(0.1, 0.2, 0.5, 1), 4),
			     v = rep(c(0.8, 0.8, 0.8, 0.8), 4))

  p <- signature_table %>%
    mutate(
      context = gsub('(.)\\[.>.\\](.)', '\\1-\\2', mutation_type),
      base_change = gsub('.\\[(.>.)\\].', '\\1', mutation_type)
    ) %>%
    ggplot(aes(
      x = context,
      y = proportion,
      fill = context
    )) +
    facet_grid(. ~ base_change) +
    geom_bar(
      width = 0.75,
      stat = 'identity'
    ) +
    labs(
      y = 'Probability',
      title = signature_name
    ) +
    scale_fill_manual(values = signature_colours) +
    ylim(0, max(0.2, max(signature_table$proportion))) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      legend.position = 'none'
    )
  return(p)
}
