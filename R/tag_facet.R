tag_facet <- function(p, open = "(", close = ")",
    tag_pool = letters, I_start = 1,
    x = -Inf, y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, family = "",
    ...)
{
    gb <- ggplot_build(p)
    lay <- gb$layout$layout
    ind = as.numeric(lay$PANEL) + I_start - 1
    # I_start - 1
    tags <- cbind(lay, label = paste0(open, tag_pool[ind],
        close), x = x, y = y)
    p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"),
        ..., hjust = hjust, vjust = vjust, fontface = fontface,
        family = family, inherit.aes = FALSE)
        # theme(strip.text = element_blank(),
        # strip.background = element_blank())
}
