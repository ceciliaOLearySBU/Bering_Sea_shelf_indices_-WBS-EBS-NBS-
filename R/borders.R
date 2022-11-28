borders <- function (database = "world", regions = ".", fill = NA, 
          colour = "grey50", xlim = NULL, ylim = NULL, ...) 
{
  df <- map_data(database, regions = c("USA","Russia"), xlim = xlim, ylim = ylim, exact = FALSE)
  df$long <- ifelse(df$long > 0, df$long, df$long + 360)
  geom_polygon(aes_(~long, ~lat, group = ~group), data = df, 
               fill = fill, colour = colour, ..., inherit.aes = FALSE)
}
