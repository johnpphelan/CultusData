interactive_timeplot <- function(data, date_col = "date") {
  # Convert to long format
  long_df <- data %>%
    pivot_longer(-all_of(date_col), names_to = "variable", values_to = "value")
  
  vars <- unique(long_df$variable)
  
  # Start empty plotly object
  p <- plot_ly()
  
  for (v in vars) {
    df <- long_df %>% filter(variable == v)
    
    # scatter
    p <- p %>%
      add_trace(data = df, x = ~get(date_col), y = ~value,
                type = 'scatter', mode = 'markers',
                name = v, visible = ifelse(v == vars[1], TRUE, FALSE))
    
    # line
    p <- p %>%
      add_trace(data = df, x = ~get(date_col), y = ~value,
                type = 'scatter', mode = 'lines',
                name = paste0(v, "_line"), visible = FALSE)
    
    # line+points
    p <- p %>%
      add_trace(data = df, x = ~get(date_col), y = ~value,
                type = 'scatter', mode = 'lines+markers',
                name = paste0(v, "_linepoints"), visible = FALSE)
    
    # bar
    p <- p %>%
      add_trace(data = df, x = ~get(date_col), y = ~value,
                type = 'bar',
                name = paste0(v, "_bar"), visible = FALSE)
  }
  
  # Layout with dropdowns
  p <- p %>%
    layout(
      updatemenus = list(
        # variable dropdown
        list(
          buttons = lapply(seq_along(vars), function(i) {
            vis <- rep(FALSE, length(vars)*4)
            vis[(4*i-3):(4*i)] <- TRUE
            list(
              method = "update",
              args = list(list(visible = vis),
                          list(title = paste("Variable:", vars[i]))),
              label = vars[i]
            )
          }),
          direction = "down",
          x = 1.1, y = 1
        ),
        # plot type dropdown
        list(
          buttons = list(
            list(method = "restyle", args = list("type", "scatter"), label = "Scatter"),
            list(method = "restyle", args = list("mode", "lines"), label = "Line"),
            list(method = "restyle", args = list("mode", "lines+markers"), label = "Line + Points"),
            list(method = "restyle", args = list("type", "bar"), label = "Bar")
          ),
          direction = "down",
          x = 1.1, y = 0.9
        )
      ),
      title = paste("Variable:", vars[1]),
      xaxis = list(title = date_col)
    )
  
  return(p)
}