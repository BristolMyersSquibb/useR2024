library(blockr)

plot_histogram <- function(
  data, 
  column,
  breaks = 20, 
  obs = FALSE, 
  density = FALSE, 
  bw_adjust = 1,
  color = "blue"
){
  values <- data[[column]]
  hist(
    values,
    probability = TRUE,
    breaks = breaks
  )

  if (obs) {
    rug(values)
  }

  if (density) {
    dens <- density(
      values,
      adjust = bw_adjust
    )

    lines(dens, col = color)
  }
}

new_histogram_block <- function(...){
  first_col <- \(data) data |> dplyr::select_if(is.numeric) |> colnames() |> (\(x) x[1])()
  columns <- \(data) data |> dplyr::select_if(is.numeric) |> colnames()

  new_block(
    name = "Histogram",
    expr = quote(plot_histogram(.(column))),
    class = c("histogram_block", "plot_block"),
    fields = list(
      column = new_select_field(first_col, columns)
    ),
    ...
  )
}

stack <- new_stack(
  new_dataset_block,
  new_histogram_block
)

serve_stack(stack)
