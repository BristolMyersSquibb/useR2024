library(blockr)
library(blockr.ggplot2)
library(palmerpenguins)

new_geom_smooth_block <- function(y = character(), model = character(), ...) {

  all_cols <- function(data) colnames(data)

  fields <- list(
    y = new_select_field(y, all_cols, title = "y"),
    model = new_select_field(model, all_cols, title = "Model")
  )

  new_block(
    fields = fields,
    expr = quote(lm(as.formula(paste0(.(y), "~", .(model))), data = data)),
    class = c("geom_smooth_block", "transform_block"),
    ...
  )
}

stack <- new_stack(
  data = new_dataset_block(selected = "penguins", package = "palmerpenguins"),
  smooth = new_geom_smooth_block(y = "bill_length_mm", model = "body_mass_g")
)

serve_stack(stack)
