# define function to help apply custom css
#  to htmlwidgets using css specificity with id
style_widget <- function(hw=NULL, style="", addl_selector="") {
  stopifnot(!is.null(hw), inherits(hw, "htmlwidget"))

  # use current id of htmlwidget if already specified
  elementId <- hw$elementId
  if(is.null(elementId)) {
    # borrow htmlwidgets unique id creator
    elementId <- sprintf(
      'htmlwidget-%s',
      htmlwidgets:::createWidgetId()
    )
    hw$elementId <- elementId
  }

  htmlwidgets::prependContent(
    hw,
    htmltools::tags$style(
      sprintf(
        "#%s %s {%s}",
        elementId,
        addl_selector,
        style
      )
    )
  )
}
