#' Create template folder structure
#'
#' @description Creates template folder structure according to http://rmarkdown.rstudio.com/developer_document_templates.html.
#' This assumes you are working to create an R package
#' @param name name of the template
#' @param description a short description of the template
#' @param yamlextra list of extra details for template.yaml
#' @param titleblockextra list of extra options for title block in rmd
#' @importFrom yaml as.yaml
#' @export
createTemplate <-
  function(name = 'newTemplate',
           description = "A new template",
           yamlextra = list(),
           titleblockextra = list()) {
    # create the directories
    templateRoot <- templateDir(name)
    templateSkeleton <- file.path(templateRoot, 'skeleton')
    dir.create(templateRoot, recursive = TRUE)
    dir.create(templateSkeleton)
    # template.yaml
    template_yaml <-
      yaml::as.yaml(c(list(
        name = name, description = description
      ), yamlextra))
    cat(template_yaml, file = file.path(templateRoot, 'template.yaml'))
    #skeleton.Rmd
    templateTitleBlock <- do.call("makeTitleBlock", titleblockextra)
    cat(templateTitleBlock,
        file = file.path(templateSkeleton, 'skeleton.Rmd'))
  }


#' A simple function to create a title block for an rmarkdown template.
#'
#' @param title title defaults to 'Untitled'
#' @export
makeTitleBlock <- function(title = 'Untitled', ...) {
  titleBlock <- sprintf('-----\n%s\n------\n',
                        yaml::as.yaml(list(title = title, ...)))
}
