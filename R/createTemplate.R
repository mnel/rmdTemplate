#' Create template folder structure
#'
#' @description Creates template folder structure according to http://rmarkdown.rstudio.com/developer_document_templates.html.
#' This assumes you are working to create an R package
#' @param name name of the template
#' @param description a short description of the template
#' @param yamlextra list of extra details for template.yaml
#' @param titleblockextra list of extra options for title block in rmd
#' @author Michael Nelson
#' @importFrom yaml as.yaml
#' @export
createTemplate <-
  function(name = 'newTemplate',
           description = "A new template",
           yamlextra = list(),
           titleblockextra = list()) {
    # create the directories
    templateRoot <- file.path('inst','templates', name)
    templateSkeleton <- file.path(templateRoot, 'skeleton')

    # but only if the template does not already exist
    if(dir.exists(templateRoot)){
      stop("directory ", templateRoot, " already exists")
    }
    dir.create(templateSkeleton, recursive = TRUE)
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

#' Create a title block for an rmarkdown template.
#'
#' @param title title defaults to 'Untitled'
#' @param ... a list of additional options such as output
#' @return title block as a character string length 1
#' @author Michael Nelson
#' @examples
#' \dontrun{
#' # create a title block for an HTML document
#' makeTitleBlock(title = "Something interesting", output = "html_document")
#' # create a title block with some extra options
#' makeTitleBlock(title = "Something more interesting",
#'   output = list(html_document = list(toc = TRUE, fig_caption = TRUE, css = 'styles.css')))
#' }
#' @export
makeTitleBlock <- function(title = 'Untitled', ...) {
  titleBlock <- sprintf('-----\n%s------\n',
                        yaml::as.yaml(list(title = title, ...)))
}

#' Add 'inst/template' directory
#'
#' Called for side-effect only of creating 'inst/templates' directory where it
#' does not already exist.
#'
#' @param base directory of package (defaults to '.')
#' @return invisible(NULL)
#' @author Michael Nelson
addTemplate <- function(base = '.'){
  dir <- file.path(base, 'inst', 'templates')
  if(!dir.create(dir, recursive = TRUE)){
    stop("Directory: ", dir, " could not be created")
  }
  invisible(NULL)
}
