#' Create template folder structure
#'
#' @description Creates template folder structure according to http://rmarkdown.rstudio.com/developer_document_templates.html.
#' This assumes you are working to create an R package
#' @param name name of the template
#' @param description a short description of the template
#' @param yamlextra list of extra details for template.yaml
#' @param titleblockextra list of extra options for title block in rmd
#' @export
create_template <- function(name ='newTemplate', description = "A new template",
                            yamlextra = list(),titleblockextra = list()){

  templateRoot <- templateDir(name)
  templateSkeleton <- skeletonDir(name)

  dir.create(templateRoot, recursive = TRUE)
  dir.create(templateSkeleton)


  template_yaml <-  yaml::as.yaml(c(list(name=name, description=description), yamlextra))

  cat(template_yaml, file = file.path(templateRoot, 'template.yaml'))

  titleBlock <- do.call("skeletonYAML", titleblockextra)
  cat(titleBlock, file = file.path(templateSkeleton,'skeleton.Rmd'))
}


templateDir <- function(name) file.path('inst','templates',name)
skeletonDir <- function(name) file.path('inst','templates',name,'skeleton')

skeletonYAML <- function(title='Untitled', ...){
  titleBlock <- sprintf('-----\n%s\n------\n',
                        yaml::as.yaml(list(title=title, ...)))
}



