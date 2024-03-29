% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_tag_functions.R
\name{gen_tag_functions}
\alias{gen_tag_functions}
\title{Function generator to simplify generation of \code{XiMpLe_node} objects}
\usage{
gen_tag_functions(
  tags,
  func_names = paste0(tags, "_"),
  envir = .GlobalEnv,
  replace = FALSE,
  func_rename = c(`?xml_` = "xml_", `!--_` = "comment_", `![CDATA[_` = "CDATA_",
    `!DOCTYPE_` = "DOCTYPE_")
)
}
\arguments{
\item{tags}{A character vector defining the tags the generated functions should produce.}

\item{func_names}{A character vector the same length as \code{tags}, defining the names of
the functions to generate.}

\item{envir}{The environment where all generated functions should appear.}

\item{replace}{Logical, whether objects by the same name already present in \code{envir}
should be preserved or replaced/overwritten.}

\item{func_rename}{Named character vector defining which tags' functions should get a different name.
This makes it easier to get functions with valid names that generate special tag nodes.}
}
\value{
As many functions as specified by \code{tags}/\code{func_names}.
}
\description{
Takes a vector of character strings and turns them into functions in the defined environment
which in turn will generate \code{\link[XiMpLe:XiMpLe_node]{XiMpLe_node}} objects with the
string values as tag names.
}
\details{
The generated functions will be named according to \code{func_names} and only have a dots
argument that is given to \code{\link[XiMpLe:XMLNode]{XMLNode}}. See the examples to
understand how it's supposed to work.
}
\examples{
# Say we would like to generate an HTML website and want to use
# <a>, <div> and <p> tags.
# The standard way of creating a <div> node would be this:
(my_node <- XMLNode("div", "some content", class="important"))

# By using gen_tag_functions(), we can create some shortcut functions
# to get better readability for our code and save some typing:
gen_tag_functions(tags=c("a", "div", "p"))
# We can now use div_() instead of XMLNode("div"):
(my_node2 <- div_("some content", class="important"))

# It also works for nested tags:
(my_node3 <- div_(a_(href="foo", "some content")))

# If you don't want these functions filling up your .GlobalEnv,
# you can also put them in an attached environment, e.g.
attach(list(), name="XiMpLe_wrappers")
gen_tag_functions(tags=c("head", "body"), envir=as.environment("XiMpLe_wrappers"))
}
\seealso{
\code{\link[XiMpLe:XMLNode]{XMLNode}},
}
