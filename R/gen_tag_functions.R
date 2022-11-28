# Copyright 2022 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package XiMpLe.
#
# XiMpLe is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# XiMpLe is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with XiMpLe.  If not, see <http://www.gnu.org/licenses/>.


#' Function generator to simplify generation of XiMpLe.node objects
#' 
#' Takes a vector of character strings and turns them into functions in the defined environment
#' which in turn will generate XiMpLe.node objects with the string values as tag names.
#' 
#' The generated functions will be named according to \code{func_names} and only have a dots
#' argument that is given to \code{\link[XiMpLe:XMLNode]{XMLNode}}. See the examples to
#' understand how it's supposed to work.
#'
#' @param tags A character vector defining the tags the generated functions should produce.
#' @param func_names A character vector the same length as \code{tags}, defining the names of
#'    the functions to generate.
#' @param envir The environment where all generated functions should appear.
#' @param replace Logical, whether objects by the same name already present in \code{envir}
#'    should be preserved or replaced/overwritten.
#' @param func_rename Named character vector defining which tags' functions should get a different name.
#'    This makes it easier to get functions with valid names that generate special tag nodes.
#' @return As many functions as specified by \code{tags}/\code{func_names}.
#' @seealso
#'    \code{\link[XiMpLe:XMLNode]{XMLNode}},
#' @export
#' @examples
#' # Say we would like to generate an HTML website and want to use
#' # <a>, <div> and <p> tags.
#' # The standard way of creatig a <div> node would be this:
#' (my_node <- XMLNode("div", "some content", class="important"))
#'
#' # By using gen_tag_functions(), we can create some shortcut functions
#' # to get better readability for our code and save some typing:
#' gen_tag_functions(tags=c("a", "div", "p"))
#' # We can now use div_() instead of XMLNode("div"):
#' (my_node2 <- div_("some content", class="important"))
#' 
#' # It also works for nested tags:
#' (my_node3 <- div_(a_(href="foo", "some content")))
#' 
#' # if you don't want these functions filling up your .GlobalEnv,
#' # you can also put them in an attached environment, e.g.
#' attach(list(), name="XiMpLe_wrappers")
#' gen_tag_functions(tags=c("head", "body"), envir=as.environment("XiMpLe_wrappers"))

gen_tag_functions <- function(
    tags,
    func_names=paste0(tags, "_"),
    envir=.GlobalEnv,
    replace=FALSE,
    func_rename=c(
        "?xml_"="xml_",
        "!--_"="comment_",
        "![CDATA[_"="CDATA_"
    )
){
    tags <- unique(tags)
    func_names <- unique(func_names)
    func_name_rename <- names(func_rename) %in% func_names
    if(any(func_name_rename)){
        func_names[func_names %in% names(func_rename)[func_name_rename]] <- func_rename[func_name_rename]
    } else {}
    if(!identical(length(tags), length(func_names))){
        stop(simpleError("'tags' must be the same length as 'func_names' (unique values)!"))
    } else {}

    objects_in_env <- ls(envir=envir)

    for(this_tag_n in seq_along(tags)){
        if(any(!func_names[this_tag_n] %in% objects_in_env, isTRUE(replace))){
            if(!func_names[this_tag_n] %in% objects_in_env){
                message(paste0("Creating new function: \"", func_names[this_tag_n], "\""))
            } else {
                message(paste0("Replacing existing object with function: \"", func_names[this_tag_n], "\""))
            }
            eval(
                str2lang(
                    paste0(func_names[this_tag_n], " <- function(...){XMLNode(tag_name=\"", tags[this_tag_n], "\", ...)}")
                ),
                envir=envir
            )
        } else {
            message(paste0("Object exists, skipping: \"", func_names[this_tag_n], "\""))
        }
    }
}
