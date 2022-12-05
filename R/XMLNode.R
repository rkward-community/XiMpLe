# Copyright 2011-2022 Meik Michalke <meik.michalke@hhu.de>
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


#' Constructor function for XiMpLe.node objects
#' 
#' Can be used to create XML nodes.
#' 
#' To generate a CDATA node, set \code{tag_name="![CDATA["}, to create a comment, set \code{tag_name="!--"}.
#' 
#' Note that all defined attributes will silently be dropped if a text node, CDATA node or comment is generated.
#' 
#' @param tag_name Character string, the tag name.
#' @param ... Optional children for the tag. Must be either objects of class XiMpLe.node or character strings,
#'    which are treated as attributes if they are named, and as simple text values otherwise.
#'    Use a named \code{character()} value for empty attributes.
#'    If this argument is empty, the tag will be treated as an empty tag. To force a closing tag, supply an empty
#'    string, i.e. \code{""}.
#' @param attrs An optional named list of attributes. Will be appended to attributes already defined in
#'    the \code{...} argument.
#' @param shine A numeric integer value between 0 and 2, overwriting the \code{shine} value of, e.g., \code{\link[XiMpLe:pasteXML]{pasteXML}}
#'    for this particular node.
#' @param namespace Currently ignored.
#' @param namespaceDefinitions Currently ignored.
#' @param .children Alternative way of specifying children, if you have them already as a list.
#'    This argument completely replaces values defined in the \code{...} argument.
#' @return An object of class \code{\link[XiMpLe:XiMpLe_node-class]{XiMpLe_node}}.
#' @seealso
#'    \code{\link[XiMpLe:XMLTree]{XMLTree}},
#'    \code{\link[XiMpLe:pasteXML]{pasteXML}}
#' @export
#' @rdname XMLNode
#' @examples
#' (sample.XML.node <- XMLNode("a",
#'   attrs=list(
#'     href="http://example.com",
#'     target="_blank"
#'   ),
#'   .children="klick here!"
#' ))
#' # This is equivalent
#' (sample.XML.node2 <- XMLNode("a",
#'   "klick here!",
#'   href="http://example.com",
#'   target="_blank"
#' ))
#' # As is this
#' (sample.XML.node3 <- XMLNode("a",
#'   .children=list(
#'     "klick here!",
#'     href="http://example.com",
#'     target="_blank"
#'   )
#' ))

XMLNode <- function(
    tag_name,
    ...,
    attrs,
    shine,
    namespace,
    namespaceDefinitions,
    .children=list(...)
){
    # check if this might be old code not aware of the "tag_name" argument,
    # which was renamed from "name" in 0.11-1
    if(missing(tag_name)){
        if("name" %in% names(.children)){
            stop(simpleError("The argument 'name' if XiMpLe::XMLNode() was renamed 'tag_name' in XiMpLe 0.11-1, please adjust your code."))
        } else {
            stop(simpleError("XiMpLe::XMLNode() needs a 'tag_name'!"))
        }
    } else {}

    # split dots argument into named/unnamed character strings and the rest
    if(length(.children) > 0){
        dots_is_char <- sapply(
            .children,
            function(this_chld){
                if(length(this_chld) > 1){
                    stop(simpleError("child nodes must not have length > 1!"))
                } else{}
                any(is.character(this_chld), is.numeric(this_chld))
            },
            USE.NAMES=FALSE
        )
        # force numerics into character
        .children[which(dots_is_char)] <- sapply(
            which(dots_is_char),
            function(this_dot){
                as.character(.children[[this_dot]])
            }
        )
        dots_attrs <- lapply(.children[dots_is_char & !names(.children) == ""], as.character)
        # remove args from children list
        .children[dots_is_char & !names(.children) == ""] <- NULL
        # strip all names, might still be empty strings and cause problems in later comparisons
        names(.children) <- NULL
    } else {
        dots_attrs <- list()
    }

    if(missing(attrs)){
        attrs <- dots_attrs
    } else {
        attrs <- append(dots_attrs, as.list(attrs))
    }

    extra <- list()
    if(!missing(shine)){
        extra[["shine"]] <- shine
    } else {}

    all_children <- list()

    # text node?
    if(
        identical(tag_name, "") &
        (all(sapply(.children, is.character, USE.NAMES=FALSE)))
    ){
        value <- paste(.children, sep=" ")
    } else if(identical(.children, list(""))){
        value <- ""
    } else {
        # remove NULLs
        .children <- .children[sapply(.children, length, USE.NAMES=FALSE) != 0]
        # check for text values
        all_children <- lapply(child.list(.children), function(this_chld){
                if(is.character(this_chld)){
                    this_chld <- XiMpLe_node(
                        name="",
                        value=this_chld
                    )
                } else {}
                return(this_chld)
            }
        )
        names(all_children) <- NULL
        value <- character()
    }

    newNode <- XiMpLe_node(
        name=tag_name,
        attributes=attrs,
        children=all_children,
        value=value,
        extra=extra
    )

    return(newNode)
}
