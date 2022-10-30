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
#' To generate a CDATA node, set \code{name="![CDATA["}, to create a comment, set \code{name="!--"}.
#' 
#' Note that all defined attributes will silently be dropped if a text node, CDATA node or comment is generated.
#' 
#' @param name Character string, the tag name.
#' @param ... Optional children for the tag. Must be either objects of class XiMpLe.node or character strings,
#'    which are treated as attributes if they are named, and as simple text values otherwise.
#'    If this is empty, the tag will be treated as an empty tag. To force a closing tag, supply an empty
#'    string, i.e. \code{""}.
#' @param attrs An optional named list of attributes. Will be appended to attributes already defined in
#'    the \code{...} argument.
#' @param namespace Currently ignored.
#' @param namespaceDefinitions Currently ignored.
#' @param .children Alternative way of specifying children, if you have them already as a list.
#'    This argument completely replaces values defined in the \code{...} argument.
#' @return An object of class \code{\link[XiMpLe:XiMpLe.node-class]{XiMpLe.node}}.
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
    name,
    ...,
    attrs,
    namespace="",
    namespaceDefinitions=NULL,
    .children=list(...)
){
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
        .children[dots_is_char] <- as.character(.children[dots_is_char])
        dots_attrs <- lapply(.children[dots_is_char & !names(.children) == ""], as.character)
        # remove args from children list
        .children[dots_is_char & !names(.children) == ""] <- NULL
    } else {
        dots_attrs <- list()
    }

    if(missing(attrs)){
        attrs <- dots_attrs
    } else {
        attrs <- append(dots_attrs, as.list(attrs))
    }

    all_children <- list()

    # text node?
    if(
        identical(name, "") &
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
                    this_chld <- new("XiMpLe.node",
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

    newNode <- new("XiMpLe.node",
        name=name,
        attributes=attrs,
        children=all_children,
        value=value
    )

    return(newNode)
}
