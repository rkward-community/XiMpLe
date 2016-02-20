# Copyright 2016 Meik Michalke <meik.michalke@hhu.de>
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


#' Constructor function for XiMpLe.DTD objects
#' 
#' Can be used to create document type declaration nodes.
#' 
#' @param name A character string, name of this element (i.e., the XML tag identifier). Must be one of "!DOCTYPE", "!ENTITY", "!ELEMENT",
#'    "!ATTLIST", or "!NOTATION".
#' @param element A character string, name of the element that is being defined (e.g., its attributes).
#'    If \code{name="!ELEMENT"} or \code{name="!ATTLIST"}, this represents the tag name, if \code{name="!ENTITY"} the entity name.
#'    If \code{name="!DOCTYPE"}, it names the doctype, if \code{name="!NOTATION"} the declared notation name.
#' @param publicID A character string defining the public identifier of a doctype, e.g., "-//W3C//DTD HTML 4.01 Transitional//EN".
#'    Only valid if \code{name="!DOCTYPE"} or \code{name="!NOTATION"}.
#' @param systemID A character string defining the system identifier of a doctype, usually a URI like
#'    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd". Only valid if \code{name="!DOCTYPE"} or \code{name="!NOTATION"}.
#' @param decl A named list of character vectors or XML comments.
#'    If \code{name="!ATTLIST"}, each vector must have one to three entries: The vector name is the attribute name,
#'    the first value its type description, the optional second and third values either one of \code{"#REQUIRED"}, \code{"#IMPLIED"} or \code{"#FIXED"},
#'    and/or a default value.
#'    If \code{name="!ELEMENT"} or \code{name="!ENTITY"}, each vector must have one entry describing valid child nodes or the entity, respectively.
#'    Invalid if \code{name="!NOTATION"} or \code{name="!DOCTYPE"}.
#' @param local A list of objects of class XiMpLe.DTD or XML comments, only valid if \code{name="!DOCTYPE"}. These
#'    elements will form the local subset of document type definitions.
#' @return An object of class \code{\link[XiMpLe:XiMpLe.DTD-class]{XiMpLe.DTD}}.
#' @seealso
#'    \code{\link[XiMpLe:XMLTree]{XMLTree}}
#' @export
#' @rdname DTDNode
# @examples
# sample.XML.node <- DTDNode("a",
#   attrs=list(href="http://example.com", target="_blank"),
#   .children="klick here!")

DTDNode <- function(name, element, publicID=NULL, systemID=NULL, decl=NULL, local=NULL){
  
  if(name %in% c("!DOCTYPE", "!NOTATION")){
    if(is.null(publicID)){
      publicID <- character()
    } else {}
  } else {
    publicID <- character()
  }
  if(is.null(systemID)){
    systemID <- character()
  } else {}
  if(is.null(decl)){
    decl <- list()
  } else {}
  if(is.null(local)){
    local <- list()
  } else {}

#   # text node?
#   if(identical(name, "") &
#       (all(unlist(lapply(.children, is.character)))) |
#       all(unlist(lapply(.children, is.numeric)))){
#     value <- paste(..., sep=" ")
#   } else if(identical(.children, list(""))){
#     value <- ""
#   } else {
#     # remove NULLs
#     .children <- .children[unlist(lapply(.children, length) != 0)]
#     # check for text values
#     all.children <- sapply(child.list(.children), function(this.child){
#       if(is.character(this.child) | is.numeric(this.child)){
#         this.child <- new("XiMpLe.DTD",
#             name="",
#             value=as.character(this.child)
#           )
#       } else {}
#       return(this.child)
#     })
#     value <- character()
#   }

  newDTD <- new("XiMpLe.DTD",
    name=name,
    element=element,
    publicID=publicID,
    systemID=systemID,
    decl=decl,
    local=local
  )

  return(newDTD)
}
