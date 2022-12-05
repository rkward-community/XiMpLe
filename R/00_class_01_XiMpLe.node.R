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


#' Classes XiMpLe_node and XiMpLe.node (old)
#'
#' These classes are used to create DOM trees of XML documents, like objects that are returned
#' by \code{\link[XiMpLe:parseXMLTree]{parseXMLTree}}.
#' 
#' Class \code{XiMpLe.node} is the older one, \code{XiMpLe_node} was introduced with \code{XiMpLe} 0.11-1. It has two new slots
#' (\code{extra} and \code{version}) that would have made it impossible to load old objects without issues. \code{XiMpLe_node}
#' inherits from \code{XiMpLe.node}. You can convert old objects into valid new ones using the \code{as_XiMpLe_node} method and
#' are also advised to do so, as the \code{XiMpLe.node} class might become deprecated in future releases.
#' 
#' There are certain special values predefined for the \code{name} slot to easily create special XML elements:
#' \describe{
#'     \item{\code{name=""}}{If the name is an empty character string, a pseudo node is created,
#'       \code{\link[XiMpLe:pasteXMLNode]{pasteXMLNode}} will paste its \code{value} as plain text.}
#'     \item{\code{name="!--"}}{Creates a comment tag, i.e., this will comment out all its \code{children}.}
#'     \item{\code{name="![CDATA["}}{Creates a CDATA section and places all its \code{children} in it.}
#'     \item{\code{name="*![CDATA["}}{Creates a CDATA section and places all its \code{children} in it, where the CDATA markers are
#'       commented out by \code{/* */}, as is used for JavaScript in XHTML.}
#' }
#'
#' A contructor function \code{XiMpLe_node(...)} is available to be used instead of \code{new("XiMpLe_node", ...)}.
#'
#' There's also \code{XiMpLe_node_old(...)} to be used instead of \code{new("XiMpLe.node", ...)}, but you should not use that any longer.
#'
#' @slot name Name of the node (i.e., the XML tag identifier). For special names see details.
#' @slot attributes A list of named character values, representing the attributes of this node.
#'    Use \code{character()} as value for empty attributes.
#' @slot children A list of further objects of class XiMpLe.node, representing child nodes of this node.
#' @slot value Plain text to be used as the enclosed value of this node. Set to \code{value=""} if you
#'    want a childless node to be forced into an non-empty pair of start and end tags by \code{\link[XiMpLe:pasteXMLNode]{pasteXMLNode}}.
#' @name XiMpLe_node,-class
#' @aliases XiMpLe.node-class XiMpLe.node,-class
#' @import methods
#' @keywords classes
#' @rdname XiMpLe_node-class
#' @export XiMpLe_node_old
#' @exportClass XiMpLe.node

XiMpLe_node_old <- setClass("XiMpLe.node",
  representation=representation(
    name="character",
    attributes="list",
    children="list",
    value="character"
  ),
  prototype(
    name=character(),
    attributes=list(),
    children=list(),
    value=character()
  )
)

setValidity("XiMpLe.node", function(object){
  obj.name <- slot(object, "name")
  obj.attributes <- slot(object, "attributes")
  obj.children <- slot(object, "children")
  obj.value <- slot(object, "value")

  if(isTRUE(!nchar(obj.name) > 0) & isTRUE(!nchar(obj.value) > 0)){
    print(str(object))
    stop(simpleError("Invalid object: A node must at least have a name or a value!"))
  } else {}

  obj.attributes.names <- names(obj.attributes)
  # if there are attributes, check that they all have names
  if(length(obj.attributes) > 0){
    if(length(obj.attributes) != length(obj.attributes.names)){
      stop(simpleError("Invalid object: All attributes must have names!"))
    } else {}
  } else {}

  # check content of children
  if(length(obj.children) > 0){
    child.nodes <- sapply(
      obj.children,
      is.XiMpLe.node,
      USE.NAMES=FALSE
    )
    if(!all(child.nodes)){
      stop(simpleError("Invalid object: All list elements of children must be of class XiMpLe.node!"))
    } else {}
  } else {}
  return(TRUE)
})

#' @slot extra A named list that can be used to store additional information on a node (\code{XiMpLe_node} only).
#'   The only value with a noticeable effect as of now (version 2) is:
#'    \describe{
#'      \item{\code{shine}: }{A numeric integer value between 0 and 2, overwriting the \code{shine} value of, e.g., \code{\link[XiMpLe:pasteXML]{pasteXML}}
#'        for this particular node.}
#'    }
#' @slot version A numeric integer, currently defaults to 2 (\code{XiMpLe_node} only). This is a version number that can be used in the future
#'    in combination with the added \code{extra} slot. Should that get more supported values like \code{shine} that are interpreted by package methods,
#'    the version number will be increased and the differences documented here. You shouldn't set it manually.
#' @rdname XiMpLe_node-class
#' @aliases XiMpLe_node-class XiMpLe_node,-class
#' @keywords classes
#' @export XiMpLe_node
#' @exportClass XiMpLe_node

XiMpLe_node <- setClass("XiMpLe_node",
  representation=representation(
    extra="list",
    version="numeric"
  ),
  prototype=prototype(
    extra=list(),
    version=2
  ),
  contains="XiMpLe.node"
)

setValidity("XiMpLe_node", function(object){
  obj.name <- slot(object, "name")
  obj.attributes <- slot(object, "attributes")
  obj.children <- slot(object, "children")
  obj.value <- slot(object, "value")
  obj.version <- slot(object, "version")
  obj.extra <- slot(object, "extra")

  if(isTRUE(!nchar(obj.name) > 0) & isTRUE(!nchar(obj.value) > 0)){
    print(str(object))
    stop(simpleError("Invalid object: A node must at least have a name or a value!"))
  } else {}

  obj.attributes.names <- names(obj.attributes)
  # if there are attributes, check that they all have names
  if(length(obj.attributes) > 0){
    if(length(obj.attributes) != length(obj.attributes.names)){
      stop(simpleError("Invalid object: All attributes must have names!"))
    } else {}
  } else {}

  # check content of children
  if(length(obj.children) > 0){
    child.nodes <- sapply(
      obj.children,
      is.XiMpLe_node,
      USE.NAMES=FALSE
    )
    if(!all(child.nodes)){
      stop(simpleError("Invalid object: All list elements of children must be of class XiMpLe_node!"))
    } else {}
  } else {}

  if(!identical(obj.version, 2)){
    stop(simpleError(paste0("Invalid object: Unsupported version number (", paste0(obj.version, collapse=" "), ")!")))
  } else {}
  
  if("shine" %in% names(obj.extra)){
    if(!is.numeric(obj.extra[["shine"]]) | !obj.extra[["shine"]] %in% 0:2){
      stop(simpleError("Invalid object: \"shine\" in slot \"extra\" must be an integer value between 0 and 2!"))
    } else {}
  } else {}

  return(TRUE)
})
