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


#' Classes XiMpLe_doc and XiMpLe.doc (old)
#'
#' This class is used for objects that are returned by \code{\link[XiMpLe:parseXMLTree]{parseXMLTree}}.
#'
#' Class \code{XiMpLe.doc} is the older one, \code{XiMpLe_doc} was introduced with \code{XiMpLe} 0.11-1. It has two new slots
#' (\code{extra} and \code{version}) that would have made it impossible to load old objects without issues. \code{XiMpLe_doc}
#' inherits from \code{XiMpLe.doc}. You can convert old objects into valid new ones using the \code{as_XiMpLe_doc} method and
#' are also advised to do so, as the \code{XiMpLe.doc} class might become deprecated in future releases.
#' 
#' A contructor function \code{XiMpLe_doc(...)} is available to be used instead of \code{new("XiMpLe_doc", ...)}.
#'
#' There's also \code{XiMpLe_doc_old(...)} to be used instead of \code{new("XiMpLe.doc", ...)}, but you should not use that any longer.
#'
#' @slot file Character string, Name of the file.
#' @slot xml Either a named list of character values (attributes for the XML declaration of the file),
#'    or a list of \code{XiMpLe_node}s with tags whose names must start with a "?".
#' @slot dtd A named list, attributes for the doctype definition of the file.
#' @slot children A list of objects of class \code{\link[XiMpLe:XiMpLe_node]{XiMpLe_node}} (\code{XiMpLe_doc} only), or
#'    \code{\link[XiMpLe:XiMpLe_node]{XiMpLe.node}} (old, \code{XiMpLe.doc} only), representing the DOM structure of the XML document.
#' @name XiMpLe_doc,-class
#' @rdname XiMpLe_doc-class
#' @aliases XiMpLe.doc-class XiMpLe.doc,-class
#' @include 00_class_01_XiMpLe.node.R
#' @import methods
#' @keywords classes
#' @export XiMpLe_doc_old
#' @exportClass XiMpLe.doc

XiMpLe_doc_old <- setClass("XiMpLe.doc",
  representation=representation(
    file="character",
    xml="list",
    dtd="list",
    children="list"
  ),
  prototype(
    file=character(),
    xml=list(),
    dtd=list(),
    children=list()
  )
)

setValidity("XiMpLe.doc", function(object){
  obj.xml <- slot(object, "xml")
  obj.dtd <- slot(object, "dtd")
  obj.children <- slot(object, "children")

  obj.dtd.names <- names(obj.dtd)
  # if there are declarations, check that they all have names
  if(length(obj.xml) > 0){
    if(all(sapply(obj.xml, is.character, USE.NAMES=FALSE))){
      obj.xml.names <- names(obj.xml)
      if(length(obj.xml) != length(obj.xml.names)){
        stop(simpleError("Invalid object: All xml declarations must have names!"))
      } else {}
    } else if(all(sapply(obj.xml, is.XiMpLe.node, USE.NAMES=FALSE))){
      doc.xml <- sapply(
        obj.xml,
        function(this.decl){
          if(!grepl("^\\?", XMLName(this.decl))){
            stop(simpleError(paste0("Invalid object: All xml declarations must be named character values or\n  XiMpLe nodes whose tag names start with \"?\"!")))
          } else {}
        },
        USE.NAMES=FALSE
      )
    } else {
      stop(simpleError(paste0("Invalid object: All xml declarations must be named character values or\n  XiMpLe nodes whose tag names start with \"?\"!")))
    }
  } else {}
  if(length(obj.dtd) > 0){
    if(length(obj.dtd) != length(obj.dtd.names)){
      stop(simpleError("Invalid object: All doctype declarations must have names!"))
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

#' @slot extra A named list that can be used to store additional information on a document (\code{XiMpLe_doc} only).
#' @slot version A numeric integer, currently defaults to 2 (\code{XiMpLe_doc} only). This is a version number that can be used in the future
#'    in combination with the added \code{extra} slot. Should that get sime supported values that are interpreted by package methods,
#'    the version number will be increased and the differences documented here. You shouldn't set it manually.
#' @rdname XiMpLe_doc-class
#' @aliases XiMpLe_doc-class XiMpLe_doc,-class
#' @keywords classes
#' @export XiMpLe_doc
#' @exportClass XiMpLe_doc
XiMpLe_doc <- setClass("XiMpLe_doc",
  representation=representation(
    extra="list",
    version="numeric"
  ),
  prototype=prototype(
    extra=list(),
    version=2
  ),
  contains="XiMpLe.doc"
)

setValidity("XiMpLe_doc", function(object){
  obj.xml <- slot(object, "xml")
  obj.dtd <- slot(object, "dtd")
  obj.children <- slot(object, "children")
  obj.version <- slot(object, "version")
  # extra is not checked at the moment, as there are no defined values yet
  # obj.extra <- slot(object, "extra")


  obj.dtd.names <- names(obj.dtd)
  # if there are declarations, check that they all have names
  if(length(obj.xml) > 0){
    if(all(sapply(obj.xml, is.character, USE.NAMES=FALSE))){
      obj.xml.names <- names(obj.xml)
      if(length(obj.xml) != length(obj.xml.names)){
        stop(simpleError("Invalid object: All xml declarations must have names!"))
      } else {}
    } else if(all(sapply(obj.xml, is.XiMpLe.node, USE.NAMES=FALSE))){
      doc.xml <- sapply(
        obj.xml,
        function(this.decl){
          if(!grepl("^\\?", XMLName(this.decl))){
            stop(simpleError(paste0("Invalid object: All xml declarations must be named character values or\n  XiMpLe nodes whose tag names start with \"?\"!")))
          } else {}
        },
        USE.NAMES=FALSE
      )
    } else {
      stop(simpleError(paste0("Invalid object: All xml declarations must be named character values or\n  XiMpLe nodes whose tag names start with \"?\"!")))
    }
  } else {}
  if(length(obj.dtd) > 0){
    if(length(obj.dtd) != length(obj.dtd.names)){
      stop(simpleError("Invalid object: All doctype declarations must have names!"))
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

  return(TRUE)
})
