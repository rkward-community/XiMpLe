# Copyright 2011-2016 Meik Michalke <meik.michalke@hhu.de>
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


#' Class XiMpLe.doc
#'
#' This class is used for objects that are returned by \code{\link[XiMpLe:parseXMLTree]{parseXMLTree}}.
#'
#' @slot file Character string, Name of the file.
#' @slot xml Either a named list of character values (attributes for the XML declaration of the file),
#'    or a list of XiMpLe.nodes with tags whose names must start with a "?".
#' @slot dtd A named list, attributes for the doctype definition of the file.
#' @slot children A list of objects of class XiMpLe.node, representing the DOM structure of the XML document.
#' @name XiMpLe.doc,-class
#' @aliases XiMpLe.doc-class XiMpLe.doc,-class
#' @include 00_class_01_XiMpLe.node.R
#' @import methods
#' @keywords classes
#' @rdname XiMpLe.doc-class
#' @export

setClass("XiMpLe.doc",
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
    if(all(sapply(obj.xml, is.character))){
      obj.xml.names <- names(obj.xml)
      if(length(obj.xml) != length(obj.xml.names)){
        stop(simpleError("Invalid object: All xml declarations must have names!"))
      } else {}
    } else if(all(sapply(obj.xml, is.XiMpLe.node))){
      doc.xml <- sapply(
        obj.xml,
        function(this.decl){
          if(!grepl("^\\?", XMLName(this.decl))){
            stop(simpleError(paste0("Invalid object: All xml declarations must be named character values or\n  XiMpLe nodes whose tag names start with \"?\"!")))
          } else {}
        }
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
    child.nodes <- sapply(obj.children, function(this.child){is.XiMpLe.node(this.child)})
    if(!all(child.nodes)){
      stop(simpleError("Invalid object: All list elements of children must be of class XiMpLe.node!"))
    } else {}
  } else {}
  return(TRUE)
})
