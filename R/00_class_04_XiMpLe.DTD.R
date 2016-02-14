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


#' Class XiMpLe.DTD
#'
#' This class is used to create XML document type defintions to be used in the \code{dtd} slot in
#' \code{\link[XiMpLe:XiMpLe.doc-class]{XiMpLe.doc}} objects, e.g. by calling \code{\link[XiMpLe:XMLTree]{XMLTree}}.
#' 
#' The name of a node defines how slots are used and how nodes can be nested. You should use the function XMLDTD to create elements of this class.
#'
#' @slot name A character string, name of this element (i.e., the XML tag identifier). Must be one of "!DOCTYPE", "!ENTITY", "!ELEMENT",
#'    "!ATTLIST", or "!NOTATION".
#' @slot element A character string, name of the element that is being defined (e.g., its attributes).
#'    If \code{name="!ELEMENT"} or \code{name="!ATTLIST"}, this represents the tag name, if \code{name="!ENTITY"} the entity name.
#'    If \code{name="!DOCTYPE"}, it names the doctype, if \code{name="!NOTATION"} the declared notation name.
#' @slot publicID A character string defining the public identifier of a doctype, e.g., "-//W3C//DTD HTML 4.01 Transitional//EN".
#'    Only valid if \code{name="!DOCTYPE"} or \code{name="!NOTATION"}.
#' @slot systemID A character string defining the system identifier of a doctype, usually a URI like
#'    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd". Only valid if \code{name="!DOCTYPE"} or \code{name="!NOTATION"}.
#' @slot decl A named list of character vectors or XML comments.
#'    If \code{name="!ATTLIST"}, each vector must have one to three entries: The vector name is the attribute name,
#'    the first value its type description, the optional second and third values either one of \code{"#REQUIRED"}, \code{"#IMPLIED"} or \code{"#FIXED"},
#'    and/or a default value.
#'    If \code{name="!ELEMENT"} or \code{name="!ENTITY"}, each vector must have one entry describing valid child nodes or the entity, respectively.
#'    Invalid if \code{name="!NOTATION"} or \code{name="!DOCTYPE"}.
#' @slot local A list of objects of class XiMpLe.DTD or XML comments, only valid if \code{name="!DOCTYPE"}. This
#'    slot will form the local subset of document type definitions.
#' @name XiMpLe.DTD,-class
#' @aliases XiMpLe.DTD-class XiMpLe.DTD,-class
#' @import methods
#' @keywords classes
#' @rdname XiMpLe.DTD-class
#' @export
#' @examples
#' \dontrun{
#' XMLDTD("!ELEMENT", element="html", decl=list("(head, body)"))
#' XMLDTD("!ATTLIST", element="img", decl=list(
#'     src=c("CDATA", "#REQUIRED"),
#'     id=c("ID", "#IMPLIED"),
#'     sort=c("CDATA", "#FIXED", "\"true\"")
#'     print=c("(yes | no)", "yes")
#'   )
#' )
#' }

setClass("XiMpLe.DTD",
  representation=representation(
    name="character",
    element="character",
    publicID="character",
    systemID="character",
    decl="list",
    local="list"
  ),
  prototype(
    name=character(),
    element=character(),
    id=character(),
    publicID=character(),
    systemID=character(),
    decl=list(),
    local=list()
  )
)

# !ELEMENT
# - EMPTY -- no child nodes
# - ANY -- all kinds of child nodes allowed
# - sequence list: (a, b, c) -- all nodes must appear and in this order
# - choice list: (a | b | c)* -- any of these child elements allowed in any order/quantity
# - #PCDATA -- only one text element allowed
# - (#PCDATA | a | b)* -- text data or one or more of the child nodes allowed in any order
# - quantifiers:
#   - +: present one or more times
#   - *: optional
#   - ?: not more than one appearance
#   - none: exactly one appearance in the specified order

# !ATTLIST
# - CDATA -- character data
# - ID -- a valid and unique ID incl. "#names"
# - IDREF or IDREFS -- a valid and unique ID referencing an ID defined in another element of the document
# - NMTOKEN or NMTOKENS -- only valid name tokens allowed
# - ENTITY or ENTITIES -- must be an unparsed external entity (or space separated list of)
# - (value1|...) -- attribute value can only be one of these (possibly quoted)
# - NOTATION (notation1|...) -- value must be one of these notaions that are also defined elsewhere
# - #REQUIRED/#IMPLIED/#FIXED
# - "..." -- default value if not set

# setValidity("XiMpLe.DTD", function(object){
#   return(TRUE)
# })
