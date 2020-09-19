# Copyright 2016-2017 Meik Michalke <meik.michalke@hhu.de>
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


#' Generate XML generator functions from XiMpLe.validity object
#' 
#' Takes an object of class \code{\link[XiMpLe:XiMpLe.validity-class]{XiMpLe.validity}}
#' and turns it into a character vector of generator functions for each XML node that
#' was defined.
#' 
#' The resulting code follows these rules:
#' \itemize{
#'    \item{Each child node gets its own argument, except if there is only one valid child
#'      node. It will use the dots element instead then.}
#'    \item{Each attribute will also get its own argument.}
#'    \item{If \code{CheckValidity=TRUE}, one extra argument named after the value of \code{valParam} will be added.}
#'    \item{All arguments are set to \code{NULL} by default.}
#'    \item{Only the main level of \code{"allAttrs"} will be taken into account, there's no recursion for this slot.}
#' }
#'
#' @param validity An dobject of class \code{XiMpLe.validity}.
#' @param prefix A character string to be used as a prefix for the resulting function names.
#' @param checkValidity Logical, whether all functions should include a check for valid XML.
#' @param indent.by A charachter string defining how indentation should be done.
#' @param roxygenDocs Logical, whether a skeleton for roxygen2-ish documentation should be added.
#' @param valParam A charachter string, name of the additional parameter to use for validation if
#'    \code{checkValidity=TRUE}.
#' @param replaceChar A (single) character to be used as an replacement for invalid characters for
#'    \code{R} parameter names.
#' @param dir A charachter string, path to write files to. If \code{dir=NULL}, no files are being
#'    written, but the results returned in form of a character vector. If \code{dir} is set and the
#'    directory does not yet exist, it will be created.
#' @param overwrite Logical, whether existing files should be replaced when \code{dir} is set.
#' @param oneFile A charachter string. If set, all functions are to be documented in one single *.Rd file,
#'    named like the string.
#' @return If \code{dir=NULL} a named vector of character strings. Otherwise one or more files are
#'    written do the location specified via \code{dir}.
#' @aliases
#'    XMLgenerators,-methods
#'    XMLgenerators,XiMpLe.validity-method
#' @seealso
#'    \code{\link[XiMpLe:XMLValidity]{XMLValidity}} and
#'    \code{\link[XiMpLe:XiMpLe.validity-class]{XiMpLe.validity}}
#' @keywords methods
#' @docType methods
#' @export
#' @rdname XMLgenerators
#' @include 00_class_03_XiMpLe.validity.R
setGeneric("XMLgenerators", function(validity, prefix="XML", checkValidity=TRUE, indent.by="\t", roxygenDocs=FALSE,
  valParam="validity", replaceChar="_", dir=NULL, overwrite=FALSE, oneFile=NULL){standardGeneric("XMLgenerators")})

#' @rdname XMLgenerators
#' @importFrom utils file_test
#' @export
#' @examples
#' HTMLish <- XMLValidity(
#'    children=list(
#'      body=c("a", "p", "ol", "ul", "strong"),
#'      head=c("title"),
#'      html=c("head", "body"),
#'      li=c("a", "br", "strong"),
#'      ol=c("li"),
#'      p=c("a", "br", "ol", "ul", "strong"),
#'      ul=c("li")
#'    ),
#'    attrs=list(
#'      a=c("href", "name"),
#'      p=c("align")
#'    ),
#'    allChildren=c("!--"),
#'    allAttrs=c("id", "class"),
#'    empty=c("br")
#' )
#' XMLgenerators(HTMLish)
setMethod("XMLgenerators", signature(validity="XiMpLe.validity"), function(validity, prefix="XML", checkValidity=TRUE,
  indent.by="\t", roxygenDocs=FALSE, valParam="validity", replaceChar="_", dir=NULL, overwrite=FALSE, oneFile=NULL){
    validitySource <- XMLgenRecursion(validity=validity)
    allKnownNodes  <- validitySource[["allKnownNodes"]]
    children <- validitySource[["children"]]
    childNames <- validitySource[["childNames"]]
    allChildren <- validitySource[["allChildren"]]
    attrs <- validitySource[["attrs"]]
    attrNames  <- validitySource[["attrNames"]]
    allAttrs <- validitySource[["allAttrs"]]
    empty <- validitySource[["empty"]]
    ignore <- validitySource[["ignore"]]

    result <- sapply(
      allKnownNodes,
      function(thisNode){
        thisNodeOptions <- dotsOption <- character()
        attrsInNode <- childrenInNode <- functionBodyAttrs <- functionBodyChildren <- functionBodyValidation <- rxdoc <- NULL
        genStart <- paste0(prefix, validParamName(thisNode), " <- function(")
        # check for child nodes
        if(thisNode %in% childNames){
          thisNodeChildNames <- c(children[[thisNode]], allChildren)
          if(length(thisNodeChildNames) == 1){
            # replace the only child node with dots parameter
            dotsOption <- "..."
          } else {
            thisNodeOptions <- paste0(validParamName(thisNodeChildNames), "=NULL")
          }
        } else if(length(allChildren) > 0){
          thisNodeChildNames <- allChildren
          thisNodeOptions <- paste0(validParamName(thisNodeChildNames), "=NULL")
        } else {
          thisNodeChildNames <- character()
        }
        # check for attributes
        haveAttrs <- FALSE
        if(thisNode %in% attrNames){
          thisNodeAttrNames <- attrs[[thisNode]]
          thisNodeOptions <- c(thisNodeOptions, paste0(validParamName(thisNodeAttrNames), "=NULL"))
          haveAttrs <- TRUE
        } else {
          thisNodeAttrNames <- character()
        }
        if(length(allAttrs) > 0){
          haveAttrs <- TRUE
          thisNodeAttrNames <- c(thisNodeAttrNames, allAttrs)
          thisNodeOptions <- c(thisNodeOptions, paste0(validParamName(allAttrs), "=NULL"))
        } else {}
        if(isTRUE(haveAttrs)){
          functionBodyAttrs <- paste0(indent.by, "attrs.list <- list()")
          attrsInNode <- paste0(indent.by, "attrs=attrs.list")
        } else {}
        if(isTRUE(checkValidity)){
          thisNodeOptions <- c(thisNodeOptions, paste0(validParamName(valParam), "=NULL"))
        } else {}
        genFormals <- paste0(paste0(c(dotsOption, thisNodeOptions), collapse=", "), "){\n")

        # function body
        for (thisNodeAttr in thisNodeAttrNames){
          functionBodyAttrs <- paste(
            functionBodyAttrs,
            paste0("if(!is.null(", validParamName(thisNodeAttr), ")){"),
            paste0(indent.by, "attrs.list[[\"", thisNodeAttr, "\"]] <- ", validParamName(thisNodeAttr)),
            "} else {}",
            sep=paste0("\n", indent.by)
          )
        }
        if(identical(dotsOption, character())){
          if(length(thisNodeChildNames > 1)){
            childrenInNode <- paste(
              paste0(indent.by, "all.children <- list()"),
              paste0("for (thisNode in list(", paste0(validParamName(thisNodeChildNames), collapse=", "), ")){"),
              paste0(indent.by, "if(!is.null(thisNode)){"),
              paste0(indent.by, indent.by, "all.children <- append(all.children, thisNode)"),
              paste0(indent.by, "} else {}"),
              "}",
              sep=paste0("\n", indent.by)
            )
          } else {}

          functionBodyChildren <- c(
            paste0(indent.by, indent.by, "\"", thisNode, "\""),
            if(!is.null(attrsInNode)){attrsInNode},
            if(!is.null(childrenInNode)){paste0(indent.by, ".children=all.children")}
          )
          functionBodyChildren <- paste0(indent.by, "results <- XMLNode(\n",
            paste0(functionBodyChildren, collapse=paste0(",\n", indent.by)),
            "\n", indent.by, ")"
          )
        } else {
          functionBodyChildren <- c(
            paste0(indent.by, "results <- XMLNode("),
            paste0(indent.by, "\"", thisNode, "\","),
            if(!is.null(attrsInNode)){paste0(attrsInNode, ",")},
            paste0(indent.by, ".children=list(...)"),
            ")"
          )
          functionBodyChildren <- paste0(functionBodyChildren, collapse=paste0("\n", indent.by))
        }
        if(isTRUE(checkValidity)){
          functionBodyValidation <- paste(
            paste0(indent.by, "if(!is.null(", validParamName(valParam), ")){"),
            paste0(indent.by, "validXML(results, validity=", validParamName(valParam), ")"),
            "} else {}",
            sep=paste0("\n", indent.by)
          )
        } else {}
        functionBodyReturn <- paste0(indent.by, "return(results)")

        functionBody <- c(
          if(!is.null(functionBodyAttrs)){functionBodyAttrs},
          if(!is.null(childrenInNode)){childrenInNode},
          if(!is.null(functionBodyChildren)){functionBodyChildren},
          if(!is.null(functionBodyValidation)){functionBodyValidation},
          functionBodyReturn
        )
        functionBody <- paste0(functionBody, collapse="\n\n")
        
        genResult <- paste0(genStart, genFormals, functionBody, "\n}")

        # add documentation?
        if(isTRUE(roxygenDocs)){
          if(identical(dotsOption, character())){
            if(length(thisNodeChildNames > 1)){
              rxdocChildren <- paste0(
                "#' @param ", validParamName(thisNodeChildNames),
                " An object of class \\code{XiMpLe.node} (or list of) to define \\code{<", thisNodeChildNames, ">} child nodes for this node. Ignored if \\code{NULL.}"
              )
            } else {
              rxdocChildren <- NULL
            }
          } else {
              rxdocChildren <- paste0(
                "#' @param ... ",
                " Object(s) of class \\code{XiMpLe.node} to define \\code{<", thisNodeChildNames, ">} child nodes for this node. Ignored if empty."
              )
          }

          if(!identical(thisNodeAttrNames, character())){
            rxdocAttrs <- paste0(
              "#' @param ", validParamName(thisNodeAttrNames),
              " Character string, used to set the \\code{", thisNodeAttrNames,"} attribute of this node. Ignored if \\code{NULL.}"
            )
          } else {
            rxdocAttrs <- NULL
          }
          rxdocParams <- c(rxdocChildren, rxdocAttrs)
          
          if(isTRUE(checkValidity)){
          rxdocParams <- c(rxdocParams,
              paste0(
                "#' @param ", validParamName(valParam), " ",
                " An object of class \\code{XiMpLe.validity} to check the validity of this node. Ignored if \\code{NULL.}"
              )
            )
          } else {}
          
          if(is.null(oneFile)){
            rdFile <- paste0(prefix, validParamName(thisNode))
          } else {
            rdFile <- validParamName(gsub("\\.rd$|\\.Rd$", "", oneFile))
          }
          rxdoc <- paste(
            paste0("#' Generate <", thisNode, "> XML nodes"),
            "#'",
            paste0("#' Generates an object of class \\code{\\link[XiMpLe:XiMpLe.node-class]{XiMpLe.node}} with node name \\code{<", thisNode, ">}."),
            "#'",
            paste0(rxdocParams, collapse="\n"),
            "#' @return An object of class \\code{\\link[XiMpLe:XiMpLe.node-class]{XiMpLe.node}}.",
            paste0("#' @rdname ", rdFile),
            "#' @export",
            sep="\n"
          )
          genResult <- paste0(rxdoc, "\n", genResult)
        } else {}

        return(genResult) 
      },
      USE.NAMES=FALSE
    )
    if(is.null(dir)){
      return(result)
    } else {
      if(!file_test("-d", dir)){
        stopifnot(dir.create(dir, recursive=TRUE))
        message(paste0("created directory: ", dir))
      } else {}
      for (thisFile in names(result)){
        thisFileName <- paste0(prefix, validParamName(thisFile), ".R")
        thisFilePath <- file.path(dir, thisFileName)
        if(file.exists(thisFilePath)){
          if(isTRUE(overwrite)){
            cat(result[thisFile], file=thisFilePath)
            message(paste0("file exists, overwriting: ", thisFilePath))
          } else {
            warning(paste0("file exists, skipping: ", thisFilePath), call.=FALSE)
          }
        } else {
          cat(result[thisFile], file=thisFilePath)
          message(paste0("written new file: ", thisFilePath))
        }
      }
      return(invisible(NULL))
    }
  }
)


## function XMLgenRecursion()
# helper function to get all child nodes and attributes out of nested validity objects
XMLgenRecursion <- function(validity){
  children <- slot(validity, "children")
  childrenVal <- which(sapply(children, is.XiMpLe.validity, USE.NAMES=FALSE))
  childrenChr <- children[sapply(children, is.character, USE.NAMES=FALSE)]
  childNames <- names(children)
  attrs <- slot(validity, "attrs")
  attrNames <- names(attrs)
  allChildren <- slot(validity, "allChildren")
  allAttrs <- slot(validity, "allAttrs")
  empty <- slot(validity, "empty")
  ignore <- slot(validity, "ignore")
  allKnownNodes <- unique(c(childNames, unlist(childrenChr), allChildren, attrNames, empty, ignore))

  # full recursion
  if(length(childrenVal) > 0){
    for (thisChildValNum in childrenVal){
      recursiveResult <- XMLgenRecursion(validity=children[[thisChildValNum]])
      # this adds child nodes for the current node to the list
      thisRecNodeName <- childNames[[thisChildValNum]]
      if(length(recursiveResult[["childNames"]]) > 0){
        childrenChr[[thisRecNodeName]] <- sort(unique(c(childrenChr[[thisRecNodeName]], recursiveResult[["childNames"]])))
      } else {}
      # this now also adds children's child nodes
      for (thisRecChildName in names(recursiveResult[["children"]])){
        if(thisRecChildName %in% names(childrenChr)){
          childrenChr[[thisRecChildName]] <- sort(unique(c(childrenChr[[thisRecChildName]], recursiveResult[["children"]][[thisRecChildName]])))
        } else {
          childrenChr <- append(childrenChr, recursiveResult[["children"]][thisRecChildName])
        }
      }
      for (thisRecAttrName in names(recursiveResult[["attrs"]])){
        if(thisRecAttrName %in% attrNames){
          attrs[[thisRecAttrName]] <- sort(unique(c(attrs[[thisRecAttrName]], recursiveResult[["attrs"]][[thisRecAttrName]])))
        } else {
          attrs <- append(attrs, recursiveResult[["attrs"]][thisRecAttrName])
        }
      }
      childNames <- unique(c(childNames, recursiveResult[["childNames"]]))
      attrNames <- unique(c(attrNames, recursiveResult[["attrNames"]]))
      empty <- unique(c(empty, recursiveResult[["empty"]]))
      ignore <- unique(c(ignore, recursiveResult[["ignore"]]))
      allKnownNodes <- unique(c(allKnownNodes, recursiveResult[["allKnownNodes"]]))
    }
  } else {}

  result <- list(
    allKnownNodes=sort(allKnownNodes),
    children=childrenChr[sort(names(childrenChr))],
    childNames=sort(childNames),
    allChildren=sort(allChildren),
    attrs=attrs[sort(names(attrs))],
    attrNames=sort(attrNames),
    allAttrs=sort(allAttrs),
    empty=sort(empty),
    ignore=sort(ignore)
  )

  return(result)
} ## end function XMLgenRecursion()
