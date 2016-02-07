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


#' Generate XML generator functions from XiMpLe.valisity object
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
#'    \item{If \code{CheckValidity=TRUE}, one extra argument \code{validity will be added}}
#'    \item{All arguments are set to \code{NULL} by default}
#' }
#'
#' @param validity An dobject of class \code{XiMpLe.validity}.
#' @param prefix A character string to be used as a prefix for the resulting function names.
#' @param checkValidity Logical, whether all functions should include a check for valid XML.
#' @param indent.by A charachter string defining how indentation should be done.
#' @param roxygenDocs Logical, whether a skeleton for roxygen2-ish documentation should be added.
#' @return A named vector of character strings.
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
setGeneric("XMLgenerators", function(validity, prefix="XML", checkValidity=TRUE, indent.by="\t", roxygenDocs=FALSE){standardGeneric("XMLgenerators")})

#' @rdname XMLgenerators
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
setMethod("XMLgenerators", signature(validity="XiMpLe.validity"), function(validity, prefix="XML", checkValidity=TRUE, indent.by="\t", roxygenDocs=FALSE){
    children <- slot(validity, "children")
    children <- children[sapply(children, is.character)]
    ## TODO: support full recursion
#     childrenVal <- which(sapply(children, is.XiMple.validity))
#     for (thisChildVal in childrenVal){
#       
#     }
    
    childNames <- names(children)
    attrs <- slot(validity, "attrs")
    attrNames <- names(attrs)
    ## TODO:
    # allChildren <- slot(validity, "allChildren")
    # allAttrs <- slot(validity, "allAttrs")
    allKnownNodes <- sort(unique(c(childNames, attrNames)))
    result <- sapply(
      allKnownNodes,
      function(thisNode){
        thisNodeOptions <- dotsOption <- character()
        attrsInNode <- childrenInNode <- functionBodyAttrs <- functionBodyChildren <- functionBodyValidation <- rxdoc <- NULL
        genStart <- paste0(prefix, thisNode, " <- function(")
        # check for child nodes
        if(thisNode %in% childNames){
          thisNodeChildNames <- children[[thisNode]]
          if(length(thisNodeChildNames) == 1){
            # replace the only child node with dots parameter
            dotsOption <- "..."
          } else {
            thisNodeOptions <- paste0(thisNodeChildNames, "=NULL")
          }
        } else {
          thisNodeChildNames <- character()
        }
        # check for attributes
        if(thisNode %in% attrNames){
          thisNodeAttrNames <- attrs[[thisNode]]
          thisNodeOptions <- c(thisNodeOptions, paste0(thisNodeAttrNames, "=NULL"))
          functionBodyAttrs <- paste0(indent.by, "attrs.list <- list()")
          attrsInNode <- paste0(indent.by, "attrs=attrs.list")
        } else {
          thisNodeAttrNames <- character()
        }
        if(isTRUE(checkValidity)){
          ## TODO: configurable varname
          thisNodeOptions <- c(thisNodeOptions, "validity=NULL")
        } else {}
        genFormals <- paste0(paste0(c(dotsOption, thisNodeOptions), collapse=", "), "){\n")

        # function body
        for (thisNodeAttr in thisNodeAttrNames){
          functionBodyAttrs <- paste(
            functionBodyAttrs,
            paste0("if(!is.null(", thisNodeAttr, ")){"),
            paste0(indent.by, "attrs.list[[\"", thisNodeAttr, "\"]] <- ", thisNodeAttr),
            "} else {}",
            sep=paste0("\n", indent.by)
          )
        }
        if(identical(dotsOption, character())){
          if(length(thisNodeChildNames > 1)){
            childrenInNode <- paste(
              paste0(indent.by, "all.children <- list()"),
              paste0("for (thisNode in list(", paste0(thisNodeChildNames, collapse=", "), ")){"),
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
            paste0(indent.by, "if(!is.null(validity)){"),
            paste0(indent.by, "validXML(results, validity=validity)"),
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
                "#' @param ", thisNodeChildNames,
                " An objects of class \\code{XiMpLe.node} (or list of) to define \\code{<", thisNodeChildNames, ">} child nodes for this node. Ignored if \\cdoe{NULL.}"
              )
            } else {
              rxdocChildren <- NULL
            }
          } else {
              rxdocChildren <- paste0(
                "#' @param ... ",
                " An object of class \\code{XiMpLe.node} (or list of) to define \\code{<", thisNodeChildNames, ">} child nodes for this node. Ignored if empty."
              )
          }

          if(!identical(thisNodeAttrNames, character())){
            rxdocAttrs <- paste0(
              "#' @param ", thisNodeAttrNames,
              " Character string, used to set the \\code{", thisNodeAttrNames,"} attribute of this node. Ignored if \\cdoe{NULL.}"
            )
          } else {
            rxdocAttrs <- NULL
          }
          rxdocParams <- c(rxdocChildren, rxdocAttrs)
          
          if(isTRUE(checkValidity)){
          rxdocParams <- c(rxdocParams,
              paste0(
                "#' @param validity ",
                " An object of class \\code{XiMpLe.validity} to check the validity of this node. Ignored if \\cdoe{NULL.}"
              )
            )
          } else {}
          
          rxdoc <- paste(
            paste0("#' Generate <", thisNode, "> XML nodes"),
            "#'",
            paste0("#' Generates an object of class \\code{\\link[XiMpLe:XiMpLe.node-class]{XiMpLe.node}} with node name \\code{<", thisNode, ">}."),
            "#'",
            paste0(rxdocParams, collapse="\n"),
            "#' @return An object of class \\code{\\link[XiMpLe:XiMpLe.node-class]{XiMpLe.node}}.",
            "#' @export",
            sep="\n"
          )
          genResult <- paste0(rxdoc, "\n", genResult)
        } else {}

        return(genResult) 
      }
    )
    return(result)
  }
)
