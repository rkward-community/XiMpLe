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

depr_msg <- function(node_or_doc=c("node", "doc")){
  node_or_doc <- match.arg(node_or_doc)
  .Deprecated(msg=paste0(
      "Object class XiMpLe.", node_or_doc, " is deprecated.\nUse XiMpLe::as_XiMpLe_", node_or_doc, " to update old objects."
    )
  )
}

#' Deprecated functions and methods
#'
#' These functions were used in earlier versions of the package but are now either
#' replaced or removed.
#'
#' For methods this can also mean that the object class is deprecated and you are asked
#' to update old objects via \code{\link[XiMpLe:as_XiMpLe_node]{as_XiMpLe_node}} or
#' \code{\link[XiMpLe:as_XiMpLe_doc]{as_XiMpLe_doc}}.
#'
#' @param obj An object of deprecated classes \code{XiMpLe.node} or \code{XiMpLe.doc}.
#' @param name No longer used.
#' @param as.list No longer used.
#' @param find No longer used.
#' @param search No longer used.
#' @param value No longer used.
#' @param ... No longer used.
#' @rdname deprecated
#' @aliases
#'    XiMpLe-deprecated
#'    XMLName,XiMpLe.node-method
#' @export
#' @docType methods
#' @include zzz_is_get_utils.R
setMethod("XMLName",
  signature=signature(obj="XiMpLe.node"),
  function(obj){
    depr_msg("node")
    return(XMLName(as_XiMpLe_node(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLName<-,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLName<-",
  signature=signature(obj="XiMpLe.node"),
  function(obj, value){
    depr_msg("node")
    return("XMLName<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLAttrs,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLAttrs",
  signature=signature(obj="XiMpLe.node"),
  function(obj){
    depr_msg("node")
    return(XMLAttrs(as_XiMpLe_node(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLAttrs<-,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLAttrs<-",
  signature=signature(obj="XiMpLe.node"),
  function(obj, value){
    depr_msg("node")
    return("XMLAttrs<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLChildren,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLChildren",
  signature=signature(obj="XiMpLe.node"),
  function(obj){
    depr_msg("node")
    return(XMLChildren(as_XiMpLe_node(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLChildren,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLChildren",
  signature=signature(obj="XiMpLe.doc"),
  function(obj){
    depr_msg("doc")
    return(XMLChildren(as_XiMpLe_doc(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLChildren<-,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLChildren<-",
  signature=signature(obj="XiMpLe.node"),
  function(obj, value){
    depr_msg("node")
    return("XMLChildren<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLChildren<-,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLChildren<-",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, value){
    depr_msg("doc")
    return("XMLChildren<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLValue,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLValue",
  signature=signature(obj="XiMpLe.node"),
  function(obj){
    depr_msg("node")
    return(XMLValue(as_XiMpLe_node(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLValue<-,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLValue<-",
  signature=signature(obj="XiMpLe.node"),
  function(obj, value){
    depr_msg("node")
    return("XMLValue<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLFile,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLFile",
  signature=signature(obj="XiMpLe.doc"),
  function(obj){
    depr_msg("doc")
    return(XMLFile(as_XiMpLe_doc(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLFile<-,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLFile<-",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, value){
    depr_msg("doc")
    return("XMLFile<-"(obj=as_XiMpLe_node(obj), value=value))

    slot(obj ,"file") <- value
    return(obj)
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLDecl,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLDecl",
  signature=signature(obj="XiMpLe.doc"),
  function(obj){
    depr_msg("doc")
    return(XMLDecl(as_XiMpLe_doc(obj)))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLDecl<-,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLDecl<-",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, value){
    depr_msg("doc")
    return("XMLDecl<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLDTD,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLDTD",
  signature=signature(obj="XiMpLe.doc"),
  function(obj){
    depr_msg("doc")
    return(XMLDTD(as_XiMpLe_doc(obj)))

    return(slot(obj ,"dtd"))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLDTD<-,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLDTD<-",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, value){
    depr_msg("doc")
    return("XMLDTD<-"(obj=as_XiMpLe_node(obj), value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLScan,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLScan",
  signature=signature(obj="XiMpLe.node"),
  function(obj, name, as.list=FALSE){
    depr_msg("node")
    return(XMLScan(as_XiMpLe_node(obj), name=name, as.list=as.list))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLScan,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLScan",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, name, as.list=FALSE){
    depr_msg("doc")
    return(XMLScan(as_XiMpLe_doc(obj), name=name, as.list=as.list))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLScan<-,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLScan<-",
  signature=signature(obj="XiMpLe.node"),
  function(obj, name, value){
    depr_msg("node")
    return("XMLScan<-"(obj=as_XiMpLe_node(obj), name=name, value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLScan<-,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLScan<-",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, name, value){
    depr_msg("doc")
    return("XMLScan<-"(obj=as_XiMpLe_node(obj), name=name, value=value))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLScanDeep,XiMpLe.node-method
#' @docType methods
#' @include 00_class_01_XiMpLe.node.R
setMethod("XMLScanDeep",
  signature=signature(obj="XiMpLe.node"),
  function(obj, find, search){
    depr_msg("node")
    return(XMLScanDeep(as_XiMpLe_node(obj), find=find, search=search))
  }
)

#' @rdname deprecated
#' @aliases
#'    XMLScanDeep,XiMpLe.doc-method
#' @docType methods
#' @include 00_class_02_XiMpLe.doc.R
setMethod("XMLScanDeep",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, find, search){
    depr_msg("doc")
    return(XMLScanDeep(as_XiMpLe_doc(obj), find=find, search=search))
  }
)

#' @rdname deprecated
#' @include 01_method_01_pasteXML.R
#' @aliases
#'    pasteXML,XiMpLe.node-method
setMethod("pasteXML",
  signature=signature(obj="XiMpLe.node"),
  function(obj, ...){
    depr_msg("node")
    pasteXML(
      as_XiMpLe_node(obj),
      ...
    )
  }
)


#' @rdname deprecated
#' @include 01_method_01_pasteXML.R
#' @aliases
#'    pasteXML,XiMpLe.doc-method
setMethod("pasteXML",
  signature=signature(obj="XiMpLe.doc"),
  function(obj, ...){
    depr_msg("doc")
    pasteXML(
      as_XiMpLe_node(obj),
      ...
    )
  }
)

