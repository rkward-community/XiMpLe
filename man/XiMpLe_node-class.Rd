% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_class_01_XiMpLe.node.R, R/zzz_is_get_utils.R
\docType{class}
\name{XiMpLe_node,-class}
\alias{XiMpLe_node,-class}
\alias{XiMpLe_node_old}
\alias{XiMpLe.node-class}
\alias{XiMpLe.node,-class}
\alias{XiMpLe_node-class}
\alias{XiMpLe_node}
\alias{is.XiMpLe.node}
\alias{is.XiMpLe_node}
\alias{as_XiMpLe_node}
\alias{as_XiMpLe_node,XiMpLe.node-method}
\alias{as_XiMpLe_node,-methods}
\title{Classes XiMpLe_node and XiMpLe.node (old)}
\usage{
is.XiMpLe.node(x)

is.XiMpLe_node(x)

as_XiMpLe_node(obj, extra = list(), version = 2)

\S4method{as_XiMpLe_node}{XiMpLe.node}(obj, extra = list(), version = 2)
}
\arguments{
\item{x}{An arbitrary \code{R} object.}

\item{obj}{An object of old class \code{XiMpLe.node}.}

\item{extra}{A list of values to set the \code{extra} slot. Note that this will be applied recursively on child nodes also.}

\item{version}{Integer numeric,
      to set the \code{version} slot. Note that this will be applied recursively on child nodes also.}
}
\description{
These classes are used to create DOM trees of XML documents,
      like objects that are returned
by \code{\link[XiMpLe:parseXMLTree]{parseXMLTree}}.
}
\details{
Class \code{XiMpLe.node} is the older one,
      \code{XiMpLe_node} was introduced with \code{XiMpLe} 0.11-1. It has two new slots
(\code{extra} and \code{version}) that would have made it impossible to load old objects without issues. \code{XiMpLe_node}
inherits from \code{XiMpLe.node}. You can convert old objects into valid new ones using the \code{as_XiMpLe_node} method and
are also advised to do so,
      as the \code{XiMpLe.node} class might become deprecated in future releases.

There are certain special values predefined for the \code{name} slot to easily create special XML elements:
\describe{
    \item{\code{name=""}}{If the name is an empty character string,
      a pseudo node is created,
      \code{\link[XiMpLe:pasteXMLNode]{pasteXMLNode}} will paste its \code{value} as plain text.}
    \item{\code{name="!--"}}{Creates a comment tag, i.e.,
      this will comment out all its \code{children}.}
    \item{\code{name="![CDATA["}}{Creates a CDATA section and places all its \code{children} in it.}
    \item{\code{name="*![CDATA["}}{Creates a CDATA section and places all its \code{children} in it,
      where the CDATA markers are
      commented out by \code{/* */}, as is used for JavaScript in XHTML.}
}

A contructor function \code{XiMpLe_node(...)} is available to be used instead of \code{new("XiMpLe_node",
      ...)}.

There's also \code{XiMpLe_node_old(...)} to be used instead of \code{new("XiMpLe.node",
      ...)}, but you should not use that any longer.
}
\section{Slots}{

\describe{
\item{\code{name}}{Name of the node (i.e.,
      the XML tag identifier). For special names see details.}

\item{\code{attributes}}{A list of named character values,
      representing the attributes of this node.
Use \code{character()} as value for empty attributes.}

\item{\code{children}}{A list of further objects of class XiMpLe.node,
      representing child nodes of this node.}

\item{\code{value}}{Plain text to be used as the enclosed value of this node. Set to \code{value=""} if you
want a childless node to be forced into an non-empty pair of start and end tags by \code{\link[XiMpLe:pasteXMLNode]{pasteXMLNode}}.}

\item{\code{extra}}{A named list that can be used to store additional information on a node (\code{XiMpLe_node} only).
The only value with a noticeable effect as of now (version 2) is:
 \describe{
   \item{\code{shine}: }{A numeric integer value between 0 and 2,
      overwriting the \code{shine} value of, e.g., \code{\link[XiMpLe:pasteXML]{pasteXML}}
     for this particular node.}
 }}

\item{\code{version}}{A numeric integer,
      currently defaults to 2 (\code{XiMpLe_node} only). This is a version number that can be used in the future
in combination with the added \code{extra} slot. Should that get more supported values like \code{shine} that are interpreted by package methods,
the version number will be increased and the differences documented here. You shouldn't set it manually.}
}}

\keyword{classes}
