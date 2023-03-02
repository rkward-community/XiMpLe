## ---- include=FALSE, cache=FALSE---------------------------------------------------
library(XiMpLe)

## ---- set-options, echo=FALSE, cache=FALSE-----------------------------------------
options(width=85)

## ----------------------------------------------------------------------------------
XMLNode("useless")

## ----------------------------------------------------------------------------------
XMLNode("other", foo="bar")

## ----------------------------------------------------------------------------------
XMLNode("other", attrs=list(foo="bar"))

## ----------------------------------------------------------------------------------
XMLNode("other", "", foo="bar")

## ----------------------------------------------------------------------------------
XMLNode("other", attrs=list(foo="bar"), .children=list(""))

## ----------------------------------------------------------------------------------
XMLNode(
  "other",
  "this text is the child of the \"other\" node.",
  "it can have multiple entries.",
  foo="bar"
)

## ----------------------------------------------------------------------------------
XMLNode("!--", "following is an empty node named \"useless\"")

## ----------------------------------------------------------------------------------
sample_XML_a <- XMLNode("a", "klick here!", href="http://example.com", target="_blank")
sample_XML_body <- XMLNode("body", sample_XML_a)
sample_XML_html <- XMLNode("html", XMLNode("head", ""), sample_XML_body)
(sample_XML_tree <- XMLTree(sample_XML_html,
  xml=list(version="1.0", encoding="UTF-8"),
  dtd=list(doctype="html", decl="PUBLIC",
  id="-//W3C//DTD XHTML 1.0 Transitional//EN",
  refer="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")))

## ----------------------------------------------------------------------------------
useless_node <- XMLNode("useless")
pasteXML(useless_node)

## ---- eval=FALSE-------------------------------------------------------------------
#  cat(pasteXML(sample_XML_tree), file="example.html")

## ---- eval=FALSE-------------------------------------------------------------------
#  cat(pasteXML(sample_XML_tree, shine=2))

## ---- eval=FALSE-------------------------------------------------------------------
#  sample_XML_parsed <- parseXMLTree("example.html")

## ----------------------------------------------------------------------------------
my.XML.stuff <- c("<start>here it begins","</start>")
parseXMLTree(my.XML.stuff, object=TRUE)

## ----------------------------------------------------------------------------------
node(sample_XML_tree, node=list("html","body","a"), what="attributes")

## ----------------------------------------------------------------------------------
node(sample_XML_tree, node=list("html","body","a"), what="value")

## ----------------------------------------------------------------------------------
node(sample_XML_tree, node=list("html","body","a"), what="attributes", element="href") <- "http://example.com/foobar"
node(sample_XML_tree, node=list("html","body","a"), what="attributes", element="target") <- NULL
sample_XML_tree

## ----------------------------------------------------------------------------------
gen_tag_functions(c("html", "head", "body", "a"))

# see them in action
(sample_XML_tree2 <- html_(
  head_(),
  body_(
    a_(href="http://example.com", target="_blank", "klick here!")
  )
))

## ----------------------------------------------------------------------------------
attach(list(), name="XiMpLe_wrappers")
gen_tag_functions(tags=c("p", "div"), envir=as.environment("XiMpLe_wrappers"))

## ----------------------------------------------------------------------------------
cat(pasteXML(sample_XML_tree2, as_script=TRUE))

