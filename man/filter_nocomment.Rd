\name{filter_nocomment}
\alias{filter_nocomment}
\title{Removes a selected range of 'content-free' strings from x...}
\usage{filter_nocomment(x, remove="^(No|no|NO|Nope|None|none|n.a.|NA|n/a).?$")}
\description{Removes a selected range of 'content-free' strings from x}
\details{Removes all strings that removes all strings that match the regular 
expression "remove" from x}
\arguments{\item{x}{A regular expression in character format}
\item{remove}{A character string passed to grep()}}
