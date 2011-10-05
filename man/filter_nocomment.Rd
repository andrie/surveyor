\name{filter_nocomment}
\alias{filter_nocomment}
\title{Removes a selected range of 'content-free' strings.}
\usage{
  filter_nocomment(x, remove =
  "^(No|no|NO|Nope|None|none|n.a.|NA|n/a).?$")
}
\arguments{
  \item{x}{A regular expression in character format}

  \item{remove}{A character string passed to grep()}
}
\description{
  Removes all strings that match the regular expression.
}

