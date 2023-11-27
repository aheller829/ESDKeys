\name{SiteKeyBuild}
\alias{SiteKeyBuild}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build Ecological Site Keys
%%  ~~function to do ... ~~
}
\description{
Build an ecological site key for an MLRA by pulling ecological site attributes from the Ecosystem Dynamics Interpretive Tool (EDIT; https://edit.jornada.nmsu.edu/), with an option to limit ecological sites by a geographic state. SiteKeyBuild() produces a table listing abiotic criteria for every ecological site publically available in the EDIT database for the MLRA specified, limited by state if desired. Table columns include ecological site ID, property, and representative low and high values for each property. In the case of certain qualitative properties that are categorical rather than ordinal, a list of strings pulled from the ecological site description occupies the representative low column with NA values in the representative high column. Missing data will be skipped. Relational statements are used to create property ranges from representative lows and highs. The abiotic criteria include ecological site attributes that are useful for building a field-based ecological site key and for matching monitoring plot data to ecological sites.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SiteKeyBuild(
 mlra = NULL,
 stateset = FALSE
 state = NULL
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mlra}{Character string. The desired Major Land Resource Area (MLRA) to use for gathering ecological sites. Use the standard MLRA symbol convention, as is used in the EDIT database.
%%     ~~Describe \code{x} here~~
}
 \item{stateset}{True/False. If FALSE, ecological sites from the whole MLRA of interest are gathered and will span multiple states if the MLRA spans multiple states. If TRUE, ecological sites will be constrained by state within MLRA.
}
 \item{state}{Character string. If stateset = TRUE, list state of interest using conventional two-letter state abbreviation.
 }
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe of ecological site properties with relational statements.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
SiteKeyBuild(
mlra = "036X",
stateset = TRUE,
state = "NM"
)

SiteKeyBuild(
mlra = "042B",
stateset = FALSE,
state = NULL
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
