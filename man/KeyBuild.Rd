\name{KeyBuild}
\alias{KeyBuild}

\title{Build Ecological Site Keys

}
\description{
Build an ecological site key for an MLRA, with an option to limit ecological sites by a geographic state. KeyBuild() produces a table listing abiotic criteria for every ecological site publically available in the EDIT database for the MLRA specified, limited by state if desired. Table columns include ecological site ID, property, and representative low and high values for each property. In the case of certain qualitative properties that are categorical rather than ordinal, a list of strings pulled from the ecological site description occupies the representative low column with NA values in the representative high column. Missing data will be skipped. Relational statements are used to create property ranges from representative lows and highs. The abiotic criteria include ecological site attributes that are useful for building a field-based ecological site key and for matching monitoring plot data to ecological sites.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
KeyBuild(
 mlra = NULL,
 stateset = FALSE
 state = NULL
)
}

\arguments{
  \item{mlra}{
Character string. Define a Major Land Resource Area (MLRA) of interest using the MLRA symbol convention, e.g., MLRA 36 should be written as "036X".
}
  \item{stateset}{
True/False. If True, the ecological site key will be constrained by a state. State should then be specified by  If False, ecological sites are constrained by MLRA and the ecological sites in the output table will span multiple states if the MLRA spans multiple states.
}
  \item{state}{
Character string. Define a geographic state using the conventional two-letter abbreviation.
}
}
}
}

\value{
A dataframe of ecological site properties with relational statements.
}

}

\examples{
KeyBuild(
mlra = "036X",
stateset = TRUE,
state = "NM"
)
}
