*    README
*
*    In a terminal, compile the main module using "ghc Main.lhs".
* Then run it using "./Main". The system will ask for the name of
* an input file you want to use. This Readme also serves as an input
* file, so you can enter "README.txt"
*    If you have any synatx or other errors in your input file, the
* system will raise them and stop.
* 	Provided the input file is formatted correctly, the system will
* show you the details of the CAES it constructed and, if you
* entered any queries (see below), the results to those queries.
*
*    *Details about formatting input files*
*
*    Every argument, standard and query must be on a single line.
* The list of assumptions must be on a separate line as well.
* There can be no whitespace between the "arg", "standard", etc. 
* keywords and the beginning of the line. In the rest of the line,
* parsers will generally be flexible about whitespace.
*
*    Arguments
* Format: "arg [<premises>] [<exceptions>] <conclusion> <weight>"
* Note that list elements are only separated by whitespace and are
* NOT surrounded with quotes. Negative propositions have a "-" sing
* in front. Arguments are not named.
* 	Names of proposition must start with a letter or a "-" sign and 
* can only contain letters and digits.

arg [kill intent] [] murder 0.8
arg [witness] [unreliable] intent 0.3
arg [witness2] [unreliable2] -intent 0.8

*    Assumptions
* Format "assumptions [<list of propositions>]"
* This list has the same format as those used in the declaration
* of arguments.

assumptions [kill witness witness2 unreliable2]

*    Proof standards
* Each conclusion must have a proof standards assigned to it.
* If the proof standard is the same for a proposition and its 
* negation, this has to be stated explicitly. All propositions
* which don't have a proof standard assigned to them can use a
* default one, assigned using the wildcard "_"
* Format: "standard <prop> <standard_name>", where the name of
* the standard can be one of those defined in Carneades.

standard intent beyond_reasonable_doubt 
standard -intent beyond_reasonable_doubt
standard _ scintilla

* Optional: Queries can be inserted into the input file.
*
*    Applicability queries
* Format "applicable <prop>"

applicable intent
applicable -intent

*    Acceptability queries
* Format "acceptable <prop>"

acceptable murder