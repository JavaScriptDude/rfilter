#!/usr/bin/env Rscript

#########################################
# .: rfilter.R :.
# Searches a flat file that includes a header column and outputs results based on a search expression
# .: Usage :.
# Usage: Rscript --vanilla rfilter.R <path_to_csv> <csv_delim> "<R query>" <file_to_output>
# query expressions:
# . <field> == <value>: Equals
# . <field> != <value>: Not Equals
# . <field> %like% <value>: Regexp match
# . <field> %glob% <value>: Glob (wildcard) match
# . &: logical AND
# . |: logical OR
# . Use parentheces to specify precedence
# .: Examples :.
# . rfilter -i test.ffr -d '\t' -q "type == 'f' & fsize > 8 & name %glob% 'myA*'" -o test_filter.ffr 
# . # This will return all the records where type column is 'f' AND fsize column is > 8 AND name column matches the glob (wildcard) of 'myA*'
# .: Dependencies :.
# . In R REPL:
# . % install.packages("data.table"); install.packages("getopt")
# .: Installing :.
# . append this into .bashrc:
#  . alias rfilter="Rscript --vanilla <path_to_script>/rfilter.R"
# .: Other :.
# Author: Timothy C. Quinn
# Home: https://github.com/JavaScriptDude/rfilter
# Licence: https://opensource.org/licenses/MIT
#########################################
# TODO:
# . Implement writing to stdout
# . Implement handling of dates
# . Make usage output nicer
########################################

##############################IMPORTS##############################

library(data.table)
library(getopt, quietly = TRUE)

##############################OPTIONS##############################

spec = matrix(c(
 'help'          , 'h', 0, "logical"   , "display help"
,'input'         , 'i', 1, "character" , "path to input file"
,'delim'         , 'd', 2, "character" , "field delimiter. Default is comma"
,'query'         , 'q', 1, "character" , "query expression to filer data"
,'output'        , 'o', 1, "character" , "path to output file."
,'subset'        , 's', 0, "logical"   , "return the records not included in query. Inverts the result."
), byrow=TRUE, ncol=5)

opts = getopt(spec)

if ( !is.null(opts$help) ) {
	cat(getopt(spec, usage=TRUE))
	q(status=1)
}

##############################CHECK UP/SET UP##############################

if ( is.null(opts$input ) ) { write("Error : -i|--input option can not be null",stderr()); write("\n",stderr()); cat(getopt(spec, usage=TRUE)); q(status=1) }
if ( is.null(opts$query ) ) { write("Error : -q|--query option can not be null",stderr()); write("\n",stderr()); cat(getopt(spec, usage=TRUE)); q(status=1) }


"%glob%" = function(vector, pattern) like(vector, glob2rx(pattern))

if(opts$delim=='\\t'){ opts$delim = '\t' }

# Open csv and read into table
df_all=data.table(read.csv(opts$input, sep=opts$delim, header=TRUE, stringsAsFactors=FALSE))
# Build / Parse query expression
expr = paste("df_filter=df_all[",ifelse(opts$subset,"!(",""),opts$query,ifelse(opts$subset,")",""),"]", sep='')
# Evaluate Query expression
eval(parse(text=expr))
# Write output
write.table(df_filter, opts$output, sep=opts$delim, row.names=FALSE)

cat("File written to: ", opts$output, "\n")

