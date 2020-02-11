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
# . % install.packages("data.table"); install.packages("getopt"); install.packages("tidyverse")
# .: Installing :.
# . append this into .bashrc:
#  . alias rfilter="Rscript --vanilla <path_to_script>/rfilter.R"
# .: Other :.
# Author: Timothy C. Quinn
# Home: https://github.com/JavaScriptDude/rfilter
# Licence: https://opensource.org/licenses/MIT
#########################################
# TODO:
# [.] Implement handling of dates
# [.] Make usage output nicer
# [.] Find way to make quoting output be less agressive with double quotes
#     . EG: Fields that don't contain delim char, carriage returns or quotes should not require double quotes around them (like Excel, Python csv etc...)
#     . Question posted: https://stackoverflow.com/questions/60176317/r-write-rfc4180-compatible-flat-files-with-less-agressive-quoting

########################################

##############################IMPORTS##############################

library(data.table)
library(getopt, quietly = TRUE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)


PRINTF_MODE_SINGLE = 1
PRINTF_MODE_NEW_COL = 2

##############################OPTIONS##############################

spec = matrix(c(
 'help'          , 'h', 0, "logical"   , "display help"
,'input'         , 'i', 1, "character" , "path to input file"
,'delim'         , 'd', 2, "character" , "field delimiter. Default is comma"
,'query'         , 'q', 1, "character" , "query expression to filer data"
,'output'        , 'o', 2, "character" , "path to output file. If not specified, will go to standard out"
,'subset'        , 's', 0, "logical"   , "return the records not included in query. Inverts the result."
,'printf'        , 'p', 2, "character" , "| separated list of: <R_sprintf_format_str>|<column_1>[|<column_2>,...]"
,'printf_mode'   , 'm', 2, "integer"   , "1 (default) = to stdout with no header; 2 = new column to table"
,'columns'       , 'c', 2, "character" , "| seprated list of columns to output"
), byrow=TRUE, ncol=5)

opts = getopt(spec)

if ( !is.null(opts$help) ) {
	cat(getopt(spec, usage=TRUE))
	q(status=1)
}

##############################CHECK UP/SET UP##############################

if ( is.null(opts$input) ) { write("Error : -i|--input option can not be null",stderr()); write("\n",stderr()); cat(getopt(spec, usage=TRUE)); q(status=1) }
if ( is.null(opts$query) ) { write("Error : -q|--query option can not be null",stderr()); write("\n",stderr()); cat(getopt(spec, usage=TRUE)); q(status=1) }
if ( !is.null(opts$printf) ) { 
	l = unlist(strsplit(opts$printf, "[|]"))
	opts$printf_string_full = l
	if ( length(l) < 2 ) {
		write("Error : -p|--printf string is invalid",stderr()); write("\n",stderr()); cat(getopt(spec, usage=TRUE)); q(status=1)
	}
	opts$printf_string = l[1]
	opts$printf_columns = l[-1]


	if ( is.null(opts$printf_mode) ) { 
		opts$printf_mode = PRINTF_MODE_SINGLE
	} else {
		if (opts$printf_mode > PRINTF_MODE_NEW_COL) {
			write("Error : -m|--printf_mode can be either 1 (default) or 2",stderr()); write("\n",stderr()); cat(getopt(spec, usage=TRUE)); q(status=1)
		}
	}
	
}

out_columns = NULL
if ( !is.null(opts$columns) ) { 
	out_columns = unlist(strsplit(opts$columns, "[|]"))
}
to_stdout = ifelse(is.null(opts$output), TRUE, FALSE)

show_column_names = TRUE


"%glob%" = function(vector, pattern) like(vector, glob2rx(pattern))
`%fmt%` <- function(x, y) { do.call(sprintf, c(list(x), y)) }
getvals <- function(rec, fields) { 
	l = list()
	for (i in 1:length(fields)[1]) {
		l[i] <- rec[[fields[i]]] 
	}
	l
}


if(opts$delim=='\\t'){ opts$delim = '\t' }

# Open csv and read into table
df_all=data.table(read.csv(opts$input, sep=opts$delim, header=TRUE, stringsAsFactors=FALSE))
# Build / Parse query expression
expr = paste("df=df_all[",ifelse(opts$subset,"!(",""),opts$query,ifelse(opts$subset,")",""),"]", sep='')
# Evaluate Query expression
eval(parse(text=expr))
#cat("df type = ", typeof(df), "\n"); q(status=1)

if (!is.null(opts$printf)) {
	
	l = list()
	for (i in 1:dim(df)[1]) {
		vals = getvals(df[i], opts$printf_columns)
		l[[i]] <- do.call(sprintf, c(opts$printf_string, vals))
	}
	df$rfilter_printf <- l[1]

	if ( opts$printf_mode == PRINTF_MODE_SINGLE ) {
		out_columns = c('rfilter_printf')
		show_column_names = FALSE
	}
		
}

# Choose output file or temp for stdout
out_file =  ifelse(to_stdout, tempfile(pattern = "file", tmpdir = tempdir(), fileext = ""), opts$output)

# conver to data.frame
setDF(df)

if ( is.null(out_columns) ) {
	write.table(df, file=out_file, sep=opts$delim, row.names=FALSE, col.names=show_column_names)
} else {
	write.table(df[,out_columns], file=out_file, sep=opts$delim, row.names=FALSE, col.names=show_column_names, quote=(length(out_columns) > 1))
}

if ( to_stdout ) {
	con=file(out_file,open="r")
	while ( TRUE ) {
		line = readLines(con, n = 1)
		if ( length(line) == 0 ) {
			break
		}
		cat(line, "\n")
	}
} else {
	cat("File written to: ", opts$output, "\n")
}

