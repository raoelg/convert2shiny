###
###
### 2013, 2015 (c) Raoul Grasman, all rights reserved
###
###
### convert2shiny
###
### This routine takes an almost arbitrary R script (that is structured according to a few rules)
### and converts it into the necessary files for a Shiny app: server.R and ui.R.
### This makes it very simple to convert an analysis or simulation script into an online
### web app without the need to (know how to) write the overhead code needed for Shiny Apps.
###
### The assumed structure of the script file is very simple:
###
###       <R code, e.g. to define functions>
###     ##! INPUT
###       <global variable definitions that modify how the script is executed>
###
###     ##! MAIN COMPUTATIONS
###
###       # compute code segment 1
###       <R code of which the computations depend on the global variables>
###       # compute code segment 2
###       <R code of which the computations depend on the global variables>
###       # compute code segment ...
###       <R code of which the computations depend on the global variables>
###       ...
###
###     ##! MAIN PAGE
###
###       # [plot|tabulate|dump|print|data] code segment 1
###       <R code of which the result is textual or graphical output>
###       # [plot|tabulate|dump|print|data] code segment 2
###       <R code of which the result is textual or graphical output>
###       # [plot|tabulate|dump|print|data] code segment ...
###       <R code of which the result is textual or graphical output>
###       ...
###
### The comment lines starting with ##! are required. In the INPUT section, each global
### variable should be defined on a seperate line and initialized by assigning a value. These
### global variables influence the outcome of the computations and output further down
### the script. Behind each assignment of a global variable a comment should elucidate the
### meaning of the global variable (which is used in the shiny app as a descriptor), followed
### by a comma, followed by an R expression that specifies range of possible values. For example:
###
###   ##! INPUT
###   plotHist = TRUE    # Display a histogram?,
###   nrsamp = 100       # Number of simulated observations, seq(1, 500, by = 10)
###   method = "ML"      # Which method?, c("ML", "OLS", "GLS","REML")
###                      # ['ML' is the preferred method]
###   vars = c("a", "b") # Which variable should be used?, names(myData)
###
### In the first example 'plotHist' is a logical variable and Shiny will interface a checkbox. In
### the second example 'nrsamp' is numeric and Shiny will allow values between 1 and 500 in
### steps of 10. The third example Shiny will let the use choose from the listed character strings.
### The third example has a comment on the line just below the definition of 'method'. This comment
### will be presented to the user in the Shiny interface as additional help. For files, a (vector of)
### accepted mime-type(s) have to be specified (currently only 'text/<type>' and
### 'application/<type>' are accepted; so for instance, "c('text/plain','text/csv',
### 'application/x-javascript','application/vnd.ms-excel')", etcera). The last example lets Shiny
### present the user with a list from which multiple values can be selected; the list in this case
### is obtained from 'myData' which is (and must be) defined in the MAIN COMPUTATIONS section, e.g.,
### in response to a file that is uploaded.
### In the MAIN COMPUTATIONS and MAIN PAGE sections, the comments at the start of each
### code segment are crucial (these are used to define function names in the server.R and
### ui.R files and therefore should be unique). In the MAIN PAGE section of the script
### the comments that start with plot, tabulate, dump, print, or data will render appropriately
### formatted output (i.e., a picture for plot, a table for tabulate, plain text for dump).
###
### If 'myscript.R' is the name of a file that contains a script that adheres to these
### conventions, then the script is turned into an app with the simple call
###
### > convert2shiny('~/path/to/myscript.R')
###
### The return value is a character vector containing the relative paths to the generated
### files. By default, the files are generated in the same directory as the directory in which
### 'myscript.R' is located.
###


convert2shiny <- function(
	scriptfile = '~/Dropbox/shiny parser/example header based on shiny examples.R',
	uifile = file.path(dirname(scriptfile), "ui.R"),
	serverfile = file.path(dirname(scriptfile), "server.R"))
{
	if (!exists("errorHandler")) errorHandler = function(e) print(e)

	### Start parsing and analyzing the script
	R = readLines(scriptfile)

	idx = unique(c(grep("##!", R), length(R)+1))
	idx = data.frame(label=R[idx[-length(idx)]], start = idx[-length(idx)], end = idx[-1]-1)


	R.parts = apply(idx, 1, function(x) R[x['start']:x['end']])
	names(R.parts) = gsub("\\W*(INPUT|MAIN)*", "\\1", idx[,1])

  script.header = R[1:(idx[1,'start']-1)]

	##
	###  Parse the headerPanel
	##

	headPanel = do.call(call, list("headerPanel", sub("\\.[^\\.]+$", "", basename(scriptfile))))

	##
	###  Parse the sidebarPanel
	##
	inp <- R.parts$INPUT

	# collapse help multiple consecutive help lines into a single line
	hlp = regexpr("^\\s+#", inp ) > 0 # which are help lines (defined as those which start with at least one space and then a # sign)
	if (any(hlp)) {
		hlp = cbind(start=which(c(0,diff(hlp))==1), end=which(c(diff(hlp),0)==-1)) # determine start and end indices for each help text segment (defined as sets of consecutive comment lines)
		ind = lapply(1:NROW(hlp), function(i) hlp[i,1]:hlp[i,2]) # generate line indices for each help text segment
		for (i in seq_along(ind)) {
			# collapse each help segment into a single line:
			tmp = sapply(strsplit(inp[ind[[i]]], "\\s+#\\s+"), function(x) x[2]) # get only the part of these line after the first # and before the second # (unless escaped)
			helptext = paste(na.omit(tmp), collapse="\n"); # help text in one string (retain the inserted newlines (!!))
			inp[ind[[i]][1]] = paste(" #", helptext); # collapsed help text into the first line of this help text segment
			inp[ind[[i]][-1]] = ""; # empty remaining lines of this help text segment
		}
	}

	inp = strsplit(inp, "\\s#\\s")

	# parse from each line the assignment, and the title and valid values specification that come after the # and are separated by the first , after #
	inp.parsed = lapply(inp, function(line) {
		xpr = parse(text = line[1])
		info = NULL
		if (length(xpr) > 0 && length(xpr[[1]])==3) {
			# this should be an expression of the form <name> = <value>
			# to determine the input type we need the following info
			name = xpr[[1]][[2]];
			value = eval(xpr[[1]][[3]]);
			modus = mode(value)
			info = list(expr=xpr[[1]], name=name, value=value, mode=modus)
			if (length(line) > 1) {
				ss = strsplit(sub(",","|",line[2]),"\\|")[[1]];  
				info = c(info, list(title=ss[1], valid= if (length(ss)>1 && length(val <- parse(text=ss[2]))>1) val[[1]] else NA))
			}
		}
		else if(length(line) > 1) {
			info = list(helptext = line[2])
		}
		info
	})
	inp.parsed = inp.parsed[!sapply(inp.parsed,is.null)]

	# Table for recognizing inputElements
	#  v = valid
	#  r = required
	#  x = invalid
	#
	#                  |  is.logical(value)  |  is.numeric(value)  |  is.array(value)  |  is.integer(valid)  | is.double(valid) | is.character(valid) | is.na(valid) | is.array(valid) | is.character(value) | is.null(names(valid)) | is.character(helptext) |
	#  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	#  sliderInput     |        x            |        r            |         v         |         v           |         v        |          x          |      x       |        r        |          x          |          r            |           x            |
	#  selectInput     |        v            |        x            |         x         |         v           |         x        |          v          |      x       |        r        |          v          |          r            |           x            |
	#  checkboxInput   |        r            |        x            |         x         |         x           |         x        |          x          |      r       |        x        |          x          |          r            |           x            |
	#  radioButtons    |        v            |        v            |         x         |         v           |         v        |          v          |      x       |        r        |          v          |          x            |           x            |
	#  helpText        |        x            |        x            |         x         |         x           |         x        |          x          |      x       |        x        |          x          |          r            |           r            |
	#  numericInput    |        x            |        r            |         x         |         x           |         x        |          x          |      r       |        x        |          x          |          r            |           x            |
	#  fileInput       |        x            |        x            |         x         |         x           |         x        |          r          |      x       |        v        |          r          |          r            |           x            |
	#
	#  It's a little bit difficult to distinguish fileInput from selectInput. To do so we need to
	#  check if valid only contains valid MIME types, a limitation of this is however that we cannot
	#  distinguish the case where valid = "".

	inputElements = lapply(inp.parsed, function(input) {
		v = if (!is.null(input$value)) eval(input$value)
		w = if (!is.null(input$valid)) eval(input$valid);
		inputElem =
			if (!is.null(input$helptext) ) {
				do.call(call, list(name = "helpText", input$helptext))
			}
			else if(is.logical(v) && is.na(w)||is.null(w)) {
				do.call(call, list(name = "checkboxInput", as.character(input$name), input$title, value = v))
			}
			else if(is.numeric(v) && is.na(w)||is.null(w)) {
				do.call(call, list(name = "numericInput", as.character(input$name), input$title, value = v))
			}
			else if(is.character(v) && is.character(v) && is.null(names(w)) && is.character(w) && all(regexpr("(text|application)\\/\\w.+", w)>0)) {
				do.call(call, list(name = "fileInput", as.character(input$name), input$title, accept=w))
			}
			else if(is.numeric(v) && is.numeric(w) && length(w)>1 && is.null(names(w))) {
				do.call(call, list(name = "sliderInput", as.character(input$name), input$title, value=v, min=min(w), max=max(w), step=diff(w)[1]))
			}
			else if(!is.numeric(v) && length(w) > 0 && is.null(names(w))) {
        if (length(v) > 1)
          do.call(call, list(name = "selectInput", as.character(input$name), input$title, choices=w, selected = v, multiple = TRUE, selectize = TRUE))
        else
          do.call(call, list(name = "selectInput", as.character(input$name), input$title, choices=w, selected = v))
			}
			else if(length(w) > 1 && !is.null(names(w))) {
				do.call(call, list(name = "radioButtons", as.character(input$name), input$title, choices=w))
			}
		inputElem
	})


	##
	### Parse mainPanel
	##

	# utility to split the code into segments
	splitcode = function(code,pattern="^#\\s+\\w"){if(!is.character(code)) return(list()); idx=diff(c(1,grep(pattern, code),length(code)+1)); split(code, rep(seq_along(idx), idx))[-1]}
	# we will need the (global) input variable names
	globvarnms = unlist(sapply(inp.parsed, function(x) as.character(x$name)));


	# First, parse the MAINCOMPUTATIONS and turn them into `reactive' code segments
	computations = R.parts$MAINCOMPUTATIONS
	comp = splitcode(computations, "#\\s+\\w")
	subs = as.list(sapply(globvarnms, function(x) parse(text=paste("input",x,sep="$"))))
	comp.parsed = sapply(comp, function(code) {
			expr = parse(text=c("{", code, "}"))[[1]]
			nc = length(expr) # number of command statements in the code (between {})
			local.vars = setdiff(x = all.vars(expr), globvarnms)
			returnValue = structure(lapply(local.vars, function(x) call("if", call("exists", x), as.name(x), NULL)), .Names = local.vars) # = named list of all local variables created in this segment of code if they exist
			expr[[nc+1]] = call("return", returnValue)
			expr = do.call(substitute, list(expr, subs)) # prepend all input variable names with `input$'
			call("reactive", call("tryCatch", expr, error = errorHandler))
	})
	names(comp.parsed) = make.names(sapply(comp, function(x) x[1]), unique=TRUE) # names will be used as names for generated reactive functions

  # All reactive code segments may create new variables that in the R script file are global. Hence these should be
  # available in all subsequent reactive code segments. So we wrap each reactive code segment in a with(..., {}) statement
  # such that its code is executed in a context where all these variables are available. Furthermore, we collect calls
  # to these reactive code segments in the variable reactCompsCalls so that these can be used in the renderers generated
  # for the MAINPAGE section (where all these variables also have to be available).
	reactCompsCalls = c() # list that will contain calls to the reactive code segments
	for (segm in 1:length(comp.parsed)) {
	  reactCompsCalls = c(reactCompsCalls, call(names(comp.parsed)[segm]))
	  if (length(comp.parsed) > 1) {
	    # the code segment in the reactive statement is enclosed in a with(..., {}) statement
	    # which makes the results from all previous reactive code segments available to the current
	    envexpr = as.call(c(as.name("c"), reactCompsCalls[-segm]))
	    comp.parsed[[segm]][[2]] = call("{", call("with", envexpr, comp.parsed[[segm]][[2]]))
	  }
	}



	# Second, parse the MAINPAGE and turn them into output rendering calls. The
	# rendering type is determined by the first (key) word after the hash (namely
	# `print', `show', `plot', or `tabulate'). The rendering defaults to renderText.
	outputs = R.parts$MAINPAGE
	outp = splitcode(outputs, "^#\\s+\\w")
	outp.parsed = sapply(outp, function(code) {
		comment = gsub("^#\\s+|\\s*$", "", code[1])
		    rend = gsub("^(print|show|plot|tabulate|data).+","\\1", comment);
		renderer = switch(rend, print="renderPrint", plot="renderPlot", tabulate="renderTable", show="renderText", data="renderDataTable", "renderPrint");
		outputer = switch(rend, print="textOutput", plot="plotOutput", tabulate="tableOutput", data="dataTableOutput", "verbatimTextOutput")
		expr = parse(text=c("{", code, "}"))[[1]]
		expr = do.call(substitute, list(expr, subs)) # prepend all input variable names with `input$'
		envexpr = as.call(c(as.name("c"), reactCompsCalls))
		cl = call(renderer, call("{", call("tryCatch", call("with", envexpr, expr), error = errorHandler)))
		attr(cl, "caption") = comment
		cl
	})
  names(outp.parsed) = make.names(sapply(outp.parsed, function(x) attr(x,'caption')), unique=TRUE) # names will be used as slotnames in output variable by which the UI's mainPanel calls their rendering


  # Create the main pannel function from the output renderers
  outputRenderers = outp.parsed # the output renderers are simply the parsed output segments
	names(outputRenderers) = names(outp.parsed)
	outputElements = unlist(lapply(names(outputRenderers), function(name) {
			caption = attr(outputRenderers[[name]], "caption")
		    rend = gsub("^(print|show|plot|tabulate|data|ui).+","\\1", caption);
			outputer = switch(rend, print="textOutput", plot="plotOutput", tabulate="tableOutput", data="dataTableOutput", ui="uiOutput", "verbatimTextOutput")
			c(call("h4", caption), call(outputer, name))
		}), recursive=FALSE)

	mnPanel = as.call(c(as.name("mainPanel"), outputElements))

	# From the inp.parsed extract the dynamically define UI elements
	dynoutp.parsed = lapply(inp.parsed, function(input) {
	  if(!is.null(input$valid) && length(all.vars(input$valid)) > 0) {
	  	name = as.character(input$name); lab = input$title; v = as.name("value"); w = as.name("valid")
	    call.sliderInput   = as.call(c(as.name("sliderInput"),  inputId = name, label = lab, value= v, min = call('min',w), max = call('max',w), step = expression(diff(valid)[1])[[1]])) # call('[',as.call(c(as.name('diff'), w)),1))
	    call.selectInput   = as.call(c(as.name("selectInput"),  inputId = name, label = lab, selected= as.name(name), choices = w, multiple=as.name('multiple'), selectize = as.name('multiple')))
	    call.radioButtons  = as.call(c(as.name("radioButtons"), inputId = name, label = lab, selected= as.name(name), choices = w))

	    call.selectInput = call("if", as.name("multiple"), call.selectInput, call.selectInput[1:5]) # only use 'selectize' when multiple=TRUE (otherwise ui appearence changes)
	    cl = call('if', expression(length(valid) > 1 && !is.null(names(valid)))[[1]], call("{", call.radioButtons))
	    cl = call('if', expression(!is.numeric(value) && length(valid) > 0 && is.null(names(valid)))[[1]], call("{", call.selectInput), cl)
	    cl = call('if', expression(is.numeric(value) && is.numeric(valid) && is.null(names(valid)))[[1]], call("{", call.sliderInput), cl)
      cl = c(call("=", as.name('valid'), input$valid), call("=", as.name('value'), input$value), call("=", as.name('multiple'), expression(length(value) > 1)[[1]]), cl)
      cl = as.call(c(as.name("{"), cl))
	    cl = do.call(substitute, list(cl, subs)) # prepend all input variable names with `input$'
	    envexpr = as.call(c(as.name("c"), reactCompsCalls))
	    cl = call("renderUI", call("{", call("tryCatch", call("with", envexpr, cl), error = errorHandler) ))
	    structure(cl, caption = input$title)
	  } else NULL
	})
	names(dynoutp.parsed) = make.names(sapply(inp.parsed, function(x) x$title), unique=TRUE)
	dynoutp.parsed = dynoutp.parsed[!sapply(dynoutp.parsed, is.null)] # remove NULL elements


	# Create the sidebar pannel function from the input Elements  and the dynamic output elements
	dynOutputRenderers = dynoutp.parsed # the output renderers are simply the parsed dynamic output segments
	names(dynOutputRenderers) = names(dynoutp.parsed)
	dynOutputElements = unlist(lapply(names(dynOutputRenderers), function(name) {
	  caption = attr(dynOutputRenderers[[name]], "caption")
	  call("uiOutput", name)
	}), recursive=FALSE)


	idx = sapply(inp.parsed, function(x) is.null(x$valid) || length(all.vars(x$valid)) == 0)
	#inputElements = inputElements[idx] # retain only non dynamic input elements
	if (length(dynOutputElements) > 0) {
    inputElements[!idx] = dynOutputElements;
  }
	sbPanel = as.call(c(as.name("sidebarPanel"), inputElements))




	# Combine output renderers and dynoutput renderers
	outputRenderers = c(outputRenderers, dynOutputRenderers)

	##
	### generate ui.R
	##

	page = call("pageWithSidebar", headPanel, sbPanel, mnPanel)
	UI = call("shinyUI", page) # do.call(call, c(list("shinyUI"), vector('list', 1)))

	sink(uifile);
	cat("library(shiny)\n\n");
	print(UI);
	sink()


	##
	### generate server.R
	##

	# the reactive and output rendering calls should be assigned to variables indicated by the
	# names() of the entries in the comp.parsed and outputRenderers lists
	reactives = sapply(names(comp.parsed), function(name) substitute(expression(a <- b), list(a=as.name(name), b=comp.parsed[[name]]))[[2]])
	output = sapply(names(outputRenderers), function(name) substitute(expression(output$a <- b), list(a=as.name(name), b=outputRenderers[[name]]))[[2]])

	# the structure of server.R should be shinyServer(function(input, output){ ... }) where the
	# ... are to be replaced with the computations in `reactives' and `output'
	foo = function(input, output){}
	body(foo) = as.call(c(as.name("{"), c(reactives, output)))
	server = call("shinyServer", foo)

	sink(serverfile);
	cat("library(shiny)\n\n"); cat(script.header, sep = "\n"); cat("\n\n\n\n")
	print(server);
	sink()

	# return the names of the generated files.
	c(uifile, serverfile)

}



