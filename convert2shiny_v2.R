# DESPITE THE FILE NAME, THIS IS ACTUALLY AN OLDER VERSION

convert2shiny <- function(
	scriptfile = '~/Dropbox/shiny parser/example header based on shiny examples.R',
	uifile = file.path(dirname(scriptfile), "ui.R"),
	serverfile = file.path(dirname(scriptfile), "server.R")) 
{
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
				ss = strsplit(sub(",","|",line[2]),"\\|")[[1]]
				info = c(info, list(title=ss[1], valid=parse(text=ss[2])))
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
		w = if (!is.null(input$valid)) eval(input$valid)
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
			else if(!is.numeric(v) && length(w) > 1 && is.null(names(w))) {
				do.call(call, list(name = "selectInput", as.character(input$name), input$title, choices=w))
			}
			else if(length(w) > 1 && !is.null(names(w))) {
				do.call(call, list(name = "radioButtons", as.character(input$name), input$title, choices=w))
			}
		inputElem
	})
	
	sbPanel = as.call(c(as.name("sidebarPanel"), inputElements))
	
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
			if(any(all.vars(expr) %in% globvarnms)) {
				nc = length(expr) # number of command statements in the code (between {})
				expr[[nc]] = as.call(c(as.name("("), expr[[nc]])) # enclose last statement between () to make sure it returns a value
				## Is there a way to detect all variables that exist in the functions environment after execution?
        ## This doesn't work, because variables in the code may be dummy's in local functions!
        ##   local.vars = setdiff(x = all.vars(expr), globvarnms)
				##   returnValue = structure(lapply(local.vars, as.name),.Names=local.vars) # = named list of all local variables created in this segment of code
				##   expr[[nc+1]] = call("return", returnValue)
				expr = do.call(substitute, list(expr, subs)) # prepend all input variable names with `input$'
				call("reactive", expr)
			}
			else {
				call("reactive", expr)
			}
		})
	names(comp.parsed) = make.names(sapply(comp, function(x) x[1]), unique=TRUE) # names will be used as names for generated reactive functions
	
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
	
  
	# Each segment of code presumably computes new variables that are used
	# in the MAINPAGE section of the code. To know which `reactive' code segment 
	# needs to be called in MAINPAGE, here the names of those newly created variables
	# are computed (this is probably not always doing the right thing; maybe this
	# can be improved with the findLocals in the codetools package):
	
		# save all existing objects in a temporary file (it is very ugly indeed!! maybe try to find a proper solution...)
	.list = ""; .list = ls(all = TRUE); tmpfile = tempfile(); save(list=.list, file=tmpfile); rm(list = .list[!(.list %in% c("comp.parsed", "globvarnms"))])
		# determine the newly defined variables
	new.vars = lapply(comp.parsed, function(x) {
			nms = all.vars(x);
			nms = nms[!sapply(nms,function(y) exists(y,envir=.GlobalEnv) && !is.function(get(y)) || (y %in% rownames(installed.packages())))]
			setdiff(nms, c(globvarnms, "input"))
		})
		# reload the saved objects and get rid of the temporary file
	load(tmpfile); unlink(tmpfile); rm(tmpfile)
	
	# Second, parse the MAINPAGE and turn them into output rendering calls. The
	# rendering type is determined by the first (key) word after the hash (namely
	# `print', `show', `plot', or `tabulate'). The rendering defaults to renderText.
	outputs = R.parts$MAINPAGE
	outp = splitcode(outputs, "^#\\s+\\w")
	outp.parsed = sapply(outp, function(code) {
			comment = gsub("^#\\s+|\\s*$", "", code[1])
		    rend = gsub("^(print|show|plot|tabulate).+","\\1", comment);
			renderer = switch(rend, print="renderPrint", plot="renderPlot", tabulate="renderTable", show="renderText", "renderPrint"); 
			outputer = switch(rend, print="textOutput", plot="plotOutput", tabulate="tableOutput", "verbatimTextOutput")
			expr = parse(text=c("{", code, "}"))[[1]]
			expr = do.call(substitute, list(expr, subs)) # prepend all input variable names with `input$'
			envexpr = as.call(c(as.name("c"), reactCompsCalls))
			cl = call(renderer, call("{", call("with", envexpr, expr)))
			attr(cl, "caption") = comment
			cl
		})
	names(outp.parsed) = make.names(sapply(outp.parsed, function(x) attr(x,'caption')), unique=TRUE) # names will be used as slotnames in output variable by which the UI's mainPanel calls their rendering
	
	
	# The variables that are used in MAINPAGE and are created in MAINCOMPUTATIONS, as listed by
	# new.vars, have to be replaced by a call to the appropriate `reactive' code segment. To do
	# so we use the function substitute using a do.call statement that needs a list of the form
	# list(name = substitute). We first create these lists
	subst.lists <- lapply(outp.parsed, function(x) {
			nms = all.vars(x); y = new.vars[new.vars %in% nms]; 
			z=names(y); 
			z=paste(z,ifelse(z!="","()",""));
			structure(as.list(parse(text=z)[1]),names=unlist(y))
		})
	# Next we apply the substitutions
	outputRenderers = lapply(1:length(outp.parsed), function(i) do.call(substitute, list(outp.parsed[[i]], subst.lists[[i]])))
	names(outputRenderers) = names(outp.parsed)
	outputElements = unlist(lapply(names(outp.parsed), function(name) {
			caption = attr(outp.parsed[[name]], "caption")
		    rend = gsub("^(print|show|plot|tabulate).+","\\1", caption);
			outputer = switch(rend, print="textOutput", plot="plotOutput", tabulate="tableOutput", "verbatimTextOutput")
			c(call("h4", caption), call(outputer, name))
		}), recursive=FALSE)
	
	mnPanel = as.call(c(as.name("mainPanel"), outputElements))
	
	
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


	
