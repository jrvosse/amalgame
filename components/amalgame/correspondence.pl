:- module(ag_component_correspondence,
	  [ html_correspondence_options//0,
	    html_correspondence_buttons//0
	  ]).

:- use_module(library(http/html_write)).

html_correspondence_options -->
	html([ 'include all correspondences with the same: ',
	       input([type(checkbox), id(allsources), autocomplete(off)]),
	       label(' source'),
	       input([type(checkbox), id(alltargets), autocomplete(off)]),
	       label(' target')
	     ]).

html_correspondence_buttons -->
	html([ button([type(button), class(cancel), value(cancel)], 'cancel'),
	       button([type(button), class(submit), value(submit)], 'submit'),
	       button([type(button), class(setall)], 'apply to all'),
	       button([type(button), class(prev), value(prev)], '<'),
	       button([type(button), class(next), value(next)], '>')
	     ]).
