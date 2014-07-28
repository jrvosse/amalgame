:- module(ag_component_correspondence,
	  [ html_correspondence_overlay//0
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
	html([ button([type(button), class(cancel), value(cancel), title(cancel)],   'x'),
	       button([type(button), class(prev),   value(prev),   title(previous)], '<'),
	       button([type(button), class(next),   value(next),   title(next)],     '>'),
	       button([type(button), class([change, submit]), value(submit)], 'submit'),
	       button([type(button), class([change, setall])], 'apply to all')
	     ]).

html_correspondence_overlay -->
	html(form([div(class('yui3-widget-bd'),
		       [
			   div(class(options), \html_correspondence_options),
			   div(class(buttons), \html_correspondence_buttons),
			   div([class(concepts), id(concepts)], [])
		       ]),
		   div(class('yui3-widget-ft'),
		       [ div(class(controls),
			     [ div(class(buttons), \html_correspondence_buttons)
			     ])
		       ])
		  ])).
