YUI.add('controls', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;

	var NODE_CONTROLS = Y.all(".control"),
		NODE_INPUT_CONTROLS = Y.all("#select .control"),
		NODE_CONTROLS_ACTIVE = Y.all(".control.always_active"),
		NODE_CONCEPT_INPUTS = Y.all('.vocab input.concept'),
		NODE_INPUT = Y.one("#input"),
		NODE_SOURCE = Y.one("#source"),
		NODE_TARGET = Y.one("#target"),
		NODE_INPUT_BTN = Y.one("#inputbtn"),
		NODE_SOURCE_BTN = Y.one("#sourcebtn"),
		NODE_TARGET_BTN = Y.one("#targetbtn");

	function Controls(config) {
		Controls.superclass.constructor.apply(this, arguments);
	}
	Controls.NAME = "controls";
	Controls.ATTRS = {
		srcNode: { value: null },
		focus: { value: null }, // focus node
		currentConcept: { value: null }, // selected concept from SKOS browser
		nodes: { value: [] }
	};

	Y.extend(Controls, Y.Base, {
		initializer: function(config) {
			var instance = this;
			var content = this.get("srcNode");

			this.explain_overlay = new Y.Overlay( { visible:false, zIndex:1, x:355, y:35 } ).render();

			// the display of the control sets can be toggled
			Y.all(".control-set .hd").on("click", function(e) {
				e.currentTarget.get("parentNode").toggleClass("active");
			});

			// The list of amalgame modules make an accordion
			Y.all(".module-list").plug(Y.Plugin.NodeAccordion, {
				multiple:false
			});

			// The control all have submit button that we bind here
			NODE_CONTROLS.each(function(node) {
			   // Y.log(node.one(".control-submit"));
			   node.one(".control-submit").on("click", this._onControlSubmit, this, node);
			   var desc =  node.one("div.desc");
			   if (desc) {
			     desc.on("mouseout",  function(e) { this.set("visible", false)}, this.explain_overlay);
			     desc.on("mouseover", this._onMouseOver, this, node);
			   }
			}, this);

			// the generate control has two additional buttons
			// to set the source and target
			Y.on("click", this._valueSetAndSyncUI, NODE_SOURCE_BTN, this, "source");
			Y.on("click", this._valueSetAndSyncUI, NODE_TARGET_BTN, this, "target");
			Y.on("click", this._valueSetAndSyncUI, NODE_INPUT_BTN, this, "input");

			// try to use the currently selected concept in the vocab browser:
			NODE_CONCEPT_INPUTS.on('focus', this.currentConceptChange, this);
			this.after('currentConceptChange', this.currentConceptChange, this);

			// toggle the controls when focus is changed
			this.after('focusChange', this.syncUI, this);
			this.after('nodesChange', this.syncUI, this);
			this.after('nodesChange', function(e) { Y.log('nodesChange in controls'); Y.log(e);});

			this.syncUI();
		},

		currentConceptChange : function(ev) {
			var targets = null;
			var c = this.get('currentConcept');
			if (ev.type == "controls:currentConceptChange")
				targets = NODE_CONCEPT_INPUTS;
			else
				targets = new Y.NodeList(ev.currentTarget);
			if (c)
				targets.each(function(n) { n.set("value", c.id); });
			else
				targets.each(function(n) { n.set("value",
					"Enter URI of the parent concept  manually or select the concept in the vocabulary browser");
				});
		},

		_onControlSubmit : function(e, node) {
			Y.log("add process:");
			e.preventDefault();

			var content = this.get("srcNode"),
				input = NODE_INPUT.get("value"),
				source = NODE_SOURCE.get("value"),
				target = NODE_TARGET.get("value"),
				focus = this.get("focus"),
				data = this._getFormData(node);

			// The input is selected based on the type of the control
			// which is stored as a CSS class
			if( node.hasClass("source_target") ||
			    node.hasClass("preloaded") ) {
				data.source = source;
				data.target = target;
			} else if(focus) {
				data.input = focus.uri;
			}
			Y.log(data);
			this.fire("submit", {data:data});
		},

		_onMouseOver : function (e, node) {
			   var explainNode = node.one("input[name=graphic]");
			   if (explainNode) {
			     var exp = this.explain_overlay;
			     var selectNode = node.one("select[name=type]");
			     var selectedIndex = selectNode.get("selectedIndex");
			     var selectType = selectNode.get("options").item(selectedIndex).getAttribute("value");
			     var explainURI = explainNode.getAttribute('value') + '-' + selectType + ".png";
			     //Y.log(explainURI);
			     exp.set("bodyContent", "<img class='explain overlay' src='"+explainURI+"'/>");
			     exp.set("visible", true);
			   };
		 },

		_getFormData : function(form) {
			var data = {};
			// get the values of all HTML input fields
			form.all("input").each(function(input) {
				var name = input.get("name"),
					value = input.get("value");
				if(input.get("type")=="checkbox") {
					if(input.get("checked")) {
						if(data[name]) {
							data[name].push(value);
						} else {
							data[name] = [value];
						}
					}
				}
				else if(input.get("type")!=="button"&&name&&value) {
					data[name] = value;
				}
			});
			// get the values of the selected options
			form.all("select").each(function(select) {
				var name = select.get("name"),
					index = select.get('selectedIndex'),
					value = select.get("options").item(index).get("value")
				if(value) {
					data[name] = value;
				}
			});

			return data;
		},

		syncUI : function() {
			var focus = this.get("focus");
			type = focus ? focus.type : "";
			Y.log('syncUI controls');
			Y.log(focus);
			Y.log(type);

			// add mapping selection radio buttons with available mappings to components that need them
			this._setMappingSelecter();
			var secSelecter = Y.one(".secinput_selecter");

			// Disable controls incompatible with the active type
			NODE_CONTROLS.each(function(node) {
				if( !type || !node.hasClass(type) ) 
					node.addClass("disabled"); 
				else
					node.removeClass("disabled"); 
			});

			NODE_CONTROLS_ACTIVE.removeClass("disabled");

			// Disable controls requiring secondairy mappings if there are none:
			if (!secSelecter || !secSelecter.getContent()) {
				Y.all(".secinput").addClass("disabled");
			}

			// enable input select when a vocabulary is selected
			NODE_INPUT_BTN.setAttribute("disabled", true);
			NODE_SOURCE_BTN.setAttribute("disabled", true);
			NODE_TARGET_BTN.setAttribute("disabled", true);
			if(type=="vocab") {
				NODE_SOURCE_BTN.removeAttribute("disabled");
				NODE_TARGET_BTN.removeAttribute("disabled");
				var current =  Y.one("#sourceLabel").get("value");
				if (!current) {
				  // auto set source to focus if no source yet:
				  this._valueSet(focus, "source");
				}
				var target = this._findOnlyOtherVocab(focus);
				if (target) {
				  // auto set target if only one other vocab exists:
				  this._valueSet(target, "target");
				}
			}
			if(type=="mapping" || type=="vocab" ) { 
				 NODE_INPUT_BTN.removeAttribute("disabled");
				 this._valueSet(focus, "input");
			}

			var preloadedSelecter = Y.one(".preloaded select option")
			if(!preloadedSelecter) {
			  Y.all(".preloaded").addClass("disabled");
			}
			// enable generator submit when both source and target have a value
			if (NODE_SOURCE.get("value") && NODE_TARGET.get("value")) {
				Y.all(".vocab .control-submit").removeAttribute("disabled");
			} else {
				Y.all(".vocab .control-submit").setAttribute("disabled", true);
			}
			if (NODE_INPUT.get("value")) {
				Y.all(".mapping .control-submit").removeAttribute("disabled");
				Y.all(".vocab.input .control-submit").removeAttribute("disabled");
			} else {
				Y.all(".mapping .control-submit").setAttribute("disabled", true);
				Y.all(".vocab.input .control-submit").setAttribute("disabled", true);
			}
		},

		_setMappingSelecter : function() {
			var nodes = this.get("nodes");
			Y.all(".secinput form").each( function(form) {
				var selecter = form.one('.secinput_selecter');
				if(!selecter) {
					selecter = Node.create('<div class="secinput_selecter"></div>');
					form.prepend(selecter);
				}
				selecter.setContent(this.formatMappingList(nodes));
			}, this);
		},

		formatMappingList : function(nodes) {
			var HTML = "";
			for (var uri in nodes) {
				var m = nodes[uri];
				if(m.type == "mapping") {
					if (m.label == m.local) { 
						label = m.label; 
					} else { 
						label = m.label + ' (' + m.local +')'; 
					}	
					var status = m.status?m.status:'unspecified';
					var checked=status.match('final')||status.match('reference')?'checked':''
					HTML += '<div><input type="checkbox" name="secondary_input" value="'
					+uri+'" ' +checked +' class="' + checked +'">'
					+'<span class="mapping_label">'+m.abbrev+':'+label+'</span></div>';
				}
			}
			return HTML;
		},

		_findOnlyOtherVocab: function(current) {
		  var nodes = this.get("nodes");
		  var others = [];
		  for (var uri in nodes) {
		    var n = nodes[uri];
		    if ((n.uri != current.uri) &&
			(n.type == "vocab")
		       ) others.push(n);
		  }
		  if (others.length == 1) return others[0]; else return null;
		},


		_valueSetAndSyncUI: function(e, which) {
		       var focus =  this.get("focus");
		       this._valueSet(focus, which);
		       this.syncUI();
		},

		_valueSet : function(focus, which) {
			if(focus) {
				if (which != 'target') this._setLanguageOptions(focus, which);
				Y.one("#"+which+'Label').set("value", focus.label);
				Y.one("#"+which).set("value", focus.uri);
			}
		},

		_setLanguageOptions : function(node, which) {
			Y.log('_setLanguageOptions');
			Y.log(node);	
			if (!node || !node.stats || !node.type) return;
			var langs = null, control_set = null;
			if ( node.type == "vocab" && which == "source" ) {
				langs = node.stats.languages;
				control_set = '#generate_control_set';
			} else if ( node.type == "mapping" && which == "input" ) {
				langs = node.stats.vocs.source.stats.languages;
				control_set = '#select_control_set';
			} else return;
			Y.log(langs);
			if (!langs) return;
			Y.all(control_set + ' select.source_language').each(function(n) {
				Y.log(n);
				n.get('childNodes').remove();
				for(var i in langs) {
					var l = langs[i];
					n.append('<option value="' + l +'">'+l+'</option>');
				}
			});
		}

	});

	Y.Controls = Controls;

}, '0.0.1', {
	    requires: ['node',
		       'event',
		       'overlay',
		       'anim',
		       'gallery-node-accordion'
		      ]
	    });
