YUI.add('controls', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;

	var NODE_CONTROLS = Y.all(".control"),
		NODE_INPUT_CONTROLS = Y.all("#select .control"),
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
		srcNode: {
			value: null
		},
		selected: {
			value: null
		},
		nodes: {
			value: []
		}
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
			   node.one(".control-submit").on("click", this._onControlSubmit, this, node);
			   var desc =  node.one("div.desc");
			   if (desc) {
			     desc.on("mouseout",  function(e) { this.set("visible", false)}, this.explain_overlay);
			     desc.on("mouseover", this._onMouseOver, this, node);
			   }
			}, this);

			// the match control has two additional buttons
			// to set the source and target
			Y.on("click", this._valueSet, NODE_INPUT_BTN, this, "input");
			Y.on("click", this._valueSet, NODE_SOURCE_BTN, this, "source");
			Y.on("click", this._valueSet, NODE_TARGET_BTN, this, "target");

			// toggle the controls when selected is changed
			this.after('selectedChange', this.syncUI, this);
			this.syncUI();
		},

		_onControlSubmit : function(e, node) {
			e.preventDefault();

			var content = this.get("srcNode"),
				input = NODE_INPUT.get("value"),
				source = NODE_SOURCE.get("value"),
				target = NODE_TARGET.get("value"),
				selected = this.get("selected"),
				data = this._getFormData(node);

			// The input is selected based on the type of the control
			// which is stored as a CSS class
			if(node.hasClass("match")) {
				if(input) {
					data.input = input;
				}
				else if(source&&target) {
					data.source = source;
					data.target = target;
				}
			}
			else if(selected) {
				data.input = selected.uri;
			}
			Y.log("add process:");
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
			var selected = this.get("selected"),
				type = selected ? selected.type : "";

			// add mapping selection radio buttons with available mappings to components that need them
			this._setMappingSelecter();

			// We only show the controls for the active type
			NODE_INPUT_CONTROLS.each(function(node) {
				if(type&&node.hasClass(type)) {
					node.removeClass("disabled");
				} else {
					node.addClass("disabled");
				}
			});

			// enable input select when a vocabulary is selected
			NODE_INPUT_BTN.setAttribute("disabled", true);
			NODE_SOURCE_BTN.setAttribute("disabled", true);
			NODE_TARGET_BTN.setAttribute("disabled", true);
			if(type=="vocab") {
				NODE_SOURCE_BTN.removeAttribute("disabled");
				NODE_TARGET_BTN.removeAttribute("disabled");
			} else if(type=="mapping") {
				NODE_INPUT_BTN.removeAttribute("disabled");
			}
			var secSelecter = Y.one(".secinput_selecter");
			if (secSelecter&&secSelecter.getContent()) {
				// Y.log("Enabling components requiring secondary inputs");
				Y.all(".secinput").removeClass("disabled");

			} else {
				// Y.log("Disabling components requiring secondary inputs");
				Y.all(".secinput").addClass("disabled");
			}
			var preloadedSelecter = Y.one(".preloaded select option")
			if(!preloadedSelecter) {
			  //Y.log("Disabling components requiring preloaded input mappings");
			  Y.all(".preloaded").addClass("disabled");
			}
			// enable matcher submit when both source and target have a value
			if(NODE_INPUT.get("value")||
				(NODE_SOURCE.get("value")&&NODE_TARGET.get("value"))) {
				Y.all("#match .control-submit").removeAttribute("disabled");
			} else {
				Y.all("#match .control-submit").setAttribute("disabled", true);
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
					var status = m.status?m.status:'unspecified';
					var checked=status.match('final')?'checked':''
					HTML += '<div><input type="checkbox" name="secondary_input" value="'
					+uri+'" ' +checked +' class="' + checked +'">'
					+'<span class="mapping_label">'+m.abbrev+':'+m.label+'</span></div>';
				}
			}
			return HTML;
		},

		_valueSet : function(e, which) {
			var selected =  this.get("selected");
			if(selected) {
				Y.one("#"+which+'Label').set("value", selected.label);
				Y.one("#"+which).set("value", selected.uri);
				this.syncUI();
			}
			if(which=="input") {
				Y.one("#sourceLabel").set("value", "");
				Y.one("#source").set("value", "");
				Y.one("#targetLabel").set("value", "");
				Y.one("#target").set("value", "");
			} else {
				Y.one("#inputLabel").set("value", "");
				Y.one("#input").set("value", "");
			}
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
