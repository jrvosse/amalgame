YUI.add('controls', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;
	
	var NODE_CONTROLS = Y.all(".control"),
		NODE_INPUT_CONTROLS = Y.all("#select .control"),
		NODE_SOURCE = Y.one("#source"),
		NODE_TARGET = Y.one("#target"),
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
		}
	};
	
	Y.extend(Controls, Y.Base, {
		initializer: function(config) {
			var instance = this,
				content = this.get("srcNode");
			
			// The controls are accordion nodes
			Y.all(".yui3-accordion").plug(Y.Plugin.NodeAccordion, { 
   				anim: true, 
				speed:0.1,
				multiple:false
			});

			NODE_CONTROLS.each( function(node) {
				node.one(".control-submit").on("click", this._onControlSubmit, this, node);
			}, this);
			
			this._toggleControls();
			
			// the alignment control has two additional buttons
			// to set the source and target
			Y.on("click", this._valueSet, NODE_SOURCE_BTN, this, "source");
	      	Y.on("click", this._valueSet, NODE_TARGET_BTN, this, "target");
			
			// toggle the controls when selected is changed
			this.after('selectedChange', this._toggleControls, this);
		},
				
		_onControlSubmit : function(e, node) {
			var content = this.get("srcNode"),
				source = NODE_SOURCE.get("value"),
				target = NODE_TARGET.get("value"),
				selected = this.get("selected"),
				data = this._getFormData(node);
			
			// The input is selected base on the type of the control
			// which is stored as a CSS class
			if(node.hasClass("sourcetarget")) {
				if(source&&target) {
					data.source = source;
					data.target = target;
				} else {
					return "no source and target known";
				}
			}
			else if(selected) {
				data.input = selected.uri;
			} else {
				return "no input";
			}
			
			this.fire("submit", {data:data});

		},
		
		_getFormData : function(form) {
			var data = {};
			// get the values of all HTML input fields
			form.all("input").each(function(input) {
				var name = input.get("name"),
					value = input.get("value");
				if(name&&value&&input.get("type")!=="button") {
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
		
		_toggleControls : function() {
			var selected = this.get("selected"),
				type = selected ? selected.type : "";
			// We only show the controls for the active type
			NODE_INPUT_CONTROLS.each(function(node) {
				if(type&&node.hasClass(type)) {
					node.removeClass("disabled");
				} else {
					node.addClass("disabled");
				}
			});
			
			// enable input select when a vocabulary is selected
			if(type=="vocab") {
				NODE_SOURCE_BTN.removeAttribute("disabled");
				NODE_TARGET_BTN.removeAttribute("disabled");
			} else {
				NODE_SOURCE_BTN.setAttribute("disabled", true);
				NODE_TARGET_BTN.setAttribute("disabled", true);
			}
			
			// enable matcher submit when both source and target have a value
			if(NODE_SOURCE.get("value")&&NODE_TARGET.get("value")) {
				Y.all("#align .control-submit").removeAttribute("disabled");
			} else {
				Y.all("#align .control-submit").setAttribute("disabled", true);
			}
		},
		
		_valueSet : function(e, which) {
			var selected =  this.get("selected");
			if(selected) {
				Y.one("#"+which+'Label').set("value", selected.label);
				Y.one("#"+which).set("value", selected.uri);
				this._toggleControls();
     		}
		}
		
	});
	
	Y.Controls = Controls;
	
}, '0.0.1', { requires: ['node,event','gallery-node-accordion']});