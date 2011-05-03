YUI.add('controls', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;
	
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
				speed:0.1
			});

			content.all("form").each( function(form) {
				form.one("input.control-submit").on("click", this._onControlSubmit, this, form);
			}, this);
			
			this._toggleControls();
			
			// the alignment control has two additional buttons
			// to set the source and target
			Y.on("click", this._valueSet, "#sourcebtn", this, "source");
	      	Y.on("click", this._valueSet, "#targetbtn", this, "target");
			
			// toggle the controls when selected is changed
			this.after('selectedChange', this._toggleControls, this);
		},
				
		_onControlSubmit : function(e, form) {
			var oSelf = this,
				content = this.get("srcNode"),
				source = content.one("#source").get("value"),
				target = content.one("#target").get("value"),
				selected = this.get("selected"),
				data = this._getFormData(form);
				
			data.process = form.get("id");

			if(source&&target) {
				data.source = source;
				data.target = target;
				content.one("#source").set("value", "");
				content.one("#target").set("value", "");
			} else if(selected) {
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
			var srcNode = this.get("srcNode"),
				selected = this.get("selected"),
				type = selected ? selected.type : "";
				
			// We only show the controls for the active type
			srcNode.all(".control").each(function(node) {
				if(type&&node.hasClass(type)) {
					node.removeClass("disabled");
				} else {
					node.addClass("disabled");
				}
			});
		},
		
		_valueSet : function(e, which) {
			var selected =  this.get("selected");
			if(selected) {
				Y.one("#"+which).set("value", selected.uri);
     		}
		}
		
	});
	
	Y.Controls = Controls;
	
}, '0.0.1', { requires: ['node,event','gallery-node-accordion']});