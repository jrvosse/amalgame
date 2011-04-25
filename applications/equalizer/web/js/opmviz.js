YUI.add('opmviz', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;
	
	function OPMViz(config) {
		OPMViz.superclass.constructor.apply(this, arguments);
	}
	OPMViz.NAME = "opmviz";
	OPMViz.ATTRS = {
		active: {
			value:null
		}
	};
	
	Y.extend(OPMViz, Y.Widget, {
		initializer: function(config) {
			this.publish("nodeSelect", {});
		},
		destructor : function() {},
		renderUI : function() {},
		bindUI : function() {
			var contentBox = this.get("contentBox");
			// Bind event handlers to the links in the graph
			// and prevent the default behavior from xlink:href

			if(contentBox.one("svg")) {
				Y.Event.purgeElement(contentBox, true);
				Y.delegate("click", function(e) {
					e.preventDefault();
					var target = e.currentTarget,
						uri = target.getAttribute("xlink:href");
					this.set("active", uri);
					Y.log("selected: "+uri);
					Y.all("svg a").removeAttribute("class", "selected");
					target.setAttribute("class", "selected");
					this.fire("nodeSelect", {target:target, uri:uri});
				}, "svg", "a", this);
			}
		},
		
		syncUI : function() {
			// put back the active selection
			var active = this.get("active");
			if(active) {
				this.get("contentBox").all("a").each(function(node) {
					if(node.getAttribute("xlink:href")==active) {
						node.setAttribute("class", "selected");
					}
				})
			}
		},
		
		setGraph : function(graph) {
			this.get("contentBox").setContent(graph);
			this.syncUI();
			this.bindUI();
		}
	});
	
	Y.OPMViz = OPMViz;
	
}, '0.0.1', { requires: ['node','event','widget']});
