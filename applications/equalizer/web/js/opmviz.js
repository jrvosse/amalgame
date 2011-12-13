YUI.add('opmviz', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;

	var svgNS = "http://www.w3.org/2000/svg";

	function OPMViz(config) {
		OPMViz.superclass.constructor.apply(this, arguments);
	}
	OPMViz.NAME = "opmviz";
	OPMViz.ATTRS = {
		alignment: {
			value: null
		},
		paths: {
			value: null
		},
		selected: {
			value: null
		}
	};

	Y.extend(OPMViz, Y.Widget, {
		initializer: function(config) {
			this.infoDS = new Y.DataSource.IO({
				source: this.get("paths").nodeinfo
			});
			this.publish("nodeSelect", {});
		},
		destructor : function() {},
		renderUI : function() {},
		bindUI : function() {
			this.after("selectedChange", this._onSelectedChange, this);
		},
		
		_bindSVG : function () {
			var contentBox = this.get("contentBox");
			// Bind event handlers to the links in the graph
			// and prevent the default behavior from xlink:href
			if(contentBox.one("svg")) {
				Y.Event.purgeElement(contentBox, true);
				Y.delegate("click", this._onNodeSelect, "svg", "a", this);
			}
		},

		syncUI : function() {
			var oSelf = this,
				selected = this.get("selected").uri,
				alignment = this.get("alignment"),
				paths = this.get("paths");

			Y.io(paths.opmgraph, {
				data:{
					"selected":selected,
					"alignment":alignment
				},
				on:{
					success: function(e,o) {
						// As the server returns an XML document, including doctype
						// we first take out the actual svg element
						var SVG = o.responseXML.lastChild;
						oSelf.get("contentBox").setContent(SVG);
						oSelf._setSelection();
						oSelf._bindSVG();
					}
				}	
			});
		},
						
		_setSelection : function() {
			var selected = this.get("selected").uri;
  			this.get("contentBox").all("a").each(function(svgnode) {
				if(svgnode.getAttribute("xlink:href") === selected) {
					svgnode.setAttribute("class", "selected");
				}
			});
	    },
	
		_onSelectedChange : function(o) {
			this.syncUI();
		},

		_onNodeSelect : function(e) {
			e.preventDefault();
			var target = e.currentTarget,
				uri = target.getAttribute("xlink:href");

			Y.log("select node: "+uri);
			Y.all("svg a").removeAttribute("class", "selected");
			target.setAttribute("class", "selected");
			this.fire("nodeSelect", {target:target, uri:uri});
		}
		
	});

	Y.OPMViz = OPMViz;

}, '0.0.1', { requires: ['node','event','widget']});
