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
			this.after("selectedChange", this.syncUI, this);
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
			this.updateGraph(this.get("selected").uri);
		},
		
		updateGraph : function(uri) {
			var oSelf = this,
				paths = this.get("paths"),
				data = {"alignment":this.get("alignment")};
				
			if(uri) {
				data.selected = uri
			}	

			Y.io(paths.opmgraph, {
				data:data,
				on:{
					success: function(e,o) {
						// As the server returns an XML document, including doctype
						// we first take out the actual svg element
						oSelf.get("contentBox").setContent(o.responseXML.lastChild);
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
