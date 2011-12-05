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
			value:null
		},
		nodes: {
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
			this.after("nodesChange", this._onNodesChange, this);
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
				alignment = this.get("alignment"),
				paths = this.get("paths");

			// check if nodes are updated
			Y.io(paths.opmgraph, {
				data:{"graph":alignment},
				on:{success: function(e,o) {
					// As the server returns an XML document, including doctype
					// we first take out the actual svg element
					var SVG = o.responseXML.lastChild;
					oSelf.get("contentBox").setContent(SVG);
					oSelf._bindSVG();
					oSelf._updateNodes();
					}
				}
			});
		},
		
		_onNodesChange : function(o) {
 			if(this._nodesCompare(o.newVal, o.prevVal)) {
				this._updateNodes();
			} else {	
				this.syncUI();
			}
		},
		
		_updateSelection : function() {
			var selected = this.get("selected").uri;
			this.get("contentBox").all("a").each(function(svgnode) {
				var id=svgnode.getAttribute("xlink:href");
				if(id == selected) {
 					svgnode.setAttribute("class", "selected");
				} else {
					svgnode.removeAttribute("class", "selected");
				}
			});
		},
		
		_updateNodes : function() {
	         // put back the active selection
			var selected = this.get("selected").uri,
				nodes = this.get("nodes");
			
  			this.get("contentBox").all("a").each(function(svgnode) {

				var id=svgnode.getAttribute("xlink:href");

				if(id == selected) {
					svgnode.setAttribute("class", "selected");
				}

				var node = nodes[id];
 
				if (node && node.type == 'mapping' && node.stats) {
 					this._insert_info(svgnode, this._layout_stats(node.stats));
				}
				if (node.type == 'mapping' && node.abbrev) {
				  svgnode.one('text').prepend(node.abbrev+':');
				}
				if (node && node.type == 'vocab' && node.count) {
 					this._insert_info(svgnode, node.count);
				}
   			}, this)
	    },

		_onNodeSelect : function(e) {
			e.preventDefault();
			var target = e.currentTarget,
				uri = target.getAttribute("xlink:href");

			Y.log("selected: "+uri);
			Y.all("svg a").removeAttribute("class", "selected");
			target.setAttribute("class", "selected");
			this.fire("nodeSelect", {target:target, uri:uri});
		},

		_insert_info : function(target, HTML) {
			var textNode = target.one("text");

			if(target.one('.info')) {
				target.one('.info').setContent(HTML);
			} else {
				var x = textNode.getAttribute("x"),
					y = parseInt(textNode.getAttribute("y")),
					infoNode = document.createElementNS(svgNS,"text");

				infoNode.appendChild(document.createTextNode(HTML));
				infoNode.setAttribute("class", "info");
				infoNode.setAttribute("text-anchor", "middle");
				infoNode.setAttribute("y",y+6);
				infoNode.setAttribute("x",x);
				infoNode.setAttribute("font-family","Times,serif");
				textNode.setAttribute("y",y-4);
				target.appendChild(infoNode);
			}	
		},

		_layout_stats : function(stats) {
			return stats.sperc+'% - '+stats.tperc+'%';
		},
		
		
		_nodesCompare : function(n1, n2) {
			n1a = Y.Object.keys(n1),
			n2a = Y.Object.keys(n2);
			if(n1a.length!==n2a.length) {
				return false;
			}
			for (var i=0; i < n1a.length; i++) {
				if(n1a[i] !== n2a[i]) {
					return false;
				}
			}
			return true;
		}

		
	});

	Y.OPMViz = OPMViz;

}, '0.0.1', { requires: ['node','event','widget']});
