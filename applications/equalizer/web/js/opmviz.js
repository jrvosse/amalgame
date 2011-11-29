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
		selected: {
			value:null
		},
		alignment: {
			value: null
		},
		paths: {
			value: null
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
			this._fetchGraph();
		},
		destructor : function() {},
		renderUI : function() {},
		bindUI : function() {
			this.on("nodesChange", this._fetchGraph, this);
			this._bindSVG();
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
	         // put back the active selection
			var selected = this.get("selected");
			var oSelf = this;
  			this.get("contentBox").all("a").each(function(svgnode) {
				var id=svgnode.getAttribute("xlink:href");

				if(id == selected) {
 					svgnode.setAttribute("class", "selected");
				}

				var node = oSelf.get('nodes')[id];
				if (node && node.type == 'mapping' && node.stats) {
 					oSelf._insert_info(svgnode, oSelf._layout_stats(node.stats));
				}
				if (node && node.type == 'vocab' && node.count) {
 					oSelf._insert_info(svgnode,node.count);
				}
   			})
	    },

		
		_fetchGraph : function(conf) {
			var alignment = this.get("alignment"),
				paths = this.get("paths"),
				oSelf = this;

			if(alignment) {
				conf = conf ? conf : {};
				conf.graph = alignment;

				Y.io(paths.opmgraph, {
					data:conf,
					on:{success: function(e,o) {
						// As the server returns an XML document, including doctype
						// we first take out the actual svg element
						var SVG = o.responseXML.lastChild;
						oSelf.setGraph(SVG);
						}
					}
				})
			}
		},

		setGraph : function(graph) {
			this.get("contentBox").setContent(graph);
			this.syncUI();
			this._bindSVG();
		},

		_onNodeSelect : function(e) {
			e.preventDefault();
			var target = e.currentTarget,
				uri = target.getAttribute("xlink:href");

			this.set("selected", uri);
			Y.log("selected: "+uri);
			Y.all("svg a").removeAttribute("class", "selected");
			target.setAttribute("class", "selected");
			this._fetchNodeStats(uri, target);
		},

		_insert_info : function(target, HTML) {
			var textNode = target.one("text"),
			  x = textNode.getAttribute("x"),
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
		},

		_layout_stats : function(stats) {
			return stats.sperc+'% - '+stats.tperc+'%';
		},

		_fetchNodeStats : function(uri, target) {
			var oSelf = this,
				alignment = this.get('alignment');
			var conf = {
				'url':uri,
				'alignment':alignment
			};
			this.infoDS.sendRequest({
				cfg: { scope: this },
				request:'?'+Y.QueryString.stringify(conf),
				callback:{success:function(o) {
						var HTML = o.response.results[0].responseText;
						if(HTML&&!target.one(".info")) {
						  oSelf._insert_info(target, HTML);
						}
						oSelf.fire("nodeSelect", {target:target, uri:uri});
					}
				}
			})
		}
	});

	Y.OPMViz = OPMViz;

}, '0.0.1', { requires: ['node','event','widget']});
