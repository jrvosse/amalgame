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
		active: {
			value:null
		},
		alignment: {
			value: null
		},
		datasource: {
			value: null
		}
	};
	
	Y.extend(OPMViz, Y.Widget, {
		initializer: function(config) {
			this.set("active", this.get("alignment"));
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
				Y.delegate("click", this._onNodeSelect, "svg", "a", this);
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
		},
		
		_onNodeSelect : function(e) {
			e.preventDefault();
			var target = e.currentTarget,
				uri = target.getAttribute("xlink:href");
				
			this.set("active", uri);
			Y.log("selected: "+uri);
			Y.all("svg a").removeAttribute("class", "selected");
			target.setAttribute("class", "selected");
			this._fetchNodeStats(uri, target);
			this.fire("nodeSelect", {target:target, uri:uri});
		},
		
		
		_fetchNodeStats : function(uri, target) {
			var datasource = this.get("datasource");
			var alignment = this.get('alignment');
			var conf = { 
				'url':uri,
				'alignment':alignment
			};
			datasource.sendRequest({
				request:'?'+Y.QueryString.stringify(conf),
				callback:{success:function(o) {
						var HTML = o.response.results[0].responseText;
						if(HTML&&!target.one(".info")) {
							var textNode = target.one("text"),
								x = textNode.getAttribute("x"),
								y = parseInt(textNode.getAttribute("y")),
								infoNode = document.createElementNS(svgNS,"text");
							
							infoNode.appendChild(document.createTextNode(HTML));	
							infoNode.setAttribute("class", "info");
							infoNode.setAttribute("text-anchor", "middle");
							infoNode.setAttribute("y",y+8);
							infoNode.setAttribute("x",x);
							infoNode.setAttribute("font-family","Times,serif");
							infoNode.setAttribute("font-size", 10);
							textNode.setAttribute("y",y-4)
							target.appendChild(infoNode);
						}
					}
				}
			})
		}
	});
	
	Y.OPMViz = OPMViz;
	
}, '0.0.1', { requires: ['node','event','widget']});
