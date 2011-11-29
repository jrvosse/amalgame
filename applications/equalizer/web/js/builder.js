YUI.add('builder', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_OPM			= Y.one("#opm"),
		NODE_CONTROLS		= Y.one("#controls"),
		NODE_INFO			= Y.one("#info"),
		NODE_SELECT			= Y.one("#select");
		

	function Builder(config) {
		Builder.superclass.constructor.apply(this, arguments);
	}
	Builder.NAME = "builder";
	Builder.ATTRS = {
		alignment : {
			value: null
		},
		selected : {
			value: null
		},
		readonly : {
			value: true
		},
		paths:{
			value: {},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
		nodes:{
			value:{},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
	    strings: {
	        value: {},
			validator: function(val) {
				return Lang.isObject(val)
			}
	    }
	};

	Y.extend(Builder, Y.Base, {

		initializer: function(args) {
			this.readonly = (this.get('readonly') != "false"); // string/boolean

			// initalize the different modules
			this._initLayout();
			this._initGraph();
			this._initControls();
			this._initInfo();
			this._initMapping();

			if (!this.readonly) {
				this.controls.on("submit", this._onControlSubmit, this);
			} else {
			  Y.one('#hint').setContent('Please login to make changes');
			  Y.all('button').each(function(button) { button.setAttribute("disabled", true); });
			};

			this.opmviz.on("nodeSelect", this._onNodeSelect, this);
			this.infobox.after("deleteNode", this._onNodeDelete, this);
			this.infobox.after("nodeUpdate", this._onNodeUpdate, this);
			this.controls.on("nodeUpdate", this._onNodeUpdate, this); // used for hints (generated on the server)
			this.mapping.on("evalSubmit", this._updateNodes, this);

			this.after('nodesChange', function(o) {
				this.controls.set("nodes", o.newVal);
			}, this);

			this._fetchGraph();

			// Let's get started by selecting the strategy node
			// if we have no selection
			var selected = this.get("selected");
			if (selected) {
			  this._onNodeSelect({uri:selected});
			} else {
			  this._onNodeSelect({uri:this.get("alignment")});
			};
		},

		_initLayout : function() {
			// graph node is resizable in height
			var resize = new Y.Resize({
		        node: '#graph',
				handles: 'b',
				wrap: true
		    }); 
			resize.on("resize", this._onGraphResize, this);
			resize.on("end", this._onGraphResize, this);
			Y.on("windowresize", this._onWindowResize, this);
			
			// make controls and, graph-bottom fit in viewport
			this._onWindowResize();
		},

		_initGraph : function() {
			var DS = new Y.DataSource.IO({
				source: this.get("paths").nodeinfo
			});
			this.opmviz = new Y.OPMViz({
				datasource: DS,
				mappings: this.get("nodes"),
				alignment: this.get("alignment")
			}).render(NODE_OPM);
		},

		_initInfo : function() {
			// The infobox is part of the controls,
			// but has some additional routines
			var DS = new Y.DataSource.IO({
				source: this.get("paths").info
			});
			//.plug({fn:Y.Plugin.DataSourceCache, cfg:{max:10}});
			this.infobox = new Y.InfoBox({
				srcNode: NODE_INFO,
				alignment: this.get("alignment"),
				mappings: this.get("nodes"),
				hint: this.get("paths").hint,
				controls: this.controls,
				readonly: this.readonly,
				paths: this.get("paths"),
				datasource: DS
			});

		},

		_initControls : function() {
			this.controls = new Y.Controls({
				srcNode: NODE_CONTROLS,
				selected: this.get("selected"),
				nodes: this.get("nodes")
			});
		},
		
		_initMapping : function() {

			this.mapping = new Y.Mapping({
				paths: this.get("paths"),
				alignment: this.get("alignment"),
				mapping: this.get("selected")
			});
		},
		
		// helper functions for resizing
		_contentHeight : function() {
			var winHeight = Y.DOM.winHeight(),
				headerHeight = Y.one("#header").get("offsetHeight");
			return (winHeight - headerHeight - 12);	
		},
		_onGraphResize : function(e) {
			// the bottom node needs be update according to the remaining space
			var bottomHeight = this._contentHeight()-(e.info.offsetHeight+10);
			Y.one("#bottom").setStyle("height", bottomHeight);
			
			// resize sets fixed width, but we want a fluid width
			Y.one("#graph").get("parentNode").setStyle("width", "100%"); 
			Y.one("#graph").setStyle("width", "100%");
		},
		_onWindowResize : function() {
			var contentHeight = this._contentHeight(),
				graphHeight = contentHeight - (Y.one("#bottom").get("offsetHeight")+10);
			Y.one("#graph").get("parentNode").setStyle("height", graphHeight); 
			Y.one("#graph").setStyle("height", graphHeight);
			Y.one("#graph").get("parentNode").setStyle("width", "100%"); 
			Y.one("#graph").setStyle("width", "100%");
			Y.one("#controls").setStyle("height", contentHeight);
		},
		
		
		_fetchGraph : function(conf) {
			var alignment = this.get("alignment"),
				paths = this.get("paths"),
				opmviz = this.opmviz;

			if(alignment) {
				conf = conf ? conf : {};
				conf.graph = alignment;

				Y.io(paths.opmgraph, {
					data:conf,
					on:{success: function(e,o) {
						// As the server returns an XML document, including doctype
						// we first take out the actual svg element
						var SVG = o.responseXML.lastChild;
						opmviz.setGraph(SVG);
						}
					}
				})
			}
		},

		_updateNodes : function() {
			var oSelf = this,
				alignment = this.get("alignment"),
				paths = this.get("paths");

			Y.io(paths.graphnodes, {
				data:{"alignment":alignment},
				on:{success: function(e,o) {
					var r =	Y.JSON.parse(o.responseText);
					if(r.nodes) { // TBD check if nodes is changed
						oSelf.set("nodes", r.nodes);
						opmviz.set("nodes", r.nodes);
						oSelf._fetchGraph();
					}
				}}
			});
		},

		_onControlSubmit : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;

			// data only contains the process parameters
			// we need to add the context
			data.alignment = this.get("alignment");
			Y.io(paths.addprocess, {
				data:data,
				on:{success:function(e,o) {
					var r =	Y.JSON.parse(o.responseText);
					oSelf.set("nodes", r.nodes);
					oSelf.opmviz.fire("nodeSelect", {uri:r.focus});
					oSelf._fetchGraph();
				}}
			});
		},

		_onNodeUpdate : function(o) {
			Y.log("nodeUpdate event caught (builder:onNodeUpdate)");
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;
			data.alignment = this.get("alignment");
			Y.io(paths.updatenode, {
				data:data,
				on:{success:function(e,o) {
					var response = Y.JSON.parse(o.responseText);
					oSelf.set("nodes", response.nodes);
					if (data.alignment == response.alignment) {
						oSelf._fetchGraph();
						Y.log("fire nodeSelect after update");
						Y.log(response);
						oSelf.opmviz.fire("nodeSelect", {uri:response.focus});
					} else { // alignment changed name, we need to fully reload ...
						oSelf.set("alignment", response.alignment);
						var l = window.location;
						var newURL = l.protocol + "//" + l.host + l.pathname +
							"?alignment=" + encodeURIComponent(response.alignment);
						window.location.replace(newURL);
					}
				}}
			})
		},

		_onNodeDelete : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = {
					alignment:this.get("alignment"),
					uri:o.uri
				};

			Y.io(paths.deletenode, {
				data:data,
				on:{success:function(e,o) {
					oSelf.set("nodes", Y.JSON.parse(o.responseText).nodes);
					oSelf._fetchGraph();
				}}
			}),
			this._onNodeSelect({uri:this.get("alignment")});
		},

		_onNodeSelect : function(e) {
			var uri = e.uri;
			var node = this.get("nodes")[uri];
			node.uri = uri;
			this.set("selected", uri);
			// update the controls and the info
			this.controls.set("selected", node);
			this.infobox.set("selected", node);
			this.opmviz.set("active", uri);
			if(node.type=="mapping") {
				this.mapping.set("mapping", uri);
			} else {
				
			}	
		}
	});

	Y.Builder = Builder;

}, '0.0.1', { requires: [
	'node','event','anim','tabview','overlay',
	'io-base','datasource-io','datasource-cache',
	'querystring-stringify-simple',
	'gallery-node-accordion','mappingtable'
	]
});
