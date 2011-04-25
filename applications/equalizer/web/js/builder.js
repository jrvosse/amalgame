YUI.add('builder', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_OPM 			= Y.one("#opm"),
		NODE_CONTROLS 		= Y.one("#controls"),
		NODE_INFOBOX 		= Y.one("#infobox");
	
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
		paths:{
			value:{
				opmgraph:'/amalgame/opmviz',
				addprocess:'/amalgame/addprocess',
				statistics:'/amalgame/statisctis'
			},
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
			// initalize the different modules
			this._initGraph();
			this._initInfo();
			this._initControls();			
			
			// bind the modules together
			this.opmviz.on("nodeSelect", this._onNodeSelect, this);
			this.controls.on("submit", this._onControlSubmit, this);
			this.infobox.after("labelChange", this._onLabelChange, this);
			
			// Let's get some stuff
			this._fetchGraph();
		},
		
		_initGraph : function() {
			this.opmviz = new Y.OPMViz().render(NODE_OPM);
		},
		
		_initInfo : function() {
			var paths = this.get("paths");
			
			// The infobox is part of the controls,
			// but has some additional routines
			var DS = new Y.DataSource.IO({
				source: paths.statistics
			})
			.plug({fn:Y.Plugin.DataSourceCache, cfg:{max:10}});
			this.infobox = new Y.InfoBox({
				srcNode: NODE_INFOBOX,
				content: "select a node",
				datasource: DS
			});
		},
		
		_initControls : function() {			
			this.controls = new Y.Controls({
				srcNode: NODE_CONTROLS
			});
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
					oSelf.set("nodes", Y.JSON.parse(o.responseText).nodes);
					oSelf._fetchGraph();
				}}
			})
		},
		
		_onLabelChange : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = {
					alignment:this.get("alignment"),
					uri:o.uri,
					label:o.newVal
				};
			
			Y.io(paths.updatelabel, {
				data:data,
				on:{success:function(e,o) {
					oSelf.set("nodes", Y.JSON.parse(o.responseText).nodes);
					oSelf._fetchGraph();
				}}
			})
		},
				
		_onNodeSelect : function(e) {
			var uri = e.uri,
				node = this.get("nodes")[uri],
				type = node.type,
				label = node.label;
			node.uri = uri;
				
			this.set("selected", uri);	
			// update the controls and the info
			this.controls.set("selected", node);
			this.infobox.set("selected", node);
		}
	});
	
	Y.Builder = Builder;
	
}, '0.0.1', { requires: [
	'node','event','anim','tabview',
	'datasource-io','datasource-cache',
	'querystring-stringify-simple',
	'gallery-node-accordion'
	]
});
