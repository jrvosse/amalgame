YUI.add('equalizer', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_TOP 			= Y.one("#top"),
		NODE_BOTTOM 		= Y.one("#bottom"),
		NODE_OPM 			= Y.one("#opm"),
		NODE_CONTROLS 		= Y.one("#controls"),
		NODE_INFOBOX 		= Y.one("#infobox"),
		NODE_MAPPINGTABLE 	= Y.one("#mappingtable"),
		NODE_CORRESPONDANCE = Y.one("#correspondance");
	
	function Equalizer(config) {
		Equalizer.superclass.constructor.apply(this, arguments);
	}
	Equalizer.NAME = "equalizer";
	Equalizer.ATTRS = {
		alignment : {
			value: null
		},
		selected : {
			value: null
		},
		paths:{
			value:{
				opmgraph:'/amalgame/opmviz',
				mapping:'/amalgame/data/mapping',
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
	
	Y.extend(Equalizer, Y.Base, {
		
		initializer: function(args) {
			// initalize the different modules
			this._initViewport();
			this._initGraph();
			this._initInfo();
			this._initControls();			
			this._initMappings();
			
			// bind the modules together
			this.opmviz.on("nodeSelect", function(e) {
				this.set("selected", e.uri);
			}, this);
			this.after("selectedChange", this._onSelectChange, this);
			this.controls.on("submit", this._onControlSubmit, this);
			
			// Let's get some stuff
			this._fetchGraph();
		},
		
		_initViewport : function() {
			// We make the top part of the window resizable
			// The bottom part react to this resize to match the viewport
			// Changes in the viewport size also are delegated to the bottom and top
			this._setBottomHeight();
			NODE_TOP.plug(Plugin.Resize, {draggable:true,handles:["b"],animate:true});
			NODE_TOP.dd.on( "drag:end", this._setBottomHeight, this);
			Y.on("resize", this._setSize, window, this);
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
		
		_initMappings : function() {
			var paths = this.get("paths");
			
			// We define a datasource to simplify 
			// access to the mappings later and add caching support
			var DS = new Y.DataSource.IO({
				source: paths.mapping
			})
			.plug(Plugin.DataSourceJSONSchema, {
				schema: {
					resultListLocator: "mapping",
		      		resultFields: ["source", "target", "relation"],
		      		metaFields: {
 						totalNumberOfResults:"total"
					}
		    	}
		  	})
			.plug({fn:Y.Plugin.DataSourceCache, cfg:{max:10}});

			this.mappingtable = new Y.MappingTable({
				srcNode: NODE_MAPPINGTABLE,
				datasource:DS
			});
		},
		
		_fetchGraph : function(conf) {
			var alignment = this.get("alignment"),
				paths = this.get("paths"),
				opmviz = this.opmviz;
		
			// reset the selected node
			this.set("selected", null);	
				
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
				
		_onSelectChange : function(e) {
			var uri = e.newVal,
				type = this.get("nodes")[uri]||"vocab";
				
			// update the controls and the info
			this.controls.set("input", uri);
			this.controls.set("type", type);
			this.infobox.set("selected", uri);
			
			// update other controls based on the type
			if(type=="mapping") {
				this.mappingtable.set("selected", uri);
			}
			else if(type=="process") {
			}
			else if(type=="vocab") {
			}
		},
		
		// The bottom part of the screen fills the height of the viewport
		_setBottomHeight : function() {
			var	bottomheight = NODE_TOP.get('winHeight') - NODE_TOP.get('clientHeight');  
			NODE_BOTTOM.setStyle("height", bottomheight+"px");
		},
		_setSize : function() {
			var width = NODE_TOP.get('winWidth'),
				height = NODE_TOP.get('winHeight'),
				topheight = Math.min(height-50, NODE_TOP.get('clientHeight')), 
				bottomheight = height - topheight;  
			NODE_TOP.setStyle("width", width+"px");
			NODE_TOP.setStyle("height", topheight+"px");
			NODE_BOTTOM.setStyle("width", width+"px");
			NODE_BOTTOM.setStyle("height", bottomheight+"px");
		}
	});
	
	Y.Equalizer = Equalizer;
	
}, '0.0.1', { requires: [
	'node','event','anim','tabview',
	'datasource-io','datasource-jsonschema','datasource-cache',
	'querystring-stringify-simple',
	'gallery-resize','gallery-node-accordion'
	]
});
