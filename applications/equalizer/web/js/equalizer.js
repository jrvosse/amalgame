YUI.add('equalizer', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_TOP 			= Y.one("#top"),
		NODE_BOTTOM 		= Y.one("#bottom"),
		NODE_OPM 			= Y.one("#opm"),
		NODE_CONTROLS 		= Y.one("#controls"),
		NODE_MCONTROLS 		= Y.one("#mappingcontrols"),
		NODE_PCONTROLS 		= Y.one("#processcontrols"),
		NODE_VCONTROLS 		= Y.one("#vocabcontrols"),
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
		mapping : {
			value: null
		},
		selected : {
			value: null
		},
		paths:{
			value:{
				opmgraph:'/amalgame/opmviz',
				mapping:'/amalgame/data/mapping'
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
			var paths = this.get("paths");
							
			// We make the top part of the window resizable
			// The bottom part react to this resize to match the viewport
			// Changes in the viewport size also are delegated to the bottom and top
			this.setBottomHeight();
			NODE_TOP.plug(Plugin.Resize, {draggable:true,handles:["b"],animate:true});
			NODE_TOP.dd.on( "drag:end", this.setBottomHeight, this);
			Y.on("resize", this.setSize, window, this);
			
			// We define a datasource to simplify 
			// access to the mappings later
			// and add caching support
			this.mappingDS = new Y.DataSource.IO({
				source: paths.mapping
			})
			.plug(Plugin.DataSourceJSONSchema, {
				schema: {
					resultListLocator: "mapping",
		      		resultFields: ["source", "target", "relation"],
		      		metaFields: {
						type:"type",
						statistics:"statistics",
						totalNumberOfResults:"statistics.mappingcount"
					}
		    	}
		  	})
			.plug({fn:Y.Plugin.DataSourceCache, cfg:{max:10}});
			
			// We have a separate widget for the OPM graph visualization
			// by the nodeSelect event we bind it to the other modules
			this.opmviz = new Y.OPMViz().render(NODE_OPM);
			this.opmviz.on("nodeSelect", this._onNodeSelect, this);
			
			// The controls are accordion nodes
			Y.all(".yui3-accordion").plug(Y.Plugin.NodeAccordion, { 
   				anim: true, 
				speed:0.1
			});

			// The options in the controls are tabviews
			var controls = {};
			Y.all(".yui3-tabview").each( function(node) {
				controls[node] = new Y.TabView({srcNode: node}).render();
			});
			Y.all("#controls form").each( function(form) {
				form.one("input.control-submit").on("click", this._onControlSubmit, this, form);
			}, this);
			

			// The infobox is part of the controls,
			// but has some additional routines
			this.infobox = new Y.InfoBox({
				srcNode: NODE_INFOBOX,
				content: "select a node"
			});
						
			// We have a separate widget for everything related to a single mapping
			this.mappingtable = new Y.MappingTable({
				srcNode: NODE_MAPPINGTABLE,
				datasource: this.mappingDS
			});
			
			// Let's get some stuff
			this.fetchGraph();
			
			
		},
		
		fetchGraph : function(conf) {
			var alignment = this.get("alignment"),
				paths = this.get("paths"),
				opmviz = this.opmviz;
				
			if(alignment) {
				conf = conf ? conf : {};
				conf.graph = alignment;
				
 				Y.io(paths.opmgraph, {
					data:conf,
					on:{success:function(e,o) {
						// As the server returns an XML document, including doctype
						// we first take out the actual svg element
						var SVG = o.responseXML.lastChild;
 						opmviz.setGraph(SVG);
					}}
				})
			}
		},
		
		fetchMapping : function() {
			var infobox = this.infobox,
				mappingtable = this.mappingtable,
				selected = this.get("selected");
								
			var callback = 	{
				success: function(o) {
					var meta = o.response.meta,
						statistics = meta.statistics;
					
					// update the other components
					infobox.set("data", statistics);
					mappingtable.handleResponse(o);
				}
			};
				
			if(selected) {
				infobox.set("waiting", true);
				this.mappingDS.sendRequest({
					request:'?'+Y.QueryString.stringify({url:selected}),
					callback:callback
				})
			}	
		},
		
		_onControlSubmit : function(e, form) {
			var paths = this.get("paths"),
				opmviz = this.opmviz,
				data = {
					input:this.get("selected"),
					process:form.get("id"),
					alignment:this.get("alignment")
				};
				
			form.all("input").each(function(input) {
				var name = input.get("name"),
					value = input.get("value");
				if(name&&value&&input.get("type")!=="button") {
					data[name] = value;
				}
			});
			form.all("select").each(function(select) {
				var name = select.get("name"),
					index = select.get('selectedIndex'),
					value = select.get("options").item(index).get("value")
				if(value) {
					data[name] = value;
				}
			});
			
			Y.io(paths.addprocess, {
				data:data,
				on:{success:function(e,o) {
					var SVG = o.responseXML.lastChild;
						opmviz.setGraph(SVG);
				}}
			})
		},
				
		_onNodeSelect : function(e) {
			var uri = e.uri,
				type = this.get("nodes")[uri]||"vocab";
			this.set("selected", uri);
			console.log(type);
			// depending on the result type we update
			// different components
			if(type=="mapping") {
				activeSet = NODE_MCONTROLS;
				this.mappingtable.set("selected", uri);
				this.fetchMapping();
			} else if(type=="process") {
				activeSet = NODE_PCONTROLS;
			} else if(type=="vocab") {
				activeSet = NODE_VCONTROLS;
			}
			// We only show the controls for the active type
			NODE_CONTROLS.all(".controlset").addClass("hidden");
			activeSet.removeClass("hidden");
			
			

		},
		
		// The bottom part of the screen fills the height of the viewport
		setBottomHeight : function() {
			var	bottomheight = NODE_TOP.get('winHeight') - NODE_TOP.get('clientHeight');  
			NODE_BOTTOM.setStyle("height", bottomheight+"px");
		},
		setSize : function() {
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
