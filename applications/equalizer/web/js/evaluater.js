YUI.add('evaluater', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_MAPPING_LIST = Y.one("#mappinglist"),
		NODE_MAPPING_TABLE = Y.one("#mappingtable");
	
	function Evaluater(config) {
		Evaluater.superclass.constructor.apply(this, arguments);
	}
	Evaluater.NAME = "evaluater";
	Evaluater.ATTRS = {
		alignment : {
			value: null
		},
		selected : {
			value: null
		},
		paths:{
			value:{
			},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
		mappings:{
			value:{
				mapping:"/amalgame/mapping"
			},
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
	
	Y.extend(Evaluater, Y.Base, {
		
		initializer: function(args) {
			this._initList();
			this._initTable();
			
			// bind the modules
			this.mappinglist.on("mappingSelect", function(o) {
				console.log(o);
				this.mappingtable.set("selected", o.uri);
			}, this);
			
		},
		
		_initList : function() {
			this.mappinglist = new Y.MappingList({
				mappings:this.get("mappings")
			}).render(NODE_MAPPING_LIST);
		},	
		_initTable : function() {
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
				srcNode: NODE_MAPPING_TABLE,
				datasource:DS
			});
		}
				
	});
	
	Y.Evaluater = Evaluater;
	
}, '0.0.1', { requires: [
	'node','event','anim',
	'datasource-io','datasource-jsonschema','datasource-cache',
	'mappinglist','mappingtable'
	]
});
