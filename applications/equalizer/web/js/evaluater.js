YUI.add('evaluater', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_MAPPING_LIST = Y.one("#mappinglist"),
		NODE_MAPPING_TABLE = Y.one("#mappingtable"),
		NODE_INFO = Y.one("#mappinginfo"),
		NODE_CONCEPTS = Y.one("#concepts"),
		NODE_NEXT = Y.one("#next"),
		NODE_PREV = Y.one("#prev"),
		NODE_DETAIL = Y.one("#detail");
	
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
				mapping:"/amalgame/mapping",
				mappinginfo:'/amalgame/private/info',
				info:"/amalgame/private/resourcecontext",
				relation:"/amalgame/updaterelation"
				
			},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
		mappings:{
			value:[],
			validator: function(val) {
				return Lang.isArray(val)
			}
		},
		relations: {
	        value: {},
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
			this._selected = {};
			this._initInfo();
			this._initList();
			this._initTable();
			this._initDetail();
			// bind the modules
			this.mappinglist.on("mappingSelect", function(o) {
				this.set("selected", o);
				this._fetchInfo(o.uri);
				this.mappingtable.set("mapping", o.uri);
			}, this);
			
			this.mappingtable.on("rowSelect", this._onRowSelect, this);
			//Y.delegate("click", this._onRelationSelect, NODE_RELATIONS, "input", this);
		},
		
		_initInfo : function() {
			this.infoDS = new Y.DataSource.IO({
				source: this.get("paths").mappinginfo
			})
		},
		
		_initList : function() {
			this.mappinglist = new Y.MappingList({
				mappings:this.get("mappings")
			}).render(NODE_MAPPING_LIST);
		},	
		_initTable : function() {
			var paths = this.get("paths"),
				mapping = this.get("selected");
			
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
				datasource:DS,
				mapping:mapping
			});
		},
		
		_initDetail : function() {
			this.detailOverlay = new Y.Overlay({
        		srcNode:NODE_DETAIL,
        		visible:false,
				centered:true,
        		width:"90%"
    		}).render();
		},
		
		_onRowSelect : function(e) {
			var server = this.get("paths").info,
				data = {
					relation: e.relation.uri,
					source: e.sourceConcept.uri,
					target: e.targetConcept.uri,
					allSource: NODE_SOURCEALL.get("value"),
					allTarge: NODE_TARGETALL.get("value")
				};
		
			Y.io(server, {
				data: data,
				on:{success:function(e,o) {
						NODE_CONCEPTS.setContent(o.responseText);
						}
					}
			});		
			this.detailOverlay.set("visible", true);
		},
		
		_onRelationSelect : function(e) {
			var server = this.get("paths").relation,
				relation = e.target.get("value"),
				comment = NODE_COMMENT.get("value"),
				label = e.target.get("parentNode").one("label").getContent();
			
			Y.io(server, {
				data:{
					source:source,
					target:target,
					relation:relation,
					comment:comment
				}
			});
		},
		
		_fetchInfo : function(uri) {
			this.infoDS.sendRequest({
				request:'?url='+uri,
				callback:{success:function(o) {
					var HTML = o.response.results[0].responseText;
					NODE_INFO.setContent(HTML);
				}}
			})
		}
				
/*
						target.all(".moretoggle").on("click", function(e) {
   							p = e.currentTarget.get("parentNode");
   							p.all(".moretoggle").toggleClass("hidden");
   							p.one(".morelist").toggleClass("hidden");
						})
*/				
				
	});
	
	Y.Evaluater = Evaluater;
	
}, '0.0.1', { requires: [
	'node','event','anim','overlay','io',
	'datasource-io','datasource-jsonschema','datasource-cache',
	'querystring-stringify-simple',
	'mappinglist','mappingtable'
	]
});
