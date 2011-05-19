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
		NODE_SUBMIT = Y.one("#submit"),
		NODE_CANCEL = Y.one("#cancel"),
		//NODE_SOURCE_ALL = Y.one("#msources"),
		//NODE_TARGET_ALL = Y.one("#mtargets"),
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
				mapping:"/amalgame/data/mapping",
				mappinginfo:'/amalgame/private/info',
				info:"/amalgame/private/correspondence",
				evaluate:"/amalgame/data/evaluate"
				
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
			this._initInfo();
			this._initList();
			this._initTable();
			this._initDetail();
			
			// bind the modules
			this.mappinglist.on("mappingSelect", this._onMappingSelect, this);
			this.mappingtable.on("rowSelect", this._onRowSelect, this);
			NODE_SUBMIT.on("click", this._onSubmit, this);
			NODE_CANCEL.on("click", this._onCancel, this);
			//NODE_NEXT.on("click", this._onNext, this);
			//NODE_PREV.on("click", this._onPrev, this);
			
		},
		
		_initInfo : function() {
			var selected = this.get("selected");
			this.infoDS = new Y.DataSource.IO({
				source: this.get("paths").mappinginfo
			});
			if(selected) {
				this._fetchInfo(selected);
			}
		},
		
		_initList : function() {
			this.mappinglist = new Y.MappingList({
				mappings:this.get("mappings"),
				selected:this.get("selected")
			}).render(NODE_MAPPING_LIST);
		},	
		
		_initTable : function() {
			// We define a datasource to simplify 
			// access to the mappings later and add caching support
			var DS = new Y.DataSource.IO({
				source: this.get("paths").mapping
			})
			.plug(Plugin.DataSourceJSONSchema, {
				schema: {
					resultListLocator: "mapping",
		      		resultFields: ["source", "target", "relation"],
		      		metaFields: {
 						totalNumberOfResults:"total"
					}
		    	}
		  	});

			this.mappingtable = new Y.MappingTable({
				srcNode: NODE_MAPPING_TABLE,
				datasource:DS,
				mapping:this.get("selected")
			});
		},
		
		_initDetail : function() {
			this.detailOverlay = new Y.Overlay({
        		srcNode:NODE_DETAIL,
        		visible:false,
				centered:true,
				constrain:true,
        		width:"90%"
    		}).render();
		},
		
		_onMappingSelect : function(e) {
			var uri = e.uri;
			this.set("selected", uri);
			this._fetchInfo(uri);
			this.mappingtable.set("mapping", uri);
		},
		
		_onRowSelect : function(e) {
			var overlay = this.detailOverlay,
				server = this.get("paths").info,
				data = {
					source: e.sourceConcept.uri,
					target: e.targetConcept.uri
					//allsource: NODE_SOURCE_ALL.get("value"),
					//alltarget: NODE_TARGET_ALL.get("value")
				};
				
			this.row = e.row;	
			Y.io(server, {
				data: data,
				on:{success:function(e,o) {
						NODE_CONCEPTS.setContent(o.responseText);
						overlay.set("visible", true);
					}
				}
			});		
		},
		
		_onSubmit : function(e) {
			e.preventDefault(e);
			var cs = this._getSelection();
			var c = cs[0];
			c.graph = this.get("alignment");
			this._submitCorrespondence(c, this.row);
			this.detailOverlay.set("visible", false);
		},
		
		_onCancel : function(e) {
			e.preventDefault(e);
			this.detailOverlay.set("visible", false);
		},
		
		_onNext : function(e) {
			e.preventDefault(e);
			var cs = this._getSelection();
			var c = cs[0];
			c.graph = this.get("alignment");
			this._submitCorrespondence(c);
			//var next = this.mappingtable.nextRow(this.row);
		},
		
		_onPrev : function(e) {
			e.preventDefault(e);
		},
		
		_getSelection : function() {
			var cs = [];
			Y.all(".relations").each(function(node) {
				var source = node.one("input[name=source]").get("value"),
					target = node.one("input[name=target]").get("value"),
					relation = node.one("input:checked").get("value"),
					comment = node.one("input[name=comment]").get("value");
				cs.push({
					source:source,
					target:target,
					relation:relation,
					comment:comment
				});
			});
			return cs;
		},
		
		_submitCorrespondence : function(c, row) {	
			var server = this.get("paths").evaluate;
			Y.io(server, {
				data:c,
				on:{success:function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					row.one(".relation").setContent(r.relation.label);
				}}
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
	'node','event','anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'querystring-stringify-simple',
	'mappinglist','mappingtable'
	]
});
