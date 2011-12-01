YUI.add('mapping', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_MAPPING_TABLE = Y.one("#mappingtable"),
		NODE_CONCEPTS = Y.one("#concepts"),
		NODE_SOURCE_ALL = Y.one("#allsources"),
		NODE_TARGET_ALL = Y.one("#alltargets"),
		NODE_DETAIL = Y.one("#detail");

	function Mapping(config) {
		Mapping.superclass.constructor.apply(this, arguments);
	}
	Mapping.NAME = "mapping";
	Mapping.ATTRS = {
		alignment : {
			value: null
		},
 		selected : {
			value: null
		},
		paths:{
			value:{
				mapping:"/amalgame/data/mapping",
				cinfo:"/amalgame/private/correspondence",
				evaluate:"/amalgame/data/evaluate"

			},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
	};

	Y.extend(Mapping, Y.Base, {

		initializer: function(args) {
			this._initTable();
			this._initDetail();

			// bind the modules
			this.after("selectedChange", this._onSelectedChange, this);
			this.mappingtable.on("rowSelect", this._onCorrespondenceSelect, this);
			NODE_DETAIL.one(".submit").on("click", this._onSubmit, this);
			NODE_DETAIL.one(".next").on("click", this._onSubmit, this, "next");
			NODE_DETAIL.one(".prev").on("click", this._onSubmit, this, "prev");
			NODE_DETAIL.one(".cancel").on("click", this._onCancel, this);

			NODE_SOURCE_ALL.on("click", this._fetchDetail, this);
			NODE_TARGET_ALL.on("click", this._fetchDetail, this);
		},

		_initTable : function() {
			var selected = this.get("selected"),
				mapping = (selected.type=="mapping") ? selected.uri : null;
				
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
				alignment: this.get("alignment"),
				mapping:mapping
			});
		},

		_initDetail : function() {
			this.detailOverlay = new Y.Overlay({
				srcNode:NODE_DETAIL,
				visible:false,
				width:"80%",
				align:{points:[Y.WidgetPositionAlign.TC, Y.WidgetPositionAlign.TC]}
			}).render();
		},

		_onSelectedChange : function() {
			var selected = this.get("selected");
			if(selected.type=="mapping") {
				this.detailOverlay.set("visible", false);
				this.mappingtable.set("mapping", selected.uri);
			}	
		},

		_onCorrespondenceSelect : function(e) {
			this._selectedRow = e.row;
			this._source = e.sourceConcept.uri;
			this._target = e.targetConcept.uri;
			this._fetchDetail();
		},

		_onSubmit : function(e, nav) {
			e.preventDefault(e);
			this.detailOverlay.set("visible", false);
			var cs = this._getSelection();
			var c = cs[0];
			c.alignment = this.get("alignment");
			c.mapping   = this.get("selected").uri;
			if (c.relation) {
			  this._submitCorrespondence(c);
			}
			var currentRow = this._selectedRow;
			var nextRow = this.mappingtable.nextRow(currentRow);
			var nextRecord =  this.mappingtable.nextRecord(currentRow);
			if(nav=="prev") {
			  nextRow = this.mappingtable.prevRow(currentRow);
			  nextRecord =  this.mappingtable.prevRecord(currentRow);
			}
			if ( nav=="prev" || nav=="next") {
			  currentRow.removeClass("yui3-datatable-selected");
			  nextRow.addClass("yui3-datatable-selected");

			  this._source = nextRecord.getValue("source").uri;
			  this._target = nextRecord.getValue("target").uri;
			  this._selectedRow = nextRow;
			  this._fetchDetail();
			}
		},

		_onCancel : function(e) {
			e.preventDefault(e);
			this.detailOverlay.set("visible", false);
		},

		_getSelection : function() {
			var cs = [];
			Y.all(".relations").each(function(node) {
				var source = node.one("input[name=source]").get("value"),
					target = node.one("input[name=target]").get("value"),
					checked = node.one("input:checked"),
					relation = checked?checked.get("value"):null,
					comment = node.one("input[name=comment]").getContent();
				cs.push({
					source:source,
					target:target,
					relation:relation,
					comment:comment
				});
			});
			return cs;
		},

		_submitCorrespondence : function(c) {
			var server = this.get("paths").evaluate,
				row = this._selectedRow;

			Y.io(server, {
				data:c,
				on:{success:function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					row.one(".relation").setContent(r.relation.label);
				}}
			});
			this.fire("evalSubmit", c);
		},

		_fetchDetail : function() {
			var overlay = this.detailOverlay,
				node = this._selectedRow,
				server = this.get("paths").cinfo;

			// position the overlay below the currently selected row
			//overlay.set("width", node.get("offsetWidth"));
			//overlay.set("align", {
			//node:node,
			//points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.BR]
			//});

			// call the server
			var data = {
				alignment:this.get("alignment"),
				mapping:this.get("selected").uri,
				source: this._source,
				target: this._target,
				allsource: NODE_SOURCE_ALL.get("checked"),
				alltarget: NODE_TARGET_ALL.get("checked")
			};
			Y.io(server, {
				data: data,
				on:{success:function(e,o) {
						NODE_CONCEPTS.setContent(o.responseText);
						overlay.set("visible", true);
					}
				}
			});
		}


/*
						target.all(".moretoggle").on("click", function(e) {
							p = e.currentTarget.get("parentNode");
							p.all(".moretoggle").toggleClass("hidden");
							p.one(".morelist").toggleClass("hidden");
						})
*/

	});

	Y.Mapping = Mapping;

}, '0.0.1', { requires: [
	'node','event','anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'querystring-stringify-simple',
	'mappingtable'
	]
});
