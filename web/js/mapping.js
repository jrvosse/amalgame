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
		strategy : { value: null },
 		focus    : { value: null },
 		builder  : { value: null },
		paths:{
			value:{
				mapping:"/amalgame/data/mapping",
				cinfo:"/amalgame/private/correspondence",
				evaluate:"/amalgame/data/evaluate"

			},
			validator: function(val) {
				return Lang.isObject(val)
			}
		}
	};

	Y.extend(Mapping, Y.Base, {

		initializer: function(args) {
			this._initTable();
			this._initDetail();
			
			// hide the table on start
			NODE_MAPPING_TABLE.addClass("hidden");
			
			// bind the modules
			this.after("focusChange", this._onSelectedChange, this);
			this.mappingtable.on("rowSelect", this._onCorrespondenceSelect, this);

			NODE_DETAIL.all(".next").on("click", this._onSubmit, this, "next");
			NODE_DETAIL.all(".prev").on("click", this._onSubmit, this, "prev");
			NODE_DETAIL.all(".cancel").on("click", this._onCancel, this);
			NODE_DETAIL.all(".submit").on("click", this._onSubmit, this);

			NODE_SOURCE_ALL.on("click", this._fetchDetail, this);
			NODE_TARGET_ALL.on("click", this._fetchDetail, this);
		},

		_initTable : function() {
			var focus = this.get("focus");
			if (focus.type != "mapping") focus = null;
				
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
						totalNumberOfResults:"total",
						stats:"stats"
					}
				}
			});

			this.mappingtable = new Y.MappingTable({
				srcNode: NODE_MAPPING_TABLE,
				datasource:DS,
				strategy: this.get("strategy"),
				focus: focus
			});
		},

		_initDetail : function() {
			this.detailOverlay = new Y.Overlay({
				id: "detail_overlay",
			        srcNode:NODE_DETAIL,
				visible:false,
				width: '85%',
				height:'95%',
				align:{points:[Y.WidgetPositionAlign.TC, Y.WidgetPositionAlign.TC]}
			}).render();
			NODE_DETAIL.removeClass("hidden");
		},

		_onSelectedChange : function() {
			var focus = this.get("focus");
			this.detailOverlay.set("visible", false);
			if(focus.type=="mapping") {
				this.mappingtable.set("focus", focus);
				NODE_MAPPING_TABLE.removeClass("hidden");
			} else {
				NODE_MAPPING_TABLE.addClass("hidden");
			}
		},

		_onCorrespondenceSelect : function(e) {
			this._focusRow = e.row;
			this._source = e.sourceConcept.uri;
			this._target = e.targetConcept.uri;
			this._fetchDetail();
		},

		_onSubmit : function(e, nav) {
			e.preventDefault();

			this.detailOverlay.set("visible", false);
			var cs = this._getSelection();
			var c = cs[0];
			c.strategy = this.get("strategy");
			c.mapping   = this.get("focus").uri;
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

			  this._source = nextRecord.get("source").uri;
			  this._target = nextRecord.get("target").uri;
			  this._selectedRow = nextRow;
			  this._fetchDetail();
			}
		},

		_onCancel : function(e) {
			e.preventDefault();
			this.detailOverlay.set("visible", false);
		},

		_getSelection : function() {
			var cs = [];
			Y.all(".manualfixes").each(function(node) {
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
			var builder = this.get('builder');
			var overlay = this.detailOverlay,
				node = this._selectedRow,
				server = this.get("paths").cinfo;

			var data = {
				strategy:this.get("strategy"),
				mapping:this.get("focus").uri,
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
						builder.onWindowResize();
					}
				}
			});
		}
	});

	Y.Mapping = Mapping;

}, '0.0.1', { requires: [
	'node','event','anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'querystring-stringify-simple',
	'mappingtable'
	]
});
