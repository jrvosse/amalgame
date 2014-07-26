YUI.add('evaluater', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_MAPPING_LIST = Y.one("#mappinglist"),
		NODE_MAPPING_TABLE = Y.one("#mappingtable"),
		NODE_INFO = Y.one("#mappinginfo"),
		NODE_CONCEPTS = Y.one("#concepts"),
		NODE_SOURCE_ALL = Y.one("#allsources"),
		NODE_TARGET_ALL = Y.one("#alltargets"),
		NODE_DETAIL = Y.one("#detail");

	function Evaluater(config) {
		Evaluater.superclass.constructor.apply(this, arguments);
	}
	Evaluater.NAME = "evaluater";
	Evaluater.ATTRS = {
		alignment: { value: null },
		paths: { value: {} },
		mappings: { value:{} },
		selected: { value: null },

		relations: { value: {} },
		allsources: { value: false },
		alltargets: { value: false }
	};

	Y.extend(Evaluater, Y.Base, {

		initializer: function(args) {
			this._initInfo();
			this._initList();
			this._initTable();
			this._initDetail();

			// bind the modules

			this.mappinglist.on("mappingSelect", this._onMappingSelect, this);
			this.mappinglist.on("wrapAround", this._onWrapAround, this);
			this.mappingtable.on("rowSelect", this._onCorrespondenceSelect, this);
			NODE_DETAIL.all(".setall").on("click", this._onSubmit, this, "setall");
			NODE_DETAIL.all(".submit").on("click", this._onSubmit, this, "submit");
			NODE_DETAIL.all(".next").on(  "click", this._onSubmit, this, "next");
			NODE_DETAIL.all(".prev").on(  "click", this._onSubmit, this, "prev");
			NODE_DETAIL.all(".cancel").on("click", this._onCancel, this);

			NODE_SOURCE_ALL.on("click", this._fetchDetail, this);
			NODE_TARGET_ALL.on("click", this._fetchDetail, this);

			var selected = this.get("selected");
			if (selected) this.mappinglist.fire('mappingSelect', {uri:selected});

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
						totalNumberOfResults:"total",
						stats:"stats",
						offset:"offset"
					}
			}
			});

			this.mappingtable = new Y.MappingTable({
				srcNode: NODE_MAPPING_TABLE,
				datasource:DS,
				alignment: this.get("alignment"),
				focus:this.get("selected")
			});
		},

		_initDetail : function() {
			this.detailOverlay = new Y.Overlay({
			srcNode:NODE_DETAIL,
			visible:false,
			width:"90%"
		}).render();
		},

		_onMappingSelect : function(e) {
			var uri = e.uri;
			this.set("selected", uri);
			this.detailOverlay.set("visible", false);
			this._fetchInfo(uri);
			this.mappingtable.set("mapping", uri);
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
			c.strategy = this.get("alignment");
			c.mapping   = this.get("selected");
			c.mode      = nav == "setall"?"all":"one";
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

		_onWrapAround : function(e) {
				  Y.log("got wraparound event");
				  window.scrollTo(e);
		},

		_onCancel : function(e) {
			e.preventDefault(e);
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
				oSelf = this,
				row = this._selectedRow;

			Y.io(server, {
				method: 'POST',
				data:c,
				on:{success:function(e,o) {
					if (c.mode == "one") {
						var r = Y.JSON.parse(o.responseText);
						row.one(".relation").setContent(r.relation.label);
					} else {
			  			oSelf.mappingtable.loadData(); // reload if all have been set at once
					}
					oSelf._fetchInfo(c.mapping);
				}}
			});
		},

		_fetchInfo : function(uri) {
			if(uri) {
				this.infoDS.sendRequest({
					request:'?url='+uri+'&alignment='+this.get("alignment"),
					callback:{success:function(o) {
						var HTML = o.response.results[0].responseText;
						NODE_INFO.setContent(HTML);
					}}
				})
			} else {
				NODE_INFO.empty();
			}
		},

		_fetchDetail : function() {
			var overlay = this.detailOverlay,
				node = this._selectedRow,
				server = this.get("paths").info;
			var mappings = this.get('mappings');
			var selected = this.get('selected');
			var sourcevoc = mappings[selected].stats.vocs.source.uri;
			var SourceConfig = { 
				source: '/api/autocomplete?q={query}', 
				resultListLocator: 'results',
				resultTextLocator: 'label',
				resultHighlighter: 'phraseMatch'
			};

			// position the overlay below the currently selected row
			overlay.set("width", node.get("offsetWidth"));
			overlay.set("align", { node:node,
				points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.BR]
				});

			// call the server
			var data = {
				alignment:this.get("alignment"),
				mapping:this.get("selected"),
				source: this._source,
				target: this._target,
				allsource: NODE_SOURCE_ALL.get("checked"),
				alltarget: NODE_TARGET_ALL.get("checked")
			};
			Y.io(server, {
				method: 'GET',
				data: data,
				on:{success:function(e,o) {
						NODE_CONCEPTS.setContent(o.responseText);
						Y.all('input[name=source]').plug(Y.Plugin.SkosAutoComplete, SourceConfig);
						Y.all('input[name=target]').plug(Y.Plugin.SkosAutoComplete, SourceConfig);
						overlay.set("visible", true);
					}
				}
			});
		}
	});

	Y.Evaluater = Evaluater;

}, '0.0.1', { requires: [
	'node','event','anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'querystring-stringify-simple',
	'mappinglist','mappingtable', 'skosautocomplete'
	]
});
