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
		strategy:   { value: null  },
		paths:      { value: {}    },
		mappings:   { value: {}    },
		selected:   { value: null  },
		editmode:   { value: 'eval'}, // or edit. 
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
			this.after('editmodeChange', this._onEditModeChange, this);
			NODE_DETAIL.all(".setall").on("click", this._onSubmit, this, "setall");
			NODE_DETAIL.all(".submit").on("click", this._onSubmit, this, "submit");
			NODE_DETAIL.all(".delete").on("click", this._onSubmit, this, "delete");
			NODE_DETAIL.all(".next")  .on("click", this._onSubmit, this, "next");
			NODE_DETAIL.all(".prev")  .on("click", this._onSubmit, this, "prev");
			NODE_DETAIL.all(".cancel").on("click", this._onCancel, this);

			NODE_SOURCE_ALL.on("click", this._fetchDetail, this);
			NODE_TARGET_ALL.on("click", this._fetchDetail, this);

			var selected = this.get('selected');
			if (selected) {
				var isReference =this.get('mappings')[selected].agStatus == 'reference' 
				this.mappinglist.fire('mappingSelect', {uri:selected, isReference:isReference});
			}

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
			this.on('mappingsChange', function(e) { 
				this.mappinglist.set('mappings', e.newVal); 
			}, this);
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
				showRelation:true,
				strategy: this.get("strategy"),
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
			if (e.isReference) this.set('editmode', "edit"); else this.set('editmode', "eval");
			this.set("selected", e.uri);
			this.detailOverlay.set("visible", false);
			this._fetchInfo(e.uri);
			this.mappingtable.set("mapping", e.uri);
		},

		_onCorrespondenceSelect : function(e) {
			this._selectedRow = e.row;
			this._selectedRecord = e.selected;
			this._source   = e.sourceConcept.uri;
			this._target   = e.targetConcept.uri;
			this._fetchDetail();
		},

		_onConceptChange: function(ev, context) {
			var item = ev.details[0].result.raw;
			NODE_DETAIL.one('.'+context.target).setContent(item.uri);
			NODE_DETAIL.all(".changed").set('disabled', false); 
		},

		_onSubmit : function(e, nav) {
			e.preventDefault(e);
			this.detailOverlay.set("visible", false);
			var cs = this._getSelection();
			var c = cs[0];
			c.strategy = this.get("strategy");
			c.mapping   = this.get("selected");
			c.editmode  = this.get("editmode");
			c.applyTo   = nav == "setall"?"all":"one";
			c.remove    = nav == "delete";
			if (this.form_dirty() || c.remove) {
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

		_onEditModeChange: function(e) {
			if (e.newVal == 'edit') {
				Y.all('#header, #header a').setStyle('color', '#ACCF89');
				Y.one('#detail').setStyle('background-color', '#ACCF89');
				Y.one('#agMessages').setContent('Warning: edits will be applied to the reference alignment graph directly');
			} else {
				Y.all('#header, #header a').setStyle('color', '#3875D7');
				Y.one('#detail').setStyle('background-color', '#DDD');
				Y.one('#agMessages').setContent('Changes will be recorded in the manual reference alignment graph');
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
				var source = node.one("div.sourceuri").getContent();
				var target = node.one("div.targeturi").getContent();
				var checked = node.one("input:checked");
				var relation = checked?checked.get("value"):null;
				var comment = node.one("input[name=comment]").get("value");
				var origsource = node.one("div.sourcediv input.original").get("value");
				var origtarget = node.one("div.targetdiv input.original").get("value");
				var values = Y.JSON.stringify(
					{ source:source, target: target });
				var originals = Y.JSON.stringify(
					{ source: origsource, target: origtarget });
				cs.push({
					'new':       values,
					originals: originals,
					relation:  relation,
					comment:   comment
				});
			});
			return cs;
		},

		_submitCorrespondence : function(c) {
			var oSelf  =   this;
			var mappings = this.get("mappings")
			var server =   this.get("paths").evaluate;
			var record =   this._selectedRecord;
			var conceptsUnchanged = (c.new == c.originals);

			Y.io(server, {
				method: 'POST',
				data:c,
				on:{success:function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					if (c.applyTo == "one" && conceptsUnchanged) {
						if (c.remove) {
							oSelf.mappingtable.removeRow(record);
						} else if (r.overruled) {
							oSelf.mappingtable.modifyRow(record, r.overruled);
						} else if (r.add) {
							oSelf.mappingtable.modifyRow(record, r.add);
						}
					} else { // applyTo == setall or !conceptsUnchanged
			  			oSelf.mappingtable.loadData(); // reload if too much has changed
					}
					oSelf._fetchInfo(c.mapping);
					if (r.mapping.uri && r.mapping.stats) {
						mappings[r.mapping.uri].stats = r.mapping.stats;
					       	oSelf.set("mappings", mappings);
					}
				}}
			});
		},

		_fetchInfo : function(uri) {
			if(uri) {
				this.infoDS.sendRequest({
					request:'?url='+uri+'&strategy='+this.get("strategy"),
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
			var oSelf = this;
			var overlay = this.detailOverlay,
				node = this._selectedRow,
				server = this.get("paths").info;

			// position the overlay below the currently selected row
			overlay.set("width", node.get("offsetWidth"));
			overlay.set("align", { node:node,
				points:[Y.WidgetPositionAlign.TR, Y.WidgetPositionAlign.BR]
				});

			// call the server
			var data = {
				strategy:  this.get("strategy"),
				mapping:   this.get("selected"),
				source:    this._source,
				target:    this._target,
				fillmode:  'fill-in',
				allsource: NODE_SOURCE_ALL.get("checked"),
				alltarget: NODE_TARGET_ALL.get("checked")
			};
			Y.io(server, {
				method: 'GET',
				data: data,
				on:{success:function(e,o) {
						NODE_CONCEPTS.setContent(o.responseText);
						oSelf.init_active_form_elements();
						overlay.set("visible", true);
					}
				}
			});
		},

		form_dirty: function() {
			return (NODE_DETAIL.one(".changed").get('disabled') == false);
		},
		init_active_form_elements: function() {
			function activate_changed(e) { 
				NODE_DETAIL.all(".changed").set('disabled', false); 
				NODE_DETAIL.all(".notchanged").set('disabled', true); 
			};
			// Disable submit buttons until ...
			NODE_DETAIL.all(".changed").set('disabled', true);
			NODE_DETAIL.all(".notchanged").set('disabled', false);
			// ... we have something to submit
			NODE_DETAIL.all(".manualfixes .relation").on("change", activate_changed);
			NODE_DETAIL.all(".skos_ac_field").on("select", activate_changed);
		
			// Activate skos autocompletion on sourceuri, targeturi input nodes:	
			var mappings = this.get('mappings');
			var selected = this.get('selected');
			var svoc = mappings[selected].stats.vocs.source.uri;
			var tvoc = mappings[selected].stats.vocs.target.uri;
			var SourceConfig = { 
				// our skos-specific ac attrs:
				caller: this,
				handler: this._onConceptChange,
				context: { target:'sourceuri' },
				
				// YUI autocomplete attrs:
				source: '/api/autocomplete?q={query}&filter=[{\"scheme":\"'+svoc+'\"}]',
				resultListLocator: 'results',
				resultTextLocator: 'label',
				resultHighlighter: 'phraseMatch'
			};
			var TargetConfig = { 
				// our skos-specific ac attrs:
				caller: this,
				handler: this._onConceptChange,
				context: { target:'targeturi' },

				// YUI autocomplete attrs:
				source: '/api/autocomplete?q={query}&filter=[{\"scheme":\"'+tvoc+'\"}]',
				resultListLocator: 'results',
				resultTextLocator: 'label',
				resultHighlighter: 'phraseMatch'
			};
			Y.all('input[name=source]').plug(Y.Plugin.SkosAutoComplete, SourceConfig);
			Y.all('input[name=target]').plug(Y.Plugin.SkosAutoComplete, TargetConfig);
		}
	});

	Y.Evaluater = Evaluater;

}, '0.0.1', { requires: [
	'node','event','json', 'anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'mappinglist','mappingtable', 'skosautocomplete'
	]
});
