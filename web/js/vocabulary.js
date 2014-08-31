/* Widget displaying a columnbrowser for skos concepts when the current "selected" node is of type "vocab"
 * Uses the browser from the skos_browser cpack.
 */

YUI.add('vocabulary', function(Y) {
	var	NODE_BROWSER = Y.one("#vocabularybrowser");

	function Vocabulary(config) {
		Vocabulary.superclass.constructor.apply(this, arguments);
	}
	Vocabulary.NAME = "vocabulary";
	Vocabulary.ATTRS = {
		conceptscheme : { value: null },
		strategy : { value: null },
		selected : { value: null },
		concept  : { value: null },
		paths:{ value:{
				concepts:"/skosapi/concepts",
				mappinglist:"amalgame/data/mappinglist" }
		}
	};

	Y.extend(Vocabulary, Y.Base, {

		initializer: function(args) {
			// hide the browser on start
			NODE_BROWSER.addClass("hidden");

			this._initHeader();
			this._initBrowser();
			this.after("selectedChange", this._onSelectedChange, this);

			this._currentMappings = {};
		},

		_initHeader : function() {
			var oSelf = this;

			var selected = this.get("selected");
			var mappingHeader = NODE_BROWSER.appendChild(Y.Node.create('<div class="header"></div>'));
			var titleBox = mappingHeader.appendChild(Y.Node.create('<div class="title-box" title="click to show options"></div>'));
			this.title = titleBox.appendChild(Y.Node.create('<span class="title"></span>'));
			var mappingSelect = mappingHeader.appendChild(Y.Node.create('<div class="mapping-select"></div>'));
			mappingSelect.appendChild('<span>highlight mappings from: </span>');
			mappingList = mappingSelect.appendChild(Y.Node.create('<ul class="mappings"></ul>'));


			mappingList.delegate('click', function(e) {
				var graphs = [];
				mappingList.all('input').each(function(o) {
					if(o.get("checked")) {
						graphs.push(o.get("value"));
					}
				});
				oSelf.browser.updateAll({graph:graphs});
			}, 'input');

			titleBox.on("click", function(e) {
				mappingSelect.toggleClass("hidden");
			});

			this.mappingList = mappingList;
			this.mappingSelect = mappingSelect;
		},

		_initBrowser : function() {
			var fetchConceptsURL = this.get("paths").concepts;
			var strategy = this.get('strategy');

			// We define a datasource to simplify
			// access to the vocabularys later and add caching support
			var DS = new Y.DataSource.IO({
				source: ""
			})
			.plug(Y.Plugin.DataSourceJSONSchema, {
				schema: {
					resultListLocator: "results",
					resultFields: ["id", "label", "hasNext", "count", "class"],
					metaFields: {
						totalNumberOfResults:"totalNumberOfResults"
					}
				}
			});
			//.plug(Y.Plugin.DataSourceCache, {"max":10});

			this.browser = new Y.mazzle.ColumnBrowser({
				datasource:DS,
				autoLoad:false,
				maxNumberItems: 25,
				searchEnabled: false,
				columns: [
				{   request: fetchConceptsURL,
					params: { type:'topconcept', strategy:strategy },
				    options: [{'value':'inscheme', 'label':'all concepts'},
					      {'value':'topconcept', 'selected':'true', 'label':'top concepts'}]
				},
				{   request: fetchConceptsURL,
				    params: {type:'child', strategy:strategy },
				    repeat: true,
				    options: []
				}
				]
			});
			this.browser.render(NODE_BROWSER);
			this.browser.after('itemSelect', this._onItemSelect, this);
		},

		_onItemSelect : function (ev) {
			if (ev.id && ev.label) {
				var selectedItem = { id:ev.id, label:ev.label };
				this.set("concept", selectedItem);
			}
		},

		_onSelectedChange : function() {
			var selected = this.get("selected");
			if(selected.type=="vocab") {
				NODE_BROWSER.removeClass("hidden");
				this.browser._updateColumn(0, selected.uri);
				this.title.setContent(selected.label);
				this.fetchMappings();
			} else {
				NODE_BROWSER.addClass("hidden");
			}
		},

		fetchMappings : function() {
			var strategy = this.get("strategy"),
				mappingNode = this.mappingList,
				mappingSelect = this.mappingSelect,
				currentMappings = this._currentMappings;

			function formatMappings(e,o) {
				var mappings = Y.JSON.parse(o.responseText);
				if (mappings.length > 0) {
					mappingSelect.removeClass("hidden");
				} else {
					mappingSelect.addClass("hidden");
				}
				for (var i=0; i < mappings.length; i++) {
					var mapping = mappings[i],
						uri = mapping['uri'],
						label = mapping['label'];
					if(currentMappings[uri]) {
						var labelNode = currentMappings[uri].one('span');
						if(labelNode) { labelNode.setContent(label); }
					} else {
						currentMappings[uri] = mappingNode.appendChild('<li><input type="checkbox" autocomplete="off" value="'+uri+'"><span>'+mapping.label+'</span></li>');
					}
				}
			}

			Y.io(this.get("paths").mappinglist, {
				data: {
					'strategy':strategy
				},
				on:{
					success:formatMappings
				}
			});
		}

	});

	Y.Vocabulary = Vocabulary;

}, '0.0.1', { requires: [
	'node','event','anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'querystring-stringify-simple',
	'columnbrowser'
	]
});
