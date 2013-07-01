YUI.add('vocabulary', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_BROWSER = Y.one("#vocabularybrowser");

	function Vocabulary(config) {
		Vocabulary.superclass.constructor.apply(this, arguments);
	}
	Vocabulary.NAME = "vocabulary";
	Vocabulary.ATTRS = {
		conceptscheme : {
			value: null
		},
		alignment : {
			value: null
		},
		selected : {
			value: null
		},
		paths:{
			value:{
				concepts:"/skosapi/concepts",
				mappinglist:"amalgame/data/mappinglist"

			},
			validator: function(val) {
				return Lang.isObject(val)
			}
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
			var mappingHeader = NODE_BROWSER.appendChild(Node.create('<div class="header"></div>'));
			var titleBox = mappingHeader.appendChild(Node.create('<div class="title-box" title="click to show options"></div>'));
			this.title = titleBox.appendChild(Node.create('<span class="title"></span>'));
			var mappingSelect = mappingHeader.appendChild(Node.create('<div class="mapping-select"></div>'));
			mappingSelect.appendChild('<span>highlight mappings from: </span>');
			mappingList = mappingSelect.appendChild(Node.create('<ul class="mappings"></ul>'));


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
		},

		_initBrowser : function() {
			var selected = this.get("selected"),
				alignment = this.get("alignment"),
				vocabulary = (selected.type=="vocabulary") ? selected.uri : null,
				fetchConceptsURL = this.get("paths").concepts;

			// We define a datasource to simplify
			// access to the vocabularys later and add caching support
			var DS = new Y.DataSource.IO({
				source: ""
			})
			.plug(Plugin.DataSourceJSONSchema, {
				schema: {
					resultListLocator: "results",
					resultFields: ["id", "label", "hasNext", "count", "class"],
					metaFields: {
						totalNumberOfResults:"totalNumberOfResults"
					}
				}
			});
			//.plug(Plugin.DataSourceCache, {"max":10});

			this.browser = new Y.mazzle.ColumnBrowser({
				datasource:DS,
				autoLoad:false,
				maxNumberItems: 25,
				searchEnabled: false,
				columns: [
				{   request: fetchConceptsURL,
				    params: {type:'topconcept'},
				    options: [{'value':'inscheme', 'label':'all concepts'},
					      {'value':'topconcept', 'selected':'true', 'label':'top concepts'}]
				},
				{   request: fetchConceptsURL,
				    params: {type:'child'},
				    repeat: true
				}
				]
			});
			this.browser.render(NODE_BROWSER);
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
			var alignment = this.get("alignment"),
				mappingNode = this.mappingList,
				currentMappings = this._currentMappings;

			function formatMappings(e,o) {
				var mappings = Y.JSON.parse(o.responseText);
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
					'alignment':alignment
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
