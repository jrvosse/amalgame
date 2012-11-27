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
		selected : {
			value: null
		},
		paths:{
			value:{
				concepts:"/skosapi/concepts"

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
			
			this._initBrowser();
			this.after("selectedChange", this._onSelectedChange, this);
		},

		_initBrowser : function() {
			var selected = this.get("selected"),
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
					resultFields: ["id", "label", "hasNext", "matches", "scheme"],
					metaFields: {
						totalNumberOfResults:"totalNumberOfResults"
					}
				}
			})
			.plug(Plugin.DataSourceCache, {"max":10});

			this.browser = new Y.mazzle.ColumnBrowser({
				datasource:DS,
				autoLoad:false,
				columns: [
			    	{   request: fetchConceptsURL,
						params: {type:'topconcept'},
						options: [
					  		{value:'inscheme', label:'all concepts'},
					  		{value:'topconcept', selected:true, label: 'top concepts'}
					 	]
			    	},
			    	{   request: fetchConceptsURL,
						params: {type:'child'},
						options: [],
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
			} else {
				NODE_BROWSER.addClass("hidden");
			}
		},

	});

	Y.Vocabulary = Vocabulary;

}, '0.0.1', { requires: [
	'node','event','anim','overlay','io-base',
	'datasource-io','datasource-jsonschema',
	'querystring-stringify-simple',
	'columnbrowser'
	]
});
