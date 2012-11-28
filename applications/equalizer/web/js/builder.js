YUI.add('builder', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_OPM			= Y.one("#opm"),
		NODE_CONTROLS		= Y.one("#controls"),
		NODE_INFO			= Y.one("#info"),
		NODE_SELECT			= Y.one("#select");


	function Builder(config) {
		Builder.superclass.constructor.apply(this, arguments);
	}
	Builder.NAME = "builder";
	Builder.ATTRS = {
		alignment : {
			value: null
		},
		readonly : {
			value: true
		},
		paths:{
			value: {},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
		selected : {
			value:{},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
		nodes:{
			value:{},
			validator: function(val) {
				return Lang.isObject(val)
			}
		}
	};

	Y.extend(Builder, Y.Base, {

		initializer: function(args) {
			this.readonly = (this.get('readonly') != "false"); // string/boolean

			// initalize the different modules
			this._initLayout();
			this._initGraph();
			this._initControls();
			this._initInfo();
			this._initMapping();
			this._initVocabulary();

			// handlers for the controls
			if (!this.readonly) {
				this.controls.on("submit", this._onControlSubmit, this);
			} else {
			  Y.one('#hint').setContent('Please login to make changes');
			  Y.all('button').each(function(button) { button.setAttribute("disabled", true); });
			};

			// handlers for the infobox
			this.infobox.after("deleteNode", this._onNodeDelete, this);
			this.infobox.after("nodeUpdate", this._onNodeUpdate, this);
			this.infobox.on("evaluate", this._onEvaluate, this);     // used for hints
			this.infobox.on("submit", this._onControlSubmit, this);  // used for hints
			this.infobox.on("nodeSelect", this._onSelectedChange, this); // used for hints

			// handlers for graph and mapping
			this.opmviz.on("nodeSelect", this._onNodeSelect, this);
			this.mapping.on("evalSubmit", this._updateNodes, this);

			// handlers for changes on attributes
			// federate them to the components
			this.on('nodesChange', function(o) {
				var nodes = o.newVal;
				this.infobox.set("nodes", nodes);
				this.controls.set("nodes", nodes);
			}, this);

			this.on("selectedChange", this._onSelectedChange, this);
		},

		_onSelectedChange: function(o) {
			var selected = o.newVal?o.newVal:o.data.newVal ;
			this.opmviz.set("selected", selected);
			this.infobox.set("selected", selected);
			this.controls.set("selected", selected);
			this.mapping.set("selected", selected);
			this.vocabulary.set("selected", selected);
		},
		_initLayout : function() {
			var oSelf = this,
				controls = Y.one("#controls"),
				bottom = Y.one("#bottom"),
				graph = Y.one("#graph");
			
			function windowResize() {
				var graphWrapper = graph.get("parentNode"),
					contentHeight = oSelf._contentHeight(),
					graphHeight = contentHeight - (bottom.get("offsetHeight")+10);

				graph.setStyle("height", graphHeight);
				graphWrapper.setStyle("height", graphHeight);
				graph.setStyle("width", "100%");
				graphWrapper.setStyle("width", "100%");
				controls.setStyle("height", contentHeight);
			}
			
			// graph node is resizable in height
			var resize = new Y.Resize({
		        node: '#graph',
				handles: 'b',
				wrap: true
		    });
			resize.after("resize", this._onGraphResize, this);
			resize.after("end", this._onGraphResize, this);
			
			//			
			Y.on("windowresize", windowResize);
			windowResize();

		},

		_initGraph : function() {
			this.opmviz = new Y.OPMViz({
				paths:this.get("paths"),
				alignment: this.get("alignment"),
				selected: this.get("selected"),
				nodes: this.get("nodes")
			}).render(NODE_OPM);
		},

		_initInfo : function() {
			this.infobox = new Y.InfoBox({
				srcNode: NODE_INFO,
				alignment: this.get("alignment"),
				nodes: this.get("nodes"),
				selected: this.get("selected"),
				readonly: this.readonly,
				paths: this.get("paths")
			});
		},

		_initControls : function() {
			this.controls = new Y.Controls({
				srcNode: NODE_CONTROLS,
				selected: this.get("selected"),
				nodes: this.get("nodes")
			});
		},

		_initMapping : function() {
			this.mapping = new Y.Mapping({
				paths: this.get("paths"),
				alignment: this.get("alignment"),
				selected: this.get("selected")
			});
		},
		
		_initVocabulary : function() {
			this.vocabulary = new Y.Vocabulary({
				paths:this.get("paths"),
				selected: this.get("selected")
			});
		},

		// helper functions for resizing
		_contentHeight : function() {
			var winHeight = Y.DOM.winHeight(),
				headerHeight = Y.one("#header").get("offsetHeight");
			return (winHeight - headerHeight - 12);
		},
		_onGraphResize : function(e) {
			// the bottom node needs be updated according to the remaining space
			var bottomHeight = this._contentHeight()-(e.info.offsetHeight+10);
			Y.one("#bottom").setStyle("height", bottomHeight);
			// resize sets fixed width, but we want a fluid width
			Y.one("#graph").get("parentNode").setStyle("width", "100%");
			Y.one("#graph").setStyle("width", "100%");
		},

		/* handlers */
		_onControlSubmit : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;

			// data only contains the process parameters
			// we need to add the context
			data.alignment = this.get("alignment");
			if (data.lastAction) this.infobox.set("lastAction", data.lastAction);
			Y.io(paths.addprocess, {
				data:data,
				on:{success:function(e,o) {
					var r =	Y.JSON.parse(o.responseText);
					// we first update the graph before anything is computed on the server
					oSelf.opmviz.updateGraph();
					// setting the properties will then initiate the components
					// to fetch the latests stats
					oSelf.set("nodes", r.nodes);
					oSelf.set("selected", r.focus);
				}}
			});
		},

		_onNodeUpdate : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;
			data.alignment = this.get("alignment");

			Y.io(paths.updatenode, {
				data:data,
				on:{success:function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					oSelf.set("nodes", r.nodes);
					oSelf.set("selected", r.focus);
					if (!data.alignment == r.alignment) {
						// alignment changed name, we need to fully reload ...
						var l = window.location;
						var newURL = l.protocol + "//" + l.host + l.pathname +
							"?alignment=" + encodeURIComponent(r.alignment);
						window.location.replace(newURL);
					}
				}}
			})
		},

		_onNodeDelete : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				alignment = this.get("alignment");

			// inform the server and update the nodes
			Y.io(paths.deletenode, {
				data:{
					alignment:alignment,
					uri:o.uri
				},
				on:{success:function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					oSelf.set("nodes", r.nodes);
					oSelf.set("selected", r.focus);
				}}
			})
		},

		_onNodeSelect : function(e) {
			var selected = this.get("nodes")[e.uri];
			this.set("selected", selected);
		},

		_onEvaluate : function(e) {
			var focus = e.data.focus;
			if(focus) {
				window.location =	this.get("paths").eq_evaluate
					+'?alignment='+encodeURIComponent(this.get("alignment"))
					+"&focus="+encodeURIComponent(focus);
			}
		}

	});

	Y.Builder = Builder;

}, '0.0.1', { requires: [
	'node','event','anim','tabview','overlay',
	'io-base','datasource-io','datasource-cache',
	'querystring-stringify-simple',
	'gallery-node-accordion','mappingtable'
	]
});
