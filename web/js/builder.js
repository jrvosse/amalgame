YUI.add('builder', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_GRAPH			= Y.one("#strategy_graph"),
		NODE_CONTROLS			= Y.one("#controls"),
		NODE_INFO			= Y.one("#info"),
		NODE_SELECT			= Y.one("#select");


	function Builder(config) {
		Builder.superclass.constructor.apply(this, arguments);
	}
	Builder.NAME = "builder";
	Builder.ATTRS = {
		strategy: { value: null },
		readonly: { value: true },
		paths:    { value: {} },
		selected: { value: {} },
		nodes:    { value: {} },
		currentConcept: { value: null } // selected concept from SKOS browser
	};

	Y.extend(Builder, Y.Base, {

		initializer: function(args) {
			// initalize the different modules
			this._initGraph();
			this._initControls();
			this._initInfo();
			this._initMapping();
			this._initVocabulary();
			this._initLayout();

			// handlers for the controls
			if (!this.get('readonly')) {
				this.controls.on("submit", this._onControlSubmit, this);
			} else {
			  Y.one('#hint').setContent('Readonly mode: Please login to make changes');
			  Y.all('#controls button').each(function(button) { button.setAttribute("disabled", true); });
			};

			// handlers for the infobox
			this.infobox.after("deleteNode", this._onNodeDelete, this);
			this.infobox.after("nodeUpdate", this._onNodeUpdate, this);
			this.infobox.on("evaluate", this._onEvaluate, this);     // used for hints
			this.infobox.on("submit", this._onControlSubmit, this);  // used for hints
			this.infobox.on("nodeSelect", this._onSelectedChange, this); // used for hints

			// handlers for graph and mapping
			this.strategy_viz.on("nodeSelect", this._onNodeSelect, this);
			this.mapping.on("evalSubmit", this._updateNodes, this);

			// handlers for changes on attributes
			// federate them to the components
			this.on('nodesChange', function(o) {
				var nodes = o.newVal;
				Y.log('nodesChange detected in builder');
				this.infobox.set("nodes", nodes);
				this.controls.set("nodes", nodes);
			}, this);

			this.on("selectedChange", this._onSelectedChange, this);
		},

		_onSelectedChange: function(o) {
			var selected = o.newVal?o.newVal:o.data.newVal ;
			this.strategy_viz.set("selected", selected);
			this.infobox.set("selected", selected);
			this.controls.set("selected", selected);
			this.mapping.set("selected", selected);
			this.vocabulary.set("selected", selected);
		},

		onWindowResize : function() {
			var oSelf = this;
			function _onWindowResize() {
				var controls = Y.one("#controls");
				var bottom = Y.one("#bottom");
				var graph = Y.one("#graph");
				var graphWrapper = graph.get("parentNode"),
					evidences = Y.one(".evidences"),
					contentHeight = oSelf._contentHeight(),
					graphHeight = contentHeight - (bottom.get("offsetHeight")+10);
					evHeight = 0.97 * contentHeight - 380;

				graph.setStyle("height", graphHeight);
				graphWrapper.setStyle("height", graphHeight);
				graph.setStyle("width", "100%");
				graphWrapper.setStyle("width", "100%");
				controls.setStyle("height", contentHeight);
				if (evidences) {
					evidences.setStyle("max-height", evHeight);
				}
			}
			Y.detach("windowresize", _onWindowResize);
			Y.on("windowresize", _onWindowResize);
			_onWindowResize();
	    	},
			
		_initLayout : function() {
			// graph node is resizable in height
			var resize = new Y.Resize({
		        	node: '#graph',
				handles: 'b',
				wrap: true
		    	});
			resize.after("resize", this._onGraphResize, this);
			resize.after("end", this._onGraphResize, this);

			this.onWindowResize();
		},

		_initGraph : function() {
			this.strategy_viz = new Y.StratViz({
				paths:this.get("paths"),
				strategy: this.get("strategy"),
				selected: this.get("selected")
			}).render(NODE_GRAPH);
		},

		_initInfo : function() {
			this.infobox = new Y.InfoBox({
				srcNode: NODE_INFO,
				strategy: this.get("strategy"),
				nodes: this.get("nodes"),
				selected: this.get("selected"),
				readonly: this.get('readonly'),
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
				builder: this,
				paths: this.get("paths"),
				strategy: this.get("strategy"),
				selected: this.get("selected")
			});
		},
		
		_initVocabulary : function() {
			this.vocabulary = new Y.Vocabulary({
				paths:this.get("paths"),
				selected: this.get("selected"),
				strategy: this.get("strategy")
			});
			this.vocabulary.on("conceptChange", this._onConceptChange, this);
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
		_onConceptChange : function(ev) {
			this.controls.set('currentConcept', ev.newVal);
		},

		_onControlSubmit : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;

			// data only contains the process parameters
			// we need to add the context
			data.strategy = this.get("strategy");
			if (data.lastAction) this.infobox.set("lastAction", data.lastAction);
			Y.io(paths.addprocess, {
				method: 'POST',
				data:data,
				on:{success:function(e,o) {
					var r =	Y.JSON.parse(o.responseText);
					// we first update the graph before anything is computed on the server
					oSelf.strategy_viz.updateGraph();
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
			data.strategy = this.get("strategy");

			Y.io(paths.updatenode, {
				data:data,
				method: 'POST',
				on:{success:function(e,o) {
					var r = Y.JSON.parse(o.responseText);
					oSelf.set("nodes", r.nodes);
					oSelf.set("selected", r.focus);
					if (!data.strategy == r.strategy) {
						// strategy changed name, we need to fully reload ...
						var l = window.location;
						var newURL = l.protocol + "//" + l.host + l.pathname +
							"?strategy=" + encodeURIComponent(r.strategy);
						window.location.replace(newURL);
					}
				}}
			})
		},

		_onNodeDelete : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				strategy = this.get("strategy");

			// inform the server and update the nodes
			Y.io(paths.deletenode, {
				method: 'POST',
				data:{
					strategy:strategy,
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
			this.updateNodeList(e.uri);
			// var selected = this.get("nodes")[e.uri];
			// this.set("selected", selected);
		},

		updateNodeList : function(nodeURI) {
			var oSelf = this;
			Y.io(this.get('paths').nodelist, {
				data: {
					strategy: this.get('strategy'),
					selected: nodeURI,
				},
				on:{success:function(e,o) {
					   var nodes = Y.JSON.parse(o.responseText);
					   oSelf.set("nodes", nodes);
					   oSelf.set("selected", nodes[nodeURI]);
			 	}}
			})
		},

		_onEvaluate : function(e) {
			var focus = e.data.focus;
			if(focus) {
				window.location =	this.get("paths").ag_evaluate
					+'?strategy='+encodeURIComponent(this.get("strategy"))
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
