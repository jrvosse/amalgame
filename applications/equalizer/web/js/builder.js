YUI.add('builder', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_OPM			= Y.one("#opm"),
		NODE_CONTROLS		= Y.one("#controls"),
		NODE_INFO			= Y.one("#info"),
		NODE_SELECT		= Y.one("#select");

	function Builder(config) {
		Builder.superclass.constructor.apply(this, arguments);
	}
	Builder.NAME = "builder";
	Builder.ATTRS = {
		alignment : {
			value: null
		},
		selected : {
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
		nodes:{
			value:{},
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

	Y.extend(Builder, Y.Base, {

		initializer: function(args) {
			this.readonly = (this.get('readonly') != "false"); // string/boolean

			// initalize the different modules
			this._initGraph();
			this._initControls();
			this._initInfo();

			if (!this.readonly) {
				this.controls.on("submit", this._onControlSubmit, this);
			} else {
			  Y.one('#hint').setContent('Please login to make changes');
			  Y.all('button').each(function(button) { button.setAttribute("disabled", true); });
                        };

			this.opmviz.on("nodeSelect", this._onNodeSelect, this);
			this.infobox.on("nodeSelect", this._onNodeSelect, this);
			this.infobox.after("nodeChange", this._onNodeChange, this);
			this.infobox.after("deleteNode", this._onNodeDelete, this);
			this.controls.on("nodeChange", this._onNodeChange, this);

			this.after('nodesChange', function(o) {
				this.controls.set("nodes", o.newVal);
			}, this);

			this._fetchGraph();
		},

		_initGraph : function() {
			var DS = new Y.DataSource.IO({
				source: this.get("paths").nodeinfo
			});
			this.opmviz = new Y.OPMViz({
				datasource: DS,
				mappings: this.get("nodes"),
				alignment: this.get("alignment")
			}).render(NODE_OPM);
		},

		_initInfo : function() {
			// The infobox is part of the controls,
			// but has some additional routines
			var DS = new Y.DataSource.IO({
				source: this.get("paths").info
			});
			//.plug({fn:Y.Plugin.DataSourceCache, cfg:{max:10}});
			this.infobox = new Y.InfoBox({
				srcNode: NODE_INFO,
				alignment: this.get("alignment"),
				mappings: this.get("nodes"),
				hint: this.get("paths").hint,
				controls: this.controls,
				readonly: this.readonly,
				datasource: DS
			});

			// Let's get started by selecting the strategy node
			var strategy = this.get("nodes")[this.get("alignment")];
			strategy.uri = this.get("alignment");
			this.infobox.set("selected", strategy);
		},

		_initControls : function() {
			this.controls = new Y.Controls({
				srcNode: NODE_CONTROLS,
				selected: this.get("selected"),
				nodes: this.get("nodes")
			});
		},

		_fetchGraph : function(conf) {
			var alignment = this.get("alignment"),
				paths = this.get("paths"),
				opmviz = this.opmviz;

			if(alignment) {
				conf = conf ? conf : {};
				conf.graph = alignment;

				Y.io(paths.opmgraph, {
					data:conf,
					on:{success: function(e,o) {
						// As the server returns an XML document, including doctype
						// we first take out the actual svg element
						var SVG = o.responseXML.lastChild;
						opmviz.setGraph(SVG);
						}
					}
				})
			}
		},

		_onControlSubmit : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;

			// data only contains the process parameters
			// we need to add the context
			data.alignment = this.get("alignment");
			Y.io(paths.addprocess, {
				data:data,
				on:{success:function(e,o) {
					var r =	Y.JSON.parse(o.responseText);
					oSelf.set("nodes", r.nodes);
					oSelf.opmviz.fire("nodeSelect", {uri:r.focus});
					oSelf._fetchGraph();
				}}
			})
		},

		_onNodeChange : function(o) {
			Y.log("nodeChange event caught (builder:onNodeChange)");
			var oSelf = this,
				paths = this.get("paths"),
				data = o.data;
			data.alignment = this.get("alignment");
			Y.io(paths.updatenode, {
				data:data,
				on:{success:function(e,o) {
					var response = Y.JSON.parse(o.responseText);
					oSelf.set("nodes", response.nodes);
					if (data.alignment == response.alignment) {
						oSelf._fetchGraph();
						Y.log("fire nodeSelect after update");
						Y.log(response);
						oSelf.opmviz.fire("nodeSelect", {uri:response.focus});
					} else { // alignment changed name, we need to fully reload ...
						oSelf.set("alignment", response.alignment);
						var l = window.location;
						var newURL = l.protocol + "//" + l.host + l.pathname +
							"?alignment=" + encodeURIComponent(response.alignment);
						window.location.replace(newURL);
					}
				}}
			})
		},

		_onNodeDelete : function(o) {
			var oSelf = this,
				paths = this.get("paths"),
				data = {
					alignment:this.get("alignment"),
					uri:o.uri
				};

			Y.io(paths.deletenode, {
				data:data,
				on:{success:function(e,o) {
					oSelf.set("nodes", Y.JSON.parse(o.responseText).nodes);
					oSelf._fetchGraph();
				}}
			})
		},

		_onNodeSelect : function(e) {
				  Y.log(e);
			var uri = e.uri;
			var node = this.get("nodes")[uri];
			node.uri = uri;
			this.set("selected", uri);
			// update the controls and the info
			this.controls.set("selected", node);
			this.infobox.set("selected", node);
			this.opmviz.set("active", uri);
		}
	});

	Y.Builder = Builder;

}, '0.0.1', { requires: [
	'node','event','anim','tabview',
	'datasource-io','datasource-cache',
	'querystring-stringify-simple',
	'gallery-node-accordion'
	]
});
