YUI.add('mappinglist', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;

	function MappingList(config) {
		MappingList.superclass.constructor.apply(this, arguments);
	}
	MappingList.NAME = "mappinglist";
	MappingList.ATTRS = {
		mappings: {
			value:[]
		},
		selected: {
			value: null
		}
	};

	Y.extend(MappingList, Y.Widget, {
		initializer: function(config) {
			this._history = new Y.History({
				initialState: {
					selected:this.get("selected")
				}
			});
		},
		destructor : function() {},
		renderUI : function() {
			this.listNode = this.get("contentBox").appendChild(Node.create("<table class='mappinglist'></table>"));
		},
		bindUI : function() {
			this._history.on('selectedChange', this._onSelectChange, this);
			Y.delegate("click", this._onMappingSelect, this.listNode, "tr.row", this)
			this.after("mappingsChange", this.syncUI, this);
		},
		syncUI : function() {
			this._setMappings();
			this._toggleSelection();
		},

		// on selection of a mapping
		// - we only update the selected value in the history manager
		// - we also update the url of the page
		_onMappingSelect : function(e) {
			var index = this.listNode.all("tr.row").indexOf(e.currentTarget);
			var uri = this.get("mappings")[index].uri;
			var params = Y.QueryString.parse(window.location.search.substr(1));
			params.focus = uri;
			this._history.addValue("selected", uri, {
				url: window.location.pathname+'?'+Y.QueryString.stringify(params)
			});
		},

		_onSelectChange : function(e) {
			var uri = e.newVal;

			Y.log('mappinglist :: select '+uri);
			this.set("selected", uri);
			this._toggleSelection();
			this.fire("mappingSelect", {uri:uri});
		},

		_setMappings : function() {
			var listNode = this.listNode,
				mappings = this.get("mappings");
			listNode.append("<tr>"+
				"<th>Mapping</a></th>"+
				"<th colspan='2' class='src_mapped'># sources</th>"+
				"<th colspan='2' class='target_mapped'># trgs</th>"+
				"<th class='nr_of_mappings'># mappings</th>"+
				"</tr>");

			if(mappings) {
				for (var i=0; i < mappings.length; i++) {
					var m = mappings[i];
					listNode.append("<tr class='row'>"+
					"<td><a href='javascript:void(0)'>"+m.label+"</a></td>"+
					"<td class='src_mapped'>"+m.stats.numberOfSourceConcepts+"</td>"+
					"<td class='p_src_mapped'>("+m.stats.pSources+"%)</td>"+
					"<td class='target_mapped'>"+m.stats.numberOfTargetConcepts+"</td>"+
					"<td class='p_target_map>ed'>("+m.stats.pTargets+"%)</td>"+
					"<td class='nr_of_mappings'>"+m.stats.numberOfMappings+"</td>"+
					"</tr>");
				}
			}
		},

		_toggleSelection : function() {
			var sel = this.get("selected"),
				nodes = this.listNode.all("tr.row"),
				index = this._mappingIndexOf(sel);
			nodes.removeClass("selected");
			if(index >= 0) {
				nodes.item(index).addClass("selected");
			}
		},

		_mappingIndexOf : function(uri) {
			mappings = this.get("mappings");
			for (var i = mappings.length - 1; i >= 0; i--){
				if(mappings[i].uri == uri) {
					return i;
				}
			}
		}

	});

	Y.MappingList = MappingList;

}, '0.0.1', { requires: ['node','event','widget','history','querystring']});
