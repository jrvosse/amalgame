YUI.add('mappinglist', function(Y) {
	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;

	function MappingList(config) {
		MappingList.superclass.constructor.apply(this, arguments);
	}
	MappingList.NAME = "mappinglist";
	MappingList.ATTRS = {
		mappings: { value:{} },
		selected: { value: null }
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
			var uri = e.currentTarget.get("title");
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
			var isReference = this._toggleSelection();
			this.fire("mappingSelect", {uri:uri, isReference:isReference});
		},

		_setMappings : function() {
			var listNode = this.listNode;
			listNode.append("<tr>"+
				"<th>Mapping</a></th>"+
				"<th colspan='1' class='src_mapped'># sources</th>"+
				"<th colspan='1' class='target_mapped'># targets</th>"+
				"<th class='nr_of_mappings'># mappings</th>"+
				"</tr>");

			var mappings = this.get("mappings");
			for (var uri in mappings) {
				var m = mappings[uri];
				if (!m.stats.totalCount > 0) continue;
				listNode.append("<tr class='row "+m.agStatus+"' "+
			        "title='"+uri+"'>"+
				"<td><a href='javascript:void(0)'>"+m.label+"</a></td>"+
				"<td class='src_mapped'>"+m.stats.mappedSourceConcepts+"</td>"+
				"<td class='target_mapped'>"+m.stats.mappedTargetConcepts+"</td>"+
				"<td class='nr_of_mappings'>"+m.stats.totalCount+"</td>"+
				"</tr>");
			}
		},

		_toggleSelection : function() {
			var isReference = false;
			var nodes = this.listNode.all("tr.row");
			nodes.removeClass("selected");
			var sel = this.get("selected");
			if(sel) { 
				var selRow = this.listNode.one('tr[title='+sel+']')
				if (selRow) {
					selRow.addClass("selected");
					isReference = selRow.hasClass("reference");
				}
			}
			return isReference;
		},
	});

	Y.MappingList = MappingList;

}, '0.0.1', { requires: ['node','event','widget','history','querystring']});
