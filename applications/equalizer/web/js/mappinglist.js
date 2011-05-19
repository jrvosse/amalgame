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
		},
		destructor : function() {},
		renderUI : function() {
			this.listNode = this.get("contentBox").appendChild(Node.create("<ul></ul>"));
		},
		bindUI : function() {	
			Y.delegate("click", this._onMappingSelect, this.listNode, "li", this)
			this.after("mappingsChange", this.syncUI, this);
			this.after("selectedChange", this._toggleSelection, this);
		},
		syncUI : function() {
			this._setMappings();
			this._toggleSelection();
		},
		
		_onMappingSelect : function(e) {
			var mappings = this.get("mappings"),
				target = e.currentTarget,
				index = target.get("parentNode").all("li").indexOf(target),
				mapping = mappings[index],
				data = {
					li:target, 
					uri:mapping.uri,
					label:mapping.label
				};	
			this.set("selected", mapping.uri);
			this.fire("mappingSelect", {data:data});
		},
		
		_setMappings : function() {
			var listNode = this.listNode,
				mappings = this.get("mappings");
	
			if(mappings) {
				for (var i=0; i < mappings.length; i++) {
					var m = mappings[i];
					listNode.append("<li><a href='javascript:void(0)'>"+m.label+"</a></li>");
				}
			}
		},
		
		_toggleSelection : function() {
			var sel = this.get("selected"),
				nodes = this.listNode.all("li"),
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
	
}, '0.0.1', { requires: ['node','event','widget']});
