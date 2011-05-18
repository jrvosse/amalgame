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
		
	};
	
	Y.extend(MappingList, Y.Widget, {
		initializer: function(config) {
		},
		destructor : function() {},
		renderUI : function() {
			var content = this.get("contentBox");
			this.listNode = content.appendChild(Node.create("<ul></ul>"));
		},
		
		bindUI : function() {
			var mappings = this.get("mappings"),
				listNode = this.listNode;
				
			Y.delegate("click", function(e) {
				var target = e.currentTarget,
					index = target.get("parentNode").all("li").indexOf(target),
					mapping = mappings[index],
					arg = {
						li:e.currentTarget, 
						uri:mapping.uri,
						label:mapping.label
					};
				this.set("selected", mapping);
				listNode.all("li").removeClass("selected");
				e.currentTarget.addClass("selected");
				this.fire("mappingSelect", arg);
				
			}, listNode, "li", this)
		},
		
		syncUI : function() {
			var mappings = this.get("mappings"),
				listNode = this.listNode;
				
			if(mappings) {
				for (var i=0; i < mappings.length; i++) {
					var m = mappings[i];
					var l = listNode.appendChild(
						Node.create("<li><a href='javascript:void(0)'>"+m.label+"</a></li>"));
				}
			}
		}
		
	});
	
	Y.MappingList = MappingList;
	
}, '0.0.1', { requires: ['node','event','widget']});
