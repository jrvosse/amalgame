YUI.add('resourcelist', function(Y) {

	var Lang = Y.Lang,
		Widget = Y.Widget,
		Node = Y.Node;

	Widget.ResourceList = ResourceList;
	var NS = Y.namespace('mazzle'); 
	NS.ResourceList = ResourceList;
	
	/* ResourceList class constructor */
	function ResourceList(config) {
		ResourceList.superclass.constructor.apply(this, arguments);
	}

	/* 
	 * Required NAME static field, to identify the Widget class and 
	 * used as an event prefix, to generate class names etc. (set to the 
	 * class name in camel case). 
	 */
	ResourceList.NAME = "resourcelist";

	/*
	 * The attribute configuration for the ResourceList widget. Attributes can be
	 * defined with default values, get/set functions and validator functions
	 * as with any other class extending Base.
	 */
	ResourceList.ATTRS = {
		maxNumberItems: {
			value: 100,
			validator : Lang.isNumber
		},
		resources: {
			value: [],
			validator : Lang.isArray
		},
		selected: {
			value: [],
			validator : Lang.isArray
		},
		formatter: {
			setter : function (f) {
				return ( Lang.isFunction(f) ? f
						 : function (resource) {
								return '<div class="resourcelist-value">'+resource+'</div>';
						 });
			}
		}
	};

	/* Static constants used to define the markup templates used to create ResourceList DOM elements */
	ResourceList.LIST_CLASS = ResourceList.NAME+'-list';
	ResourceList.ITEM_CLASS = ResourceList.NAME+'-item';
	ResourceList.LIST_TEMPLATE = '<ul class="'+ResourceList.LIST_CLASS+'"></ul>';
	ResourceList.ITEM_TEMPLATE = '<li class="'+ResourceList.ITEM_CLASS+'"></li>';

	/* ResourceList extends the base Widget class */
	Y.extend(ResourceList, Widget, {

		initializer: function(config) {
			this.publish("itemClick", {});
		},

		destructor : function() {
		},

		renderUI : function() {
			this._renderContent();
		},

		bindUI : function() {
			Y.delegate("click", this.itemSelect, this._listNode, "li", this);
		},

		syncUI : function() {
			if(this.get("resources")) {
				this.setResources(this.get("resources"));
				this.setSelection(this.get("selected"));
			}
		},

		_renderContent : function() {
			var content = this.get("contentBox"),
				nListLength = this.get("maxNumberItems");
			
			// add ul
			var list = content.one("."+ResourceList.LIST_CLASS);
			if(!list) {
				list = Node.create(ResourceList.LIST_TEMPLATE);
				content.appendChild(list);
			}
			
			// add list items
			var listItems = [];
			for (var i=0; i < nListLength; i++) {
				var listItem = list.appendChild(Node.create(ResourceList.ITEM_TEMPLATE));
				listItem.setStyle("display", "block");
				listItem._node._nItemIndex = i;
				listItems.push({el:listItem});
			}	
			this._listNode = list;
			this._listItems = listItems;
		},
		
		setResources : function(resources) {
			var listItems = this._listItems,
				formatter = this.get("formatter"),
				numberItems = Math.min(this.get("maxNumberItems"), resources.length);

			this.clearSelection();
			this.set("resources", resources);
			// add resources
			var i = 0;	
			for (i; i < numberItems; i++) {
				var oResource = resources[i],
					HTML = this.get("formatter")(oResource),
					oItem = listItems[i],
					elItem = oItem.el;

				oItem.resource = oResource;
				elItem.set("innerHTML", HTML);
				elItem.setStyle("display", "block");
			}
			
			// clear remaining elements
			for (i; i < listItems.length; i++) {
				var oItem = listItems[i],
					elItem = oItem.el;
				if(elItem.getStyle("display")=="none") {
					return;
				} else {
					elItem.innerHTML = [];
					elItem.setStyle("display", "none");
					oItem.resource = null;
				}	
			}
		},
		
		clearContent : function() {
			this.setResources([]);
		},
		
		setSelection : function() {
			var selected = this.get("selected");
			for (var i=0; i < selected.length; i++) {
				selected[i].addClass("selected");
			}
		},
		clearSelection : function() {
			var selected = this.get("selected");
			for (var i=0; i < selected.length; i++) {
				selected[i].removeClass("selected");
			}
			this.set("selected", []);
		},
		
		itemSelect : function(e) {
			var listItems = this._listItems,
				itemNode = e.currentTarget,
				oResource = listItems[itemNode.get("_nItemIndex")].resource;
			
			// @tbd add support for multiple selections
			this.clearSelection();
			this.set("selected", [itemNode]);
			this.setSelection();
			
			itemNode.addClass("selected");
			this.fire("itemClick", itemNode, oResource);
		}
		
	});
	  
}, 'gallery-2010.03.02-18' ,{requires:['node','event','widget']});