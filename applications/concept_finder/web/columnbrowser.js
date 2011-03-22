YUI.add('columnbrowser', function(Y) {

	var Lang = Y.Lang,
		Widget = Y.Widget,
		Node = Y.Node;

	Widget.ColumnBrowser = ColumnBrowser;
	var NS = Y.namespace('mazzle'); 
	NS.ColumnBrowser = ColumnBrowser;
	
	/* ColumnBrowser class constructor */
	function ColumnBrowser(config) {
		ColumnBrowser.superclass.constructor.apply(this, arguments);
	}

	/* 
	 * Required NAME static field, to identify the Widget class and 
	 * used as an event prefix, to generate class names etc. (set to the 
	 * class name in camel case). 
	 */
	ColumnBrowser.NAME = "columnbrowser";

	/*
	 * The attribute configuration for the ColumnBrowser widget. Attributes can be
	 * defined with default values, get/set functions and validator functions
	 * as with any other class extending Base.
	 */
	ColumnBrowser.ATTRS = {
		datasource: {
			value: null
		},
		columns: {
			value: null
		},
		columnWidth: {
			value: 200,
			validator: Lang.isNumber
		},
		maxNumberItems: {
			value: 100,
			validator: Lang.isNumber
		},
		minQueryLength: {
			value: 2,
			validator: Lang.isNumber
		},
		queryDelay: {
			value: 0.3,
			validator: Lang.isNumber
		},
		title: {
			value: ""
		}
	};

	/* ColumnBrowser extends the base Widget class */
	Y.extend(ColumnBrowser, Widget, {

		initializer: function(config) {
			// internal variables
			this._activeIndex = null;
			this._selectedIndex = null;
			
			// internal references to nodes
			this.titleNode = null;
			this.statusNode = null;
			this.columnsBox = null;
			this.columnsNode = null;
			
			this.publish("itemSelect", {});
		},

		destructor : function() {
			// purge itemSelect, optionSelect, offSetSelect, valueChange?
			// bodyResize
		},

		renderUI : function() {
			this._renderHeader();
			this._renderBody();
			this._renderFooter();
		},

		bindUI : function() {
			Y.on("resize", this._updateBodySize, window, this);
			this.columnsBox.dd
				.on( "drag:end" , this._updateColumnsSize, this);
		},

		syncUI : function() {
			if(this._activeIndex===0||this._activeIndex) {
				var columns = this.get("columns"),
					activeIndex = this._activeIndex,
					selectedIndex = this._selectedIndex,
					selectedItem = this._selectedItem;
			
				// update the status of the columns
				// to "selected" or "hidden"
				for (var i=0; i < columns.length; i++) {
					var list = columns[i].list;
					if(list) {
						if(i==selectedIndex) {
							list.get("boundingBox").addClass("selected");
						} else {
							list.get("boundingBox").removeClass("selected");
						}
						if(i<=activeIndex) {
							list.set("visible", true);
						} else {
							list.set("visible", false);
						}
					}
				}
			
				// update the title and footer
				this.setTitle(selectedItem, activeIndex);
				this.setFooter(selectedItem, activeIndex);

				// update the active column
				this._updateContentSize();
				//columns[activeIndex].list._node.scrollIntoView();
			} else {
				this._activeIndex = 0;
				this._updateColumn(0);
			}
		},
		
		/** 
		* Public functions to fetch ids and labels from result items
		*/
		itemId : function(item) {
			var id = item.id ? item.id : item;
			return id;
		},
		itemLabel : function(item) {
			var label = item.label ? item.label : item;
			return label;
		},
		/**
		* Public funtions to set the title and footer
		**/ 
		setTitle : function(selected, active) {
			var HTML = selected ? '<h3>'+this.itemLabel(selected)+'</h3>' : '';
			this.titleNode.set("innerHTML", HTML);
		},
		setFooter : function(selected, active) {
			var column = this.get("columns")[active],
				label = column.label || "item",
				length = column.totalNumberOfResults;
			var HTML = "";
			if(length>0) {
				label += (length>1) ? "s" : "";
				HTML = '<div>'+length+' '+label+'</div>';
			}
			else {
				HTML = '';
			}
			this.statusNode.set("innerHTML", HTML);
		},
		
		/**
		* Handles the selection of a resource list item.
		* Fires the itemSelect event
		* 
		* @private
		* @param listItem {Object} the list element node
		* @param resource {Object} the selected resource
		**/
		_itemSelect : function(listItem, oItem, index) {
			var columns = this.get("columns"),
				next = index+1;
			
			this._selectedItem = oItem;
			this._selectedIndex = index;
			
			if(columns[next]||columns[index].repeat) {
				if(oItem.hasNext) {	
					this._updateColumn(next, this.itemId(oItem));
				} else {
					this.syncUI();
				}
			}
			
			this.fire("itemSelect", oItem, index, listItem);
		},
		_setActiveColumn : function(e, index) {
			this._selectedIndex = index>=1 ? index-1 : null;
			this._activeIndex = index;
			this.syncUI();
		},
		
		/**
		* Creates the header with this.titleNode used to show active item
		* 
		* @private
		**/
		_renderHeader : function() {
			var title = this.get("title")||"";
			this.titleNode = this.get("contentBox")
				.appendChild(Node.create('<div class="hd"></div>'))
				.appendChild(Node.create('<div class="title">'+title+'</div>'));
		},
		
		/**
		* Creates the body with this.columnsNode that will contain
		* the individual columns.
		* A resize plugin is added to the columnsNode to allow 
		* changing the height
		*
		* @private
		**/
		_renderBody : function() {
			this.columnsBox = this.get("contentBox")
				.appendChild(Node.create('<div class="bd"></div>'))
				.appendChild(Node.create('<div class="columns-box"></div>'))
				.plug(Y.Plugin.Resize, {handles:["b"],animate:true});
			this.columnsNode = this.columnsBox	
				.appendChild(Node.create('<div class="columns"></div>'));
		},
		
		/**
		* Creates the footer with this.statusNode use for status info
		* 
		* @private
		**/
		_renderFooter : function() {
			this.statusNode = this.get("contentBox")
				.appendChild(Node.create('<div class="ft"></div>'))
				.appendChild(Node.create('<div class="status"></div>'));
		},
			
		/**
		* Creates a new column	based on Y.mazzle.ResourceList
		*
		* @private
		**/ 
		_updateColumn : function(index, parent) {	
			if(!this.get("columns")[index]) {
				this.get("columns")[index] = {};
			}

			var column = this.get("columns")[index];
 			if(!column.list) {
				column.list = this._createColumnList(index);
			}
			if(parent) {
				column.list.set("params.parent", parent);
			}
			column.list.updateContent();
		},
		
		_createColumnList : function(index) {
			var columns = this.get("columns"),
				previous = columns[index-1]||{},
				column = columns[index];
	
			var cfg = {
				width: this.get("columnWidth"),
				maxNumberItems: this.get("maxNumberItems"),
				minQueryLength: this.get("minQueryLength"),
				queryDelay: this.get("queryDelay")
			};

			column.repeat = column.repeat||previous.repeat;
			column.label = column.label || (column.repeat&&previous.label);
				
			// column properties are defined or inherited from previous column			
			cfg.datasource = this.get("datasource");
			cfg.request = column.request||(column.repeat&&previous.request);	
			cfg.params = column.params||(column.repeat&&previous.params);
			cfg.options = column.options||(column.repeat&&previous.options);
			
			var list = new Y.mazzle.ResourceList(cfg);
			list.render(this.columnsNode);
			list.get("boundingBox")
				.plug(Y.Plugin.Resize, {handles:["r"],animate:true})
			
			list.formatItem = column.formatter||previous.list.formatItem;
			list.on("itemClick", this._itemSelect, this, index);
			list.on("beforeContentUpdate", this._setActiveColumn, this, index);

			var dd = list.get("boundingBox").dd;
			// first make contentNode very big
			dd.on( "drag:start", function() {
				this.get("parentNode").addClass("noscroll");
				this.setStyle("width", "10000px");
			}, this.columnsNode);
			// at the end of resize put it to the actual size
			dd.on( "drag:end", this._updateContentSize, this);
			
			return list;
		},
					
		/**
		 * Handles resizing of the window by updating
		 * the fixed with of the bodyNode.
		 * This is required because the resize.plugin adds a fixed with.
		 **/
		_updateBodySize : function() {
			var width = this.get("boundingBox").get("offsetWidth");
			this.columnsBox.setStyle("width", width+"px");
		},
				
		/**
		 * Handles resizing column content by
		 * setting the size of this.colomnsNode to the width of the combined columns
		 **/
		_updateContentSize : function() {
			var columns = this.get("columns"),
				content = this.columnsNode,
				width = 0;
			
			for (var i=0; i < columns.length; i++) {
				var columnList = columns[i].list;
				if(columnList&&columnList.get("visible")) {
					width += columnList.get("boundingBox").get("offsetWidth");
				}
			}
			
			content.setStyle("width", width+"px");
			content.get("parentNode").removeClass("noscroll");
			this._updateColumnsSize();
		},
		
		_updateColumnsSize : function() {
			var columns = this.get("columns"),
				height = this.columnsNode.get("offsetHeight");
			for (var i=0; i < columns.length; i++) {
				if(columns[i].list) {
					columns[i].list.set("height", height+"px");
				}
			}
		}
		
	}); 

}, 'gallery-2010.03.02-18' ,{requires:['node','event','widget','resourcelist','value-change']});