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
			value: "200px"
		},
		maxNumberItems: {
			value: 100
		},
		minQueryLength: {
			value: 2
		},
		queryDelay: {
			value: 0.3
		}
	};

	/* ColumnBrowser extends the base Widget class */
	Y.extend(ColumnBrowser, Widget, {

		initializer: function(config) {
			this.publish("itemSelect", {});
			this.publish("optionSelect", {});
			this.publish("offsetSelect", {});
			this._nDelayID = -1;
		},

		destructor : function() {
			// purge itemSelect, optionSelect, offSetSelect, valueChange?
			// bodyResize
		},

		renderUI : function() {
			this._renderHeader();
			this._renderBody();
			this._renderFooter();
			
			this._setColumnDef(0);
			this._getColumnData(0);
		},

		bindUI : function() {
			Y.on("resize", this._updateBodySize, window, this);
			this.columnsBox.one('.yui3-resize-handle')
				.on( "mouseup" , this._updateColumnsSize, this);
		},

		syncUI : function() {
		},
		
		itemId : function(item) {
			var id = item.id ? item.id : item;
			return id;
		},
		itemLabel : function(item) {
			var label = item.label ? item.label : item;
			return label;
		},
		/**
		* Handles the selection of a resource list item.
		* Fires the itemSelect event
		* 
		* @private
		* @param listItem {Object} the list element node
		* @param resource {Object} the selected resource
		* @param index {Integer} the index of the column
		**/
		_itemSelect : function(listItem, resource, index) {
			var columns = this.get("columns"),
				next = index+1;
			this.setTitle(resource);
			this.fire("itemSelect", resource, index);
			if(columns[next]||columns[index].repeat) {			
				var column = this._setColumnDef(next, resource);
				this._getColumnData(next);
			}
		},
		
		/**
		* Handles the selection of a column option.
		* Fires the optionSelect event
		* 
		* @private
		* @param e {Object} the event object
		* @param index {Integer} the index of the column
		**/
		_optionSelect : function(e, index) {
			var column = this.get("columns")[index],
				optionValue = e.currentTarget.get("value");
			column.page = 0;
			column.option = optionValue;
			this._getColumnData(index);
			this.fire("optionSelect", optionValue, index);
		},
		
		/**
		* Handles the selection of a pagination action
		* Fires the offsetSelect event
		* 
		* @private
		* @param e {Object} the event object
		* @param index {Integer} the index of the column
		* @param direction {1 or -1} indicator for next (1) or prev (-1)
		**/		
		_offsetSelect : function(e, index, direction) {
			var column = this.get("columns")[index];							
			column.page += direction;
			this._getColumnData(index);
			this.fire("offsetSelect", index, direction);
		},
		
		/**
		* Creates the header with this.titleNode used to show active item
		* 
		* @private
		**/
		_renderHeader : function() {
			this.titleNode = this.get("contentBox")
				.appendChild(Node.create('<div class="hd"></div>'))
				.appendChild(Node.create('<div class="title"></div>'));
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
		* Creates a HTML select list with options provided in the 
		* configuration for columns[index].
		* An eventhandler is added to the HTML select element which is
		* handled by _optionSelect
		*
		* @private
		* @param index {Integer} the index of the column
		**/
		_renderOptionList : function(index) {
			var column = this.get("columns")[index];
			if(column.options) {
				var options = column.options;
				var optionsNode = column._node
						.appendChild(Node.create('<select class="options"></select>'));

				for (var i=0; i < options.length; i++) {
					var option = options[i],
						value = option.value,
						label = option.label ? option.label : value,	
						selected = option.selected ? 'selected' : '';
					optionsNode.insert('<option value="'+value+'" '+selected+'>'+label+'</option>');
				}
				optionsNode.on("change", this._optionSelect, this, index);
			}
		},
		
		/**
		* Creates pagination in a column.
		* An eventhandler is added to the prev and next buttons which is
		* handled by _offsetSelect
		* The pagination is stored in column._pagination, such that it is created
		* only once.
		* If pagination already exists we simply show it.
		*
		* @private
		* @param index {Integer} the index of the column
		* @param length {Integer} the number of resources
		**/
		_renderPagination : function(index, length) {
			var column = this.get("columns")[index],
				content = column.resourceList.get("contentBox"),
				limit = this.get("maxNumberItems"),
				start = column.page*limit,
				end = start+Math.min(limit,length);
				
			if(!column._pagination) {
				var pagination = content
					.appendChild(Node.create('<div class="pagination"></div>'));
				pagination.appendChild(Node.create('<a href="javascript:{}" class="page-prev">prev</a>'))
					.on("click", this._offsetSelect, this, index, -1);
				pagination.insert('<span class="page-label"></span>');
				pagination.appendChild(Node.create('<a href="javascript:{}" class="page-next">next</a>'))
					.on("click", this._offsetSelect, this, index, 1);
				column._pagination = pagination;		
			} else {
				column._pagination.removeClass("hidden");
			}
			
			// disable/enable buttons
			if(length<limit) { 
				Y.get(".page-next").addClass("disabled");
				Y.get(".page-prev").removeClass("disabled");
			} else if (start===0) { 
				Y.get(".page-prev").addClass("disabled", true); 
				Y.get(".page-next").removeClass("disabled");
			} else {
				Y.get(".page-next").removeClass("disabled");
				Y.get(".page-prev").removeClass("disabled");
			}
			// set page
			Y.get(".page-label").set("innerHTML", start+' -- '+end); 
		},
		
		/**
		* Fetches data for columns[index] by doing a
		* request on the datasource.
		*
		* @private
		* @param index {Integer} the index of the column
		**/
		_getColumnData : function(index) {
			var oSelf = this,
				column = this.get("columns")[index],
				params = column.params,
				request = column.request,
				offset = column.page ? column.page*this.get("maxNumberItems") : 0,
				cfg = {};
				
			// request configuration attribute consist of params in
			// the column definition and the current status of the column 	
			for(key in params) {
				if(key) {
					cfg[key] = params[key];
				}
			}
			cfg.limit = this.get("maxNumberItems");
			cfg.offset = offset;
			cfg.query = column.searchString || cfg.query ;
			cfg.type = column.option || cfg.type;
			cfg.parent = column.parent || cfg.parent;
			
			// request
			request = Lang.isFunction(request) 
				? request.call(this, cfg) 
				: request+"?"+this._requestParams(cfg);
				
			this._nDelayID = -1; // reset search query delay
			this._createColumn(index);
			this._clearColumns(index+1);
			this._setLoading(index, true);
			this.get("datasource").sendRequest({
				request:request,
				callback: {
					success: function(e){
						var resources = e.response.results;

						if(resources.length>0||column.options) { // add the results
							oSelf.activeIndex = index;
							oSelf._populateColumn(index, resources);
						} 
						else { 
							oSelf._clearColumn(column);
						}
						oSelf._setStatus(index, resources);
					},
					failure: function(e){
						alert("Could not retrieve data: " + e.error.message);
						oSelf._clearColumn(column);
					},
					scope: oSelf
				}
			});
		},
		
		_requestParams : function(cfg) {
			var params = "";
			for(var key in cfg) {
				if(cfg[key]) {
					params += key+"="+encodeURIComponent(cfg[key])+"&";
				}
			}
			return params;
		},
		
		/**
		* Updates the resourceList of columns[index] with new resources.
		* If the resourceList does not exist yet it is created first.
		*
		* @private
		* @param index {Integer} the index of the column
		* @param resources {Array} the index of the column				
		**/			
		_populateColumn : function(index, resources) {
			var column = this.get("columns")[index];
			column.resourceList.setResources(resources);
			
			// set pagination
			if(resources.length===this.get("maxNumberItems")||column.page>0) {
				this._renderPagination(index, resources.length);
			} else if(column._pagination) {
				column._pagination.addClass("hidden");
			}
			this._setLoading(index, false);
			this._updateContentSize();
			column._node.scrollIntoView();
		},

		/**
		* Creates a new column	based on Y.mazzle.ResourceList
		*
		* @private
		**/ 
		_createColumn : function(index, resources) {
			var column = this.get("columns")[index];
			if(!column.resourceList) {
				var oSelf = this,
					content = this.columnsNode,
					width = this.get("columnWidth");
				
				// create a new div in columnsNode and add resize plugin
				column._node = this.columnsNode.appendChild(
					Y.Node.create('<div class="column"></div>'))
					.plug(Y.Plugin.Resize, {handles:["r"],animate:true});
					
				this._renderOptionList(index);
				
				// search box	
				var category = Y.stamp(this)+"|";
				column._node.appendChild(Node.create('<div class="search"></div>'))
					.appendChild(Node.create('<input type="text"></div>'))
					.on(category+"valueChange", this._valueChangeHandler, this, index);	
				
				column._load = column._node.appendChild(
					Y.Node.create('<div class="hidden loading"></div>'));
					
				// hack to get a handler on the resize 
				// first make contentNode very big, and on mouse release set to actual size
				column._node.one('.yui3-resize-handle')
					.on( "mousedown" , function() {
						content.get("parentNode").addClass("noscroll");
						content.setStyle("width", "10000px")}, this);
				column._node.one('.yui3-resize-handle')
					.on( "mouseup" , this._updateContentSize, this);
	
				// create a new ResourceList
				var resourceList = new Y.mazzle.ResourceList({
					boundingBox: column._node,
					maxNumberItems: this.get("maxNumberItems"),
					resources: resources,
					width:width
				});
				resourceList.formatItem = column.formatter;
				resourceList.render();
				resourceList.on("itemClick", oSelf._itemSelect, oSelf, index);
				column.resourceList = resourceList;
			} else {
				column._node.removeClass("hidden");
			}
		},
	
		/**
		* Clears the content of all columns from index and above.
		*
		* @private
		**/		
		_clearColumns : function(index) {
			var columns = this.get("columns");
			for (var i=index; i < columns.length; i++) {
				this._clearColumn(columns[i]);
			}
		},
		_clearColumn : function(column) {			
			if(column._node) {
				column.resourceList.clearContent();
				if(column._pagination) {
					column._pagination.addClass("hidden");
				}
				if(!column.searchString) {
					column._node.addClass("hidden");
				}
			}
		},
		
		/**
		* Create or resets the column configuration.
		* When formatter and query are not specified the once
		* from the previous column are used
		*
		* @private
		**/ 
		// 
		_setColumnDef : function(index, parent) { 
			var columns = this.get("columns"),
				previous = columns[index-1]||{},
				column = columns[index] ? columns[index] : {};

			column.request = column.request||(previous.repeat ? previous.request : null);	
			column.formatter = column.formatter||previous.resourceList.formatItem;
			column.parent = parent ? this.itemId(parent) : null;
			column.params = column.params||(previous.repeat ? previous.params : null);
			column.options = column.options||(previous.repeat ? previous.options : null);
			column.repeat = column.repeat||previous.repeat;
 			column.page = 0;
			column.searchString = null;
			
			columns[index] = column;
			return column;
		},
	
		/**
		* Sets the title in the header
		*
		* @public
		**/ 
		setTitle : function(resource) {
			var HTML = "";
			if(resource) {
				HTML = '<h3>'+this.itemLabel(resource)+'</h3>'
			}
			this.titleNode.set("innerHTML", HTML);
		},
	
		/**
		* Sets the status in the footer
		*
		* @private
		**/			
		_setStatus : function(index, resources) {
			var columns = this.get("columns"),
				length = resources.length,
				HTML = "";
			
			if(length>0) {
				var column = columns[index],
					type = column.type || "item";
				
				type += (length>0) ? "s" : "";
				length += (length==this.get("maxNumberItems")) ? "+" : "";
				HTML = '<div>'+length+' '+type+'</div>';
			}
			else if(columns[index-1]) {
				var rl = columns[index-1].resourceList,
					selected = rl.get("selected").length,
					total = rl.get("resources").length;
					
				HTML = '<div>'+selected+' of '+total+' selected</div>';
			}
			this.statusNode.set("innerHTML", HTML);
		},
		
		_setLoading: function(index, status) {
			var column = this.get("columns")[index];
			if(status) {
				column._load.removeClass("hidden");
				column._node.one(".yui3-resourcelist-content").addClass("hidden");
			} else {
				column._node.one(".yui3-resourcelist-content").removeClass("hidden");
				column._load.addClass("hidden");
			}
		},	
		/**
		 * The handler that listens to valueChange events and decides whether or not
		 * to kick off a new query.
		 *
		 * @param {Object} The event object
		 * @private
		 **/
		_valueChangeHandler : function(e, index) {
			var oSelf = this,
				query = e.value;
			console.log(index);	
			// Clear previous timeout to prevent old searches to push through
		    if(oSelf._nDelayID != -1) {
		        clearTimeout(oSelf._nDelayID);
		    }
			this._columnSearch(index, query);
		},	
		_columnSearch : function(index, query) {		
			var column = this.get("columns")[index];
			column.searchString = query;
			column.page = 0;
			if (!query || query.length < this.get("minQueryLength")) {
				this._getColumnData(index);
			}
			else {
	    		// Set a timeout to prevent too many search requests
				var oSelf = this;
	    		oSelf._nDelayID = setTimeout(function(){
	            	oSelf._getColumnData(index);
	        	}, this.get("queryDelay")*1000);
			}
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
				var columnNode = columns[i]._node;
				if(columnNode&&(!columnNode.hasClass("hidden"))) {
					width += columnNode.get("offsetWidth");
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
				if(columns[i]._node) {
					columns[i]._node.setStyle("height", height+"px");
				}
			}
		}
		
	}); 

}, 'gallery-2010.03.02-18' ,{requires:['node','event','widget','resourcelist','value-change']});