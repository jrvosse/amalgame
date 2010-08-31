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
			//Y.on("resize", this._updateBodySize, window, this);
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
			var column = this._setColumnDef(index+1, resource);
			this._getColumnData(index+1);
			this._setTitle(this.itemLabel(resource));
			this.fire("itemSelect", resource, index);
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
		* and a controls bar with a search box.
		* The search box is bound to an valueChangeHandler
		* to perform autocompletion search.	 
		* 
		* @private
		**/
		_renderHeader : function() {
			var oSelf = this,
				title = Node.create('<h3 class="title"></h3>');
				//search = Node.create('<input type="text">');
				
			this.get("contentBox").
				append(Node.create('<div class="hd"></div>').
					append(title)
					/*append(Node.create('<div class="controls"></div>').
						append(Node.create('<div class="search"></div>').
							append(search).
							append('<div class="label">Search</div>')
						)
					)*/
				);
			
			//var category = Y.stamp(this)+"|";
			//Y.on(category+"valueChange", this._valueChangeHandler, search, this);
			this.titleNode = title;
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
			this.bodyNode = Node.create('<div class="bd"></div>');
			this.columnsNode = Node.create('<div class="columns"></div>');
			
			this.get("contentBox").appendChild(this.bodyNode).
				appendChild(Node.create('<div class="columns-box"></div>')).
					appendChild(this.columnsNode);
			
			//this.bodyNode.plug(Y.Plugin.Resize, {handles:["b"],animate:true});
		},
		
		/**
		* Creates the footer with this.statusNode use for status info
		* 
		* @private
		**/
		_renderFooter : function() {
			this.statusNode = Node.create('<div class="status"></div>');
			this.get("contentBox").
				append(Node.create('<div class="ft"></div>').
					append(this.statusNode)
				);
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
				var options = column.options,
					optionsNode = Node.create('<select class="options"></select>');
				
				column.resourceList.get("contentBox").prepend(optionsNode);
				for (var i=0; i < options.length; i++) {
					var option = options[i],
						value = option.value,
						label = option.label ? option.label : value;		
					optionsNode.insert('<option value="'+value+'">'+label+'</option>');
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
				var pagination = content.appendChild(Node.create('<div class="pagination"></div>'));
				pagination.appendChild(
					Node.create('<a href="javascript:{}" class="page-prev">prev</a>')).on(
						"click", this._offsetSelect, this, index, -1);
				pagination.insert('<span class="page-label"></span>');
				pagination.appendChild(
					Node.create('<a href="javascript:{}" class="page-next">next</a>')).on(
						"click", this._offsetSelect, this, index, 1);
				column._pagination = pagination;		
			} else {
				column._pagination.setStyle("display", "block");
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
				request = column.request,
				offset = column.page ? column.page*this.get("maxNumberItems") : 0,
				cfg = {
					type: column.option,
					parent: column.parent,
					limit: this.get("maxNumberItems"),
					offset: offset,
					query: column.searchString
				};
			request = Lang.isFunction(request) ? request.call(this, cfg) : request+"?"+this._requestParams(cfg);
			this._nDelayID = -1; // reset search query delay
			this.get("datasource").sendRequest({
				request:request,
				callback: {
					success: function(e){
						var resources = e.response.results;

						if(resources.length>0||column.options) { // add the results
							oSelf.activeIndex = index;
							oSelf._clearColumns(index+1);
							oSelf._populateColumn(index, resources);
						} 
						else { // hide all columns and set activeIndex to previous column
							oSelf.activeIndex = index-1;
							oSelf._clearColumns(index);
						}
						oSelf._setStatus(index, resources);
					},
					failure: function(e){
						alert("Could not retrieve data: " + e.error.message);
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
			var columns = this.get("columns"),
				column = columns[index];
				
			if(column.resourceList) { // we already have a column
				column.resourceList.setResources(resources);
			} 
			else { // create a new column				
				this._createColumn(index, resources);
			}
			
			// set pagination
			if(resources.length===this.get("maxNumberItems")||column.page>0) {
				this._renderPagination(index, resources.length);
			} else if(column._pagination) {
				column._pagination.setStyle("display", "none");
			}
			// show it
			column._node.setStyle("display", "block");
			this._updateContentSize();
		},

		/**
		* Creates a new column	based on Y.mazzle.ResourceList
		*
		* @private
		**/ 
		_createColumn : function(index, resources) {
			var oSelf = this,
				column = this.get("columns")[index],
				width = this.get("columnWidth");
			
			// create a new div in columnsNode and add resize plugin
			column._node = this.columnsNode.appendChild(Y.Node.create('<div></div>'));
			column._node.plug(Y.Plugin.Resize, {handles:["r"],animate:true});
			// hack to get a handler on the resize
			column._node.one('.yui3-resize-handle').
				on( "mouseup" , oSelf._updateContentSize, oSelf);
	
			// create a new ResourceList
			var resourceList = new Y.mazzle.ResourceList({
				boundingBox: column._node,
				maxNumberItems: this.get("maxNumberItems"),
				resources: resources,
				width:width,
				formatter:column.formatter
			});
			resourceList.render();
			resourceList.on("itemClick", oSelf._itemSelect, oSelf, index);
			column.resourceList = resourceList;
			this._renderOptionList(index);
		},
	
		/**
		* Clears the content of all columns from index and above.
		*
		* @private
		**/		
		_clearColumns : function(index) {
			var columns = this.get("columns");
			for (var i=index; i < columns.length; i++) {
				if(columns[i].resourceList) {
					var column = columns[i];
					column.resourceList.clearContent();
					column._node.setStyle("display", "none");
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
				previous = columns[index-1],
				column = columns[index] ? columns[index] : {};

			column.request = column.request||previous.request;	
			column.formatter = column.formatter||previous.resourceList.get("formatter");
			column.parent = parent ? this.itemId(parent) : null;
			column.page = 0;
			column.searchString = null;
			
			columns[index] = column;
			return column;
		},
	
		/**
		* Sets the title in the header
		*
		* @private
		**/ 
		_setTitle : function(HTML) {
			if(HTML) {
				this.titleNode.set("innerHTML", HTML);
			} else {
				this.titleNode.set("innerHTML", "");
			}
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
		
		/**
		 * The handler that listens to valueChange events and decides whether or not
		 * to kick off a new query.
		 *
		 * @param {Object} The event object
		 * @private
		 **/
		_valueChangeHandler : function(e) {
			var oSelf = this,
				value = e.value,
				index = this.activeIndex,
				column = this.get("columns")[index],
				delay = this.get("queryDelay");

			// Clear previous timeout
		    if(oSelf._nDelayID != -1) {
		        clearTimeout(oSelf._nDelayID);
		    }
			column.searchString = value;
			
			if (!value) {
				this._getColumnData(index);
			}
			else if (value === this._cachedValue || value.length < this.get("minQueryLength")) {
				return;
			} else {
				this._cachedValue = value;
				column.page = 0;

	    		// Set new timeout
	    		oSelf._nDelayID = setTimeout(function(){
	            	oSelf._getColumnData(index);
	        	}, delay*1000);
			}

		},
		
		/**
		 * Handles resizing of the window by updating
		 * the fixed with of the bodyNode.
		 * This is required because the resize.plugin adds a fixed with.
		 **/
		_updateBodySize : function() {
			var width = this.get("boundingBox").get("offsetWidth")-2;
			this.bodyNode.setStyle("width", width+"px");
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
				if(columnNode&&(columnNode.getStyle("display")=='block')) {
					width += columnNode.get("offsetWidth");
				}
			}
			content.setStyle("width", width+"px");
		}
		
	}); 

	
}, 'gallery-2010.03.02-18' ,{requires:['node','event','widget','resourcelist','value-change']});