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
		query: {
			value: null
		},
		minQueryLength: {
			value: 2,
			validator: Lang.isNumber
		},
		queryDelay: {
			value: 0.3,
			validator: Lang.isNumber
		},
		page: {
			value: 0,
			validator: Lang.isNumber
		},
		maxNumberItems: {
			value: 100,
			validator : Lang.isNumber
		},
		totalNumberOfResults: {
			value: 0,
			validator: Lang.isNumber
		},
		resources: {
			value: [],
			validator : Lang.isArray
		},
		selected: {
			value: [],
			validator : Lang.isArray
		},
		options: {
			value: [],
			validator: Lang.isArray
		},
		loading: {
			value: false,
			validator: Lang.isBoolean
		},
		datasource: {
			value: null
		},
		request: {
			value: ""
		},
		params: {
			value: {}
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
			this._nDelayID = -1;
			this._listItems = [];
			this._list = null;
			this._load = null;
			this._pagination = null;
			
			this.publish("itemClick", {});
			this.publish("beforeContentUpdate", {});
			this.publish("afterContentUpdate", {});
			this.publish("optionSelect", {});
		},

		destructor : function() {
		},

		renderUI : function() {
			this._renderOptions();
			this._renderSearch();
			this._renderLoad();
			this._renderList();
		},

		bindUI : function() {
			Y.delegate("click", this._itemSelect, this._list, "li", this);
		},

		syncUI : function() {
			
			if(this.get("loading")) {
				// replace content with loading message
				this._list.addClass("hidden");
				this._load.removeClass("hidden");
			} 
			else {
				// update the column content
				this.populateList();
				// hide loading message and show content
				this._load.addClass("hidden");
				this._list.removeClass("hidden");
			}		
		},

		_renderList : function() {
			var content = this.get("contentBox"),
				nListLength = this.get("maxNumberItems");
			
			// add ul
			var list = content.one("."+ResourceList.LIST_CLASS);
			if(!list) {
				list = content.appendChild(Node.create(ResourceList.LIST_TEMPLATE));
			}
			// add list items
			var listItems = [];
			for (var i=0; i < nListLength; i++) {
				var listItem = list.appendChild(Node.create(ResourceList.ITEM_TEMPLATE));
				listItem.setStyle("display", "block");
				listItem._node._nItemIndex = i;
				listItems.push({el:listItem});
			}	
			this._list = list;
			this._listItems = listItems;
		},
		
		/**
		* Creates a HTML select list with options provided in the 
		* configuration for columns[index].
		* An eventhandler is added to the HTML select element which is
		* handled by _optionSelect
		*
		* @private
		**/
		_renderOptions : function() {
			var options = this.get("options");
			
			if(options) {
				var optionsNode = this.get("contentBox")
						.appendChild(Node.create('<select class="options"></select>'));

				for (var i=0; i < options.length; i++) {
					var option = options[i],
						value = option.value,
						label = option.label ? option.label : value,	
						selected = option.selected ? 'selected' : '';
					optionsNode.insert('<option value="'+value+'" '+selected+'>'+label+'</option>');
				}
				optionsNode.on("change", this._optionSelect, this);
			}
		},
		
		_renderSearch : function() {
			var category = Y.stamp(this)+"|";
			this.get("contentBox").appendChild(Node.create('<div class="search"></div>'))
				.appendChild(Node.create('<input type="text"></div>'))
				.on(category+"valueChange", this._valueChangeHandler, this);	
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
		* @param length {Integer} the number of resources
		**/
		_renderPagination : function() {
			var length = this.get("totalNumberOfResults"),
				limit = this.get("maxNumberItems"),
				start = this.get("page")*limit,
				end = start+Math.min(limit, length);
				
			if(length>limit) {	
				if(!this._pagination) { // create the pagination HTML
					var pagination = this.get("contentBox")
						.appendChild(Node.create('<div class="pagination"></div>'));
					pagination.appendChild(Node.create('<a href="javascript:{}" class="page-prev">prev</a>'))
						.on("click", this._offsetSelect, this, -1);
					pagination.insert('<span class="page-label"></span>');
					pagination.appendChild(Node.create('<a href="javascript:{}" class="page-next">next</a>'))
						.on("click", this._offsetSelect, this, 1);
					this._pagination = pagination;
				} else { // or show it
					this._pagination.removeClass("hidden");
				}
			
				// now disable the inactive buttons
				if(length<limit) { 
					Y.one(".page-next").addClass("disabled");
					Y.one(".page-prev").removeClass("disabled");
				} else if (start===0) { 
					Y.one(".page-prev").addClass("disabled", true); 
					Y.one(".page-next").removeClass("disabled");
				} else {
					Y.one(".page-next").removeClass("disabled");
					Y.one(".page-prev").removeClass("disabled");
				}
				// and add the right labels
				Y.one(".page-label").set("innerHTML", start+' -- '+end); 
			} 
			else if(this._pagination) {
				this._pagination.addClass("hidden");
			}
		},
		
		_renderLoad : function() {
			this._load = this.get("contentBox").appendChild(
				Y.Node.create('<div class="hidden loading"></div>'));
		},
		
		populateList : function() {
			var listItems = this._listItems,
				resources = this.get("resources"),
				numberItems = Math.min(this.get("maxNumberItems"), resources.length);

			this.clearSelection();
			// add resources
			var i = 0;	
			for (i; i < numberItems; i++) {
				var oResource = resources[i],
					HTML = this.formatItem(oResource),
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
			this.setSelection();
			this._renderPagination();
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
		
		_itemSelect : function(e) {
			var listItems = this._listItems,
				itemNode = e.currentTarget,
				oResource = listItems[itemNode.get("_nItemIndex")].resource;
			
			// @tbd add support for multiple selections
			this.clearSelection();
			this.set("selected", [itemNode]);
			this.setSelection();
			
			itemNode.addClass("selected");
			this.fire("itemClick", itemNode, oResource);
		},
		
		/**
		* Handles the selection of a column option.
		* Fires the optionSelect event
		* 
		* @private
		* @param e {Object} the event object
		**/
		_optionSelect : function(e) {
			var optionValue = e.currentTarget.get("value");
			this.set("page", 0);
			this.set("params.type", optionValue);
			this.fire("optionSelect", optionValue);
			this.updateContent();
		},
		
		/**
		* Handles the selection of a pagination action
		* Fires the offsetSelect event
		* 
		* @private
		* @param e {Object} the event object
		* @param direction {1 or -1} indicator for next (1) or prev (-1)
		**/		
		_offsetSelect : function(e, direction) {
			this.set("page", this.get("page")+direction);
			this.fire("offsetSelect", direction);
			this.updateContent();
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
				query = e.value;
				
			// Clear previous timeout to prevent old searches to push through
		    if(oSelf._nDelayID != -1) {
		        clearTimeout(oSelf._nDelayID);
		    }
			
			this.set("page", 0);
			if (!query || query.length < this.get("minQueryLength")) {
				this.set("query", "");
				this.updateContent();
			}
			else {
	    		// Set a timeout to prevent too many search requests
				var oSelf = this;
	    		oSelf._nDelayID = setTimeout(function(){
					oSelf.set("query", query);
	            	oSelf.updateContent();
	        	}, this.get("queryDelay")*1000);
			}
		},
		
		/**
		* Fetches data by doing a
		* request on the datasource.
		*
		* @private
		**/
		updateContent : function() {
			this.fire("beforeContentUpdate");
			var request = this.get("request"),
				params = this.get("params");

			this._nDelayID = -1; // reset search query delay
			this.set("loading", true);
			this.syncUI();
				
			// the request configuration attribute consist of params in
			// the column definition and the current status of the column 	
			params.limit = this.get("maxNumberItems");
			params.offset = this.get("page")*this.get("maxNumberItems");
			params.query = this.get("query");
			
			request = Lang.isFunction(request) 
				? request.call(this, params) 
				: request+"?"+this._requestParams(params);

			var oSelf = this;
			this.get("datasource").sendRequest({
				request:request,
				callback: {
					success: function(o) {
						var results = o.response.results,
							total = o.response.meta
								? o.response.meta.totalNumberOfResults 
								: results.length;
						oSelf.set("totalNumberOfResults", total)
						oSelf.set("loading", false);
						oSelf.set("resources", results);	
						oSelf.syncUI();
						oSelf.fire("afterContentUpdate");
					},
					failure: function(o) {
						oSelf.set("totalNumberOfResults", 0)
						oSelf.set("loading", false);
						oSelf.set("resources", []);	
						oSelf.syncUI();	
						oSelf.fire("afterContentUpdate");
					}	
				}
			});
		},
		
		_requestParams : function(params) {
			var paramString = "";
			for(var key in params) {
				if(params[key]) {
					paramString += key+"="+encodeURIComponent(params[key])+"&";
				}
			}
			return paramString;
		},
		
		formatItem : function(item) {
			return '<div class="resourcelist-value">'+item.label+'</div>';
		}
		
	});
	  
}, 'gallery-2010.03.02-18' ,{requires:['node','event','widget']});