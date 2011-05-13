YUI.add('infobox', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node;
	
	var NODE_PROPS = Y.one("#properties"),
		NODE_DELETE = Y.one("#delete"),
		NODE_UPDATE = Y.one("#update"),
		NODE_TYPE = Y.one("#type"),
		NODE_URI = Y.one("#uri"),
		NODE_LABEL = Y.one("#label"),
		NODE_STATUS = Y.one("#status");
	
	function InfoBox(config) {
		InfoBox.superclass.constructor.apply(this, arguments);
	}
	InfoBox.NAME = "infobox";
	InfoBox.ATTRS = {
		srcNode: {
			value: null
		},
		waiting: {
			value:false,
			validator:function(val) {
				return Lang.isBoolean(val);
			}
		},
		datasource: {
			value: null
		},
		selected: {
			value: null
		}
	};
	
	Y.extend(InfoBox, Y.Base, {
		
		initializer : function(config) {
			var content = this.get("srcNode"),
				selected = this.get("selected"),
				uri = selected ? selected.uri : "",
				label = selected ? selected.label : "",
				type = selected ? selected.type : "input";
			
			this.bd = content.one('.bd');
			this.loadingNode = content.one('.loading');
			this.emptyNode = content.one('.empty');
			this.bd.addClass("hidden");
						
			NODE_DELETE.on("click", this._deleteNode, this);
			NODE_UPDATE.on("click", this._updateNode, this);
			this.after('waitingChange', this.toggleLoading, this);
			this.after('selectedChange', this.syncUI, this);
		},
		
		syncUI : function() {
			var instance = this,
				selected = this.get("selected"),
				datasource = this.get("datasource"),
				content = this.get("srcNode");
				
			if(selected) {
				var uri = selected.uri,
					link = selected.link,
					label = selected.label,
					type = selected.type,
					status = selected.status;
				
				this.emptyNode.addClass("hidden");
				this.set("waiting", true);
				NODE_LABEL.set("value", label);
				NODE_TYPE.setContent(type);
				NODE_URI.setContent('<a href="'+link+'">'+uri+'</a>');
				
				// there is a bug in set('selectedIndex', n)
				// so we set index of the HTML node
				Node.getDOMNode(NODE_STATUS).selectedIndex = NODE_STATUS.get('options')
					.indexOf(NODE_STATUS.one("option[value="+status+"]"));
								
				datasource.sendRequest({
					request:'?url='+uri,
					callback:{success:function(o) {
						var HTML = o.response.results[0].responseText;
						NODE_PROPS.setContent(HTML);
						instance.set("waiting", false);
					}}
				})
			} else {
				this.emptyNode.removeClass("hidden");
			}
		},
		
		_updateNode : function() {
			var sel = this.get("selected"),
				uri = sel.uri,
				oldLabel = sel.label,
				newLabel = NODE_LABEL.get("value"),
				oldStatus = sel.status,
				newStatus = NODE_STATUS.get("options")
					.item(NODE_STATUS.get("selectedIndex")).get("value");
				
			var data = {}	
			if(newLabel!==oldLabel) {
				data.label = newLabel;
				Y.log('update label for '+uri+' from '+oldLabel+' to '+newLabel);	
			}
			if(newStatus!==oldStatus) {
				data.status = newStatus;
				Y.log('change status for '+uri+' from '+oldStatus+' to '+newStatus);
			}
			if(data) {
				data.uri = uri;
				this.fire("nodeChange", {update:data});
			}			
		},
		
		_deleteNode : function() {
			var uri = this.get("selected").uri;
			Y.log("delete: "+uri);
			this.fire("deleteNode", {uri:uri});
		},
		
		toggleLoading : function () {
			if(this.get("waiting")) {
				this.bd.addClass("hidden");
				this.loadingNode.removeClass("hidden");
			} else {
				this.loadingNode.addClass("hidden");
				this.bd.removeClass("hidden");
			}
		}
		
		
		
	});
		
	Y.InfoBox = InfoBox;
	
}, '0.0.1', { requires: ['node','event']});
