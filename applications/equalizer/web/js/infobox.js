YUI.add('infobox', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node;
	
	var NODE_INFO = Y.one("#nodeInfo"),
		NODE_PROPS = Y.one("#properties"),
		NODE_DELETE = Y.one("#delete"),
		NODE_CHANGE = Y.one("#updateLabel"),
		NODE_TYPE = Y.one("#type"),
		NODE_URI = Y.one("#uri"),
		NODE_LABEL = Y.one("#label");
	
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
			var content = this.get("srcNode");
			var selected = this.get("selected"),
				uri = selected ? selected.uri : "",
				label = selected ? selected.label : "",
				type = selected ? selected.type : "input";
			
			this.bd = content.one('.bd');
			this.bd.addClass("hidden");
			
			this.emptyNode = content.appendChild(Node.create(
				'<div class="empty">select a node</div>'
			));
			this.loadingNode = content.appendChild(Node.create(
				'<div class="loading hidden"></div>'
			));
			
			NODE_DELETE.on("click", this._deleteNode, this);
			NODE_CHANGE.on("click", this._updateLabel, this);
			this.after('waitingChange', this.toggleLoading, this);
			this.after('selectedChange', this._update, this);
		},
		
		_update : function() {
			var instance = this,
				selected = this.get("selected"),
				uri = selected.uri,
				label = selected ? selected.label : "",
				type = selected ? selected.type : "",
				datasource = this.get("datasource"),
				bd = this.get("srcNode");
				
			if(selected) {
				this.emptyNode.addClass("hidden");
				this.set("waiting", true);
				NODE_LABEL.set("value", label);
				NODE_TYPE.setContent(type);
				NODE_URI.setContent(uri);
				
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
		
		_updateLabel : function() {
			var oldLabel = this.get("selected").label,
				uri = this.get("selected").uri,
				newLabel = this.labelNode.get("value");
			if(newLabel!==oldLabel) {
				this.fire("labelChange", {uri:uri, oldVal:oldLabel, newVal:newLabel});
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
