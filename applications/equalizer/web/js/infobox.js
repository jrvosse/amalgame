YUI.add('infobox', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node;
	
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
		content: {
			value: ""
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
			var bd = this.get("srcNode");
			var selected = this.get("selected"),
				label = selected ? selected.label : "",
				type = selected ? selected.type : "input";
			
			this.typeNode = bd.appendChild(Node.create(
				'<div class="hd">'+type+'</div>'
			));

			var labelBox = bd.appendChild(Node.create(
				'<div class="labelbox hidden"></div>'
			));
			labelBox.appendChild("<label>label: <label>");
			this.labelNode = labelBox.appendChild(Node.create(
				'<input type="text" name="label" class="label" size="40" value="'+label+'">'
			));
			var update = labelBox.appendChild(Node.create(
				'<input type="button" value="change">'
			));
			update.on("click", this._updateLabel, this);
			
			this.contentNode = bd.appendChild(Node.create(
				'<div class="bd">'+this.get("content")+'</div>'
			));
			this.loading = bd.appendChild(Node.create(
				'<div class="loading hidden"></div>'
			));
			this.labelBox = labelBox;
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
				bd = this.get("srcNode"),
				content = this.contentNode;

			if(selected) {
				this.labelBox.removeClass("hidden");
				this.labelNode.set("value", label);
				this.typeNode.setContent(type);
			} else {
				this.labelBox.addClass("hidden");
			}
			
			if(uri) {
				this.set("waiting", true);
				datasource.sendRequest({
					request:'?url='+uri,
					callback:{success:function(o) {
						var HTML = o.response.results[0].responseText;
						content.setContent(HTML);
						// make sure the content will fit
						if(bd.get("clientHeight") > 0) {
							bd.setStyle("height", "inherit");
						}
						instance.set("waiting", false);
					}}
				})
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
		
		toggleLoading : function () {
			if(this.get("waiting")) {
				this.contentNode.addClass("hidden");
				this.loading.removeClass("hidden");
			} else {
				this.loading.addClass("hidden");
				this.contentNode.removeClass("hidden");
			}
		}
		
		
		
	});
		
	Y.InfoBox = InfoBox;
	
}, '0.0.1', { requires: ['node','event']});
