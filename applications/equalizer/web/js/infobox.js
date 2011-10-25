YUI.add('infobox', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node;

	var NODE_PROPS = Y.one("#properties"),
		NODE_DELETE = Y.one("#delete"),
		NODE_HINT = Y.one("#hint"),
		NODE_UPDATE = Y.one("#update"),
		NODE_TYPE = Y.one("#type"),
		NODE_URI = Y.one("#uri"),
		NODE_NAMESPACE = Y.one("#namespace"),
		NODE_NAMESPACE_ROW = Y.one("#publish_ns"),
		NODE_LABEL = Y.one("#label"),
		NODE_COMMENT = Y.one("#comment"),
		NODE_STATUS_ROW = Y.one("#statusrow");
		NODE_STATUS = Y.one("#status");

	function InfoBox(config) {
		InfoBox.superclass.constructor.apply(this, arguments);
	}
	InfoBox.NAME = "infobox";
	InfoBox.ATTRS = {
		srcNode: {
			value: null
		},
		controls : { value: null },
		readonly : { value: true },
		waiting: {
			value:false,
			validator:function(val) {
				return Lang.isBoolean(val);
			}
		},
		datasource: {
			value: null
		},
		alignment: {
			value: null
		},
		selected: {
			value: null
		},
		hint : {
		       value: null
		       },
		mappings : {
			value: null
		}
	};

	Y.extend(InfoBox, Y.Base, {

		initializer : function(config) {
			var content = this.get("srcNode");
			this.bd = content.one('.bd');
			this.loadingNode = content.one('.loading');
			this.emptyNode = content.one('.empty');
			this.bd.addClass("hidden");

			NODE_DELETE.on("click", this._deleteNode, this);
			NODE_UPDATE.on("click", this._updateNode, this);
			this.after('waitingChange', this.toggleLoading, this);
			this.after('selectedChange', this.syncUI, this);
		},

		formatMappingList : function(selected) {
			var HTML = "";
			var nodes = this.get("mappings");
			for (var uri in nodes) {
				var m = nodes[uri];
				if(m.type == "mapping") {
					var index = selected.indexOf(uri);
					var checked = (index == -1)?'':'checked';
					HTML += '<div><input type="checkbox" name="secondary_input" value="'
					+uri+'" ' +checked +' class="' + checked +'">'
					+'<span>'+m.label+'</span></div>';
				}
			}

			return HTML;
		},

		syncUI : function() {
			var instance = this,
				selected = this.get("selected"),
				datasource = this.get("datasource"),
				alignment = this.get("alignment"),
				content = this.get("srcNode");

			if(selected) {
				var uri = selected.uri,
					link = selected.link||uri,
					label = selected.label||uri,
					type = selected.type||"",
					comment = selected.comment||"",
					namespace = selected.namespace||"",
					status = selected.status;
				        sec_inputs = selected.secondary_inputs|| [];


				this.emptyNode.addClass("hidden");
				this.set("waiting", true);
				NODE_LABEL.set("value", label);
				NODE_COMMENT.set("value", comment);
				Y.one('#namespace').set("value", namespace);
				NODE_TYPE.setContent(type);
				NODE_URI.setContent('<a href="'+link+'">'+uri+'</a>');

				// the status row is only shown for mappings
				if(type=="mapping") {
					NODE_STATUS_ROW.removeClass("hidden")
					Node.getDOMNode(NODE_STATUS).selectedIndex =
							  NODE_STATUS.get('options')
							    .indexOf(NODE_STATUS.one("option[value='"+status+"']"));
				} else {
					NODE_STATUS_ROW.addClass("hidden")
				}

				if(type =='alignment' || type=='strategy') {
				        NODE_NAMESPACE_ROW.removeClass("hidden");
					NODE_DELETE.setAttribute("disabled", true);
				} else {
				        NODE_NAMESPACE_ROW.addClass("hidden");
				        NODE_DELETE.removeAttribute("disabled", false);
				}
				// hide the parameter form submit button in case we are not a process
				if(type==="process") {
				        content.one('.control-submit').removeClass("hidden");
				} else {
					content.one('.control-submit').addClass("hidden");
				}
				var conf = { 'url':uri, 'alignment':alignment };
				datasource.sendRequest({
					cfg: { scope: this },
					request:'?' + Y.QueryString.stringify(conf),
					callback:{success:function(o,cfg) {
						var infobox = o.cfg.scope;
						var HTML = o.response.results[0].responseText;
						NODE_PROPS.setContent(HTML);
						var paramnode = content.one('.parameters');
						if (paramnode && sec_inputs.length > 0) {
						  Y.log(sec_inputs.length);
						  paramnode.prepend(infobox.formatMappingList(sec_inputs))
						  paramnode.prepend('<div>Additional input mappings:</div>');
						}
						instance.set("waiting", false);
					}}
				});
				if (!this.get("readonly")) this._createHint();
			} else {
				this.emptyNode.removeClass("hidden");
			}
		},

		_createHint : function () {
				var oSelf = this;
				var data =
				     { strategy: this.get("alignment"),
				       focus: this.get("selected").uri
				     };
				Y.io(this.get("hint"),
				     { data: data,
				       on: {success: function(e,o) {
						       var r = Y.JSON.parse(o.responseText);
						       if (r.text) {
							 NODE_HINT.setContent(r.text);
						       } else {
							 NODE_HINT.setContent('No hints available at this point');
						       }

						       if (r.data) {
							 NODE_HINT.appendChild('&nbsp;');
							 NODE_HINT.appendChild('(<a id="exec_hint">just do it</a>)');
							 Y.one('#exec_hint').on("click", oSelf._onExecHint, oSelf, r.data);
						       }
						     }
					   }
				     });
		},

		_onExecHint : function(e, data) {
				this.get("controls").fire("submit", {data: data});
			      }
,
		_updateNode : function() {
			var sel = this.get("selected"),
				uri = sel.uri,
				namespace = NODE_NAMESPACE.get("value"),
				label = NODE_LABEL.get("value"),
				comment = NODE_COMMENT.get("value"),
				status = NODE_STATUS.get("options")
					.item(NODE_STATUS.get("selectedIndex")).get("value");

			var data = {
				uri:uri,
				label:label,
				namespace:namespace,
				status:status,
				comment:comment
			}
			this.fire("nodeChange", {update:data});
		},

		_deleteNode : function() {
			var uri = this.get("selected").uri;
			Y.log("delete: "+uri);
			this.fire("deleteNode", {uri:uri});
			this.fire("nodeSelect",  {uri:this.get("alignment")});
			this.syncUI();
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
