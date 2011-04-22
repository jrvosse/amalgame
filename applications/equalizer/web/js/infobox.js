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
			value: null
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
			var bd = this.get("srcNode").one('.yui3-accordion-item-bd');
			this.content = bd.appendChild(Node.create(
				'<div class="content">'+this.get("content")+'</div>'
			));
			this.loading = bd.appendChild(Node.create(
				'<div class="loading hidden"></div>'
			));
			this.bd = bd;
			this.after('waitingChange', this.toggleLoading, this);
			this.after('selectedChange', this.load, this);
		},
		
		load : function() {
			var instance = this,
				selected = this.get("selected"),
				datasource = this.get("datasource"),
				bd = this.bd,
				content = this.content;

			if(selected) {
				this.set("waiting", true);
				datasource.sendRequest({
					request:'?url='+selected,
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
		
		toggleLoading : function () {
			if(this.get("waiting")) {
				this.content.addClass("hidden");
				this.loading.removeClass("hidden");
			} else {
				this.loading.addClass("hidden");
				this.content.removeClass("hidden");
			}
		}
		
	});
		
	Y.InfoBox = InfoBox;
	
}, '0.0.1', { requires: ['node','event']});
