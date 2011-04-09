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
		data:{
			value:null,
			validator:function(val) {
				return Lang.isObject(val);
			}
		},
		waiting: {
			value:false,
			validator:function(val) {
				return Lang.isBoolean(val);
			}
		},
		content: {
			value:''
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
			this.after('dataChange', this.updateContent, this);
		},
		
		updateContent : function() {
			var data = this.get("data"),
				content = this.content;	
			if(data) {
				content.setContent(this.formatInfoBox(data));
			} else {
				content.setContent(this.get("content"));
			}
			// make sure the content will fit
			if(this.bd.get("clientHeight") > 0) {
				this.bd.setStyle("height", "inherit");
			}
			this.set("waiting", false);
		},
		
		formatInfoBox : function (stats) {
			var html = "<table><tbody>";
			for(var stat in stats) {
				html += "<tr><td>"+stat+"</td><td>"+stats[stat]+"</td></tr>";
			}
			html += "</tbody></table>";
			return html;
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
