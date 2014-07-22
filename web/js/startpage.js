YUI.add('startpage', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_CONTENT 		= Y.one("#content");
	
	function AmalgameStartPage(config) {
		AmalgameStartPage.superclass.constructor.apply(this, arguments);
	}
	AmalgameStartPage.NAME = "startpage";
	AmalgameStartPage.ATTRS = {
	    strings: {
	        value: {},
			validator: function(val) {
				return Lang.isObject(val)
			}
	    }
	};
	
	Y.extend(AmalgameStartPage, Y.Base, {
		
		initializer: function(args) {
			var paths = this.get("paths");
			
			// The main node is an accordion
			Y.one(NODE_CONTENT).plug(Y.Plugin.NodeAccordion, {
				multiple:false,
				fade:true,
				anim:true,
				effect:Y.Easing.backIn
			});
			// the start button for the vocabulary selector is only shown when
			// a source and a target are selected
			Y.all("#new .option").on("click", function(e) {
	  			e.target.toggleClass("selected");
	  			var nodes = Y.Node.all("#new .selected");
				if(nodes.size()>0) {
	     			Y.all("#new .start").set("disabled", false);
          		} else {
	     			Y.all("#new .start").set("disabled", true);
	  			}
     		});

			
			// the start button for the open alignment selector is only shown when
			// an alignment is selected
			Y.all("#open .option").on("click", function(e) {
	  			e.target.toggleClass("selected");
	  			var nodes = Y.Node.all("#open .selected");
				if(nodes.size()>0) {
	     			Y.all("#open .start").set("disabled", false);
          		} else {
	     			Y.all("#open .start").set("disabled", true);
	  			}
     		});
			// the start button for the publish alignment selector is only shown when
			// an alignment is selected
			Y.all("#publish .option").on("click", function(e) {
	  			e.target.toggleClass("selected");
	  			var nodes = Y.Node.all("#publish .selected");
				if(nodes.size()>0) {
	     			Y.all("#publish .start").set("disabled", false);
          		} else {
	     			Y.all("#publish .start").set("disabled", true);
	  			}
     		});
		},
		
		formatItem : function(o) {
			var label = o["label"],
		 		uri   = o["id"],
		 		value = (label&&!Y.Lang.isObject(label)) ? label : uri;
    	
			var HTML = "";
		 	if(o.hasNext) {
				HTML += "<div class='more'>&gt;</div>";
			}
		 	HTML += "<div class='resourcelist-item-value' title='"+uri+"'>"+value+"</div>";
		 	return HTML;
		}
				
	});
	
	Y.AmalgameStartPage = AmalgameStartPage;
	
}, '0.0.1', { requires: [
	'node','base','event','anim',
	'gallery-node-accordion'
	]
});
