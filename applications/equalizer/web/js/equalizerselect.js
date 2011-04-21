YUI.add('equalizer-select', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_CONTENT 		= Y.one("#content"),
		NODE_NAV 			= Y.one("#navigator");
	
	function EqualizerSelect(config) {
		EqualizerSelect.superclass.constructor.apply(this, arguments);
	}
	EqualizerSelect.NAME = "equalizer-select";
	EqualizerSelect.ATTRS = {
		paths:{
			value:{
				conceptschemes:'/',
				concepts:'/'
			},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
	    strings: {
	        value: {},
			validator: function(val) {
				return Lang.isObject(val)
			}
	    }
	};
	
	Y.extend(EqualizerSelect, Y.Base, {
		
		initializer: function(args) {
			var paths = this.get("paths");
			
			// The main node is an accordion
			Y.one(NODE_CONTENT).plug(Y.Plugin.NodeAccordion, {
				multiple:false,
				fade:true,
				anim:true,
				effect:Y.Easing.backIn
			});
			
			// the vocabulary selecter dynamically fetches concepts from the server
			// The source of the datasource is set on request as it can be different
			// per selected item
			var DS = new Y.DataSource.IO({source:""})
			.plug(Y.Plugin.DataSourceJSONSchema, {
				schema: {
					resultListLocator: "results",
		    		resultFields: ["id", "label", "hasNext", "matches", "scheme"],
		    		metaFields: {totalNumberOfResults:"totalNumberOfResults"}
		  		}
			})
			.plug(Y.Plugin.DataSourceCache, {
				cfg:{max:20}
			});

			// For the vocabulary selecter we use the columnbrowser widget
			// We define three types of colums: 
			//      the conceptschemes, the topconcepts in this scheme
			//      and the narrower concepts
			var CB = new Y.mazzle.ColumnBrowser({
				datasource: DS,
		  		title:"navigator",
		  		maxNumberItems: 100,
				columns: [
					{request: paths.conceptschemes,
				     label: "conceptscheme",
				     formatter: this.formatItem
				  	},
		  			{request: paths.concepts,
		      		 label: "concept",
					 params:{
						type:"topconcept",
		       			parent:"voc"
		      		 },
					 options:[
						{value:"inscheme",
			 			 label:"concepts in scheme"
						},
	                	{value:"topconcept",
			 			 selected:true,
			 			 label: "top concepts"
						}]
					},
		    		{request: paths.concepts,
		      		 params: {type:"child"},
		      		 options:[
						{value:"descendant",
			 			 label:"descendants"
						},
						{value:"child",
			 			 selected:true,
			 			 label:"children"
						}],
		      		 repeat: true
		    		}
		  		]
			});
			CB.setTitle = function() {}
	      	CB.setFooter = function() {}
			CB.render(NODE_NAV);
	      	Y.on("click", this._valueSet, "#sourcebtn", this, "source");
	      	Y.on("click", this._valueSet, "#targetbtn", this, "target");
	
	
			// the start button for the mapping selector is only shown when
			// a mapping is selected
			Y.all("#mapping .option").on("click", function(e) {
	  			e.target.toggleClass("selected");
	  			var nodes = Y.Node.all("#mapping .selected");
				if(nodes.size()>0) {
	     			Y.one("#mapping .start").set("disabled", false);
          		} else {
	     			Y.one("#mapping .start").set("disabled", true);
	  			}
     		});
	
			// The controls are accordion nodes
 			Y.one(NODE_CONTENT).plug(Y.Plugin.NodeAccordion, { 
   				anim: true, 
				speed:0.1
			});
			
			this.DS = DS;
			this.CB = CB;
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
		},
		
		_valueSet : function(e, which) {
			var selected =  this.CB.get("selected");
			if(selected) {
				Y.log(which+": "+selected.id);
				var uri = selected.id,
				    label = selected.label,
				    labelNode = Y.one("#"+which+"label"),
				    uriNode = Y.one("#"+which);
				
				labelNode.set("value", label);
				labelNode.addClass("filled");
				uriNode.set("value", uri);
				var nodes = Y.Node.all("#new .label.filled");
				if(nodes.size()==2) {
	    			Y.one("#new .start").set("disabled", false);
				} else {
    				Y.one("#new .start").set("disabled", true);
       			}
     		}
		}		
	});
	
	Y.EqualizerSelect = EqualizerSelect;
	
}, '0.0.1', { requires: [
	'node','base','event','anim',
	'datasource-io','datasource-jsonschema','datasource-cache',
	'gallery-resize','gallery-node-accordion'
	]
});
