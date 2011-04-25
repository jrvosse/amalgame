YUI.add('analyser', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;
	
	var	NODE_MAPPINGS 			= Y.one("#mappings");
	
	function Analyser(config) {
		Analyser.superclass.constructor.apply(this, arguments);
	}
	Analyser.NAME = "analyser";
	Analyser.ATTRS = {
		alignment : {
			value: null
		},
		selected : {
			value: null
		},
		paths:{
			value:{
			},
			validator: function(val) {
				return Lang.isObject(val)
			}
		},
		mappings:{
			value:{},
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
	
	Y.extend(Analyser, Y.Base, {
		
		initializer: function(args) {
			this.mappinglist = new Y.MappingList({
				mappings:this.get("mappings")
			}).render(NODE_MAPPINGS);
		}
				
	});
	
	Y.Analyser = Analyser;
	
}, '0.0.1', { requires: [
	'node','event','anim',
	'mappinglist'
	]
});
