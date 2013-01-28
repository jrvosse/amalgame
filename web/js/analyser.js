YUI.add('analyser', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Plugin = Y.Plugin;

	var	NODE_MAPPINGS			= Y.one("#mappings"),
		NODE_ALPHA                      = Y.one('#alpha'),
		NODE_RATERS		        = Y.one('#raters'),
                NODE_AGREEMENT                  = Y.one("#agreement"),
	        NODE_SUBJECTS                  =  Y.one("#subjects");

	function Analyser(config) {
		Analyser.superclass.constructor.apply(this, arguments);
	}
	Analyser.NAME = "analyser";
	Analyser.ATTRS = {
		strategy : {
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

	function display_agreement(a) {
             Y.log(a);

	     NODE_ALPHA.setContent(a.value);
	     NODE_RATERS.setContent(a.raters);
	     NODE_SUBJECTS.setContent(a.subjects);
	     var anc = NODE_AGREEMENT;
	     Y.log(anc);
             anc.get
	     var etable = anc.getById('encoding');
             if (etable) {etable.remove() };
	     etable = anc.create('<table id="encoding">');

	     etable.append("<tr><th colspan='2'>Encoding table</th></tr>");
             for (var key in a.encoding) {
		 var e = a.encoding[key];
		 etable.append('<tr><td>'+e+'</td><td>= '+key+'</td></tr>');
	     }

             var ctable = anc.getById("cm");
             if (ctable) { ctable.remove() };
             ctable = anc.create('<table id="cm" style="text-align: right;">');
             var heading = ctable.append('<tr>'); heading.append('<td>c/k</td>');
             for(var c=0; c<a.dim_cm; c++) {
                   heading.append('<th style="border-bottom: 1px solid #040404;">'+c+'</th>')
	     };
	     for(var r=0; r<a.dim_cm; r++) {
		  var row = ctable.append('<tr>');
                  row.append('<td style="font-weight: bold; border-right: 1px solid #040404;">'+r+'</td>');
		  for (var c=0; c<a.dim_cm; c++) {
                      var v = a.concordance_matrix[r][c];
		      var diag = c==r?'diag':'no_diag';
                      row.append('<td class="'+diag+'" style="padding-left: .5em;">'+v+'</td>');
		  };
	     };
	     anc.appendChild(ctable);
	     anc.appendChild(etable);

	}




	Y.extend(Analyser, Y.Base, {

		initializer: function(args) {
			this.mappinglist = new Y.MappingList({
				mappings:this.get("mappings")
			}).render(NODE_MAPPINGS);
			this.mappinglist.on("mappingSelect", this._onMappingSelect, this);
		},

		_onMappingSelect : function(e) {
		       var uri = e.uri;
                       var server = this.get("paths").agreement;
		       var strategy = this.get("strategy");
                       var data = { strategy: strategy, mapping: uri};
		       this.set("selected", uri);
		       this.mappinglist.set("mapping", uri);

                       Y.io(server, {
				data:data,
				on:{success:function(e,o) {
					var a = Y.JSON.parse(o.responseText);
					display_agreement(a);
				}}
			});


	         }



	});

	Y.Analyser = Analyser;

}, '0.0.1', { requires: [
			 'node','event','anim','io-base',
			 'datasource-io','datasource-jsonschema',
			 'mappinglist'
	]
});






